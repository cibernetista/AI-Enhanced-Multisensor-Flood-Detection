# ==========================================
# 1. PREPARACIÓN DE LIBRERÍAS Y FUNCIONES
# ==========================================
library(lgcp)     # Requiere instalación legacy (o remotes)
library(spatstat)
library(terra)
library(sf)
library(stats)

# --- Tu función auxiliar de fechas (intacta) ---
extract_dates <- function(path) {
  files <- list.files(path, pattern = "\\.shp$|\\.SHP$", full.names = FALSE)
  pattern <- "(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dic)[0-9]{4}"
  dates <- regmatches(files, gregexpr(pattern, files, ignore.case = TRUE))
  return(tolower(unique(unlist(dates))))
}

# --- Tu función de alineación (intacta) ---
align_raster <- function(raster_input, raster_master) {
  r_in <- rast(raster_input)
  if (!compareGeom(r_in, raster_master, stopOnError = FALSE)) {
    if (crs(r_in) != crs(raster_master)) {
      r_in <- project(r_in, raster_master)
    }
    r_in <- resample(r_in, raster_master, method = "bilinear")
  }
  return(r_in)
}

normalize_raster_terra <- function(r_input) {
  mm <- minmax(r_input) 
  min_val <- mm[1, ]
  max_val <- mm[2, ]
  r_norm <- (r_input - min_val) / (max_val - min_val)
  return(r_norm)
}

getZmat_terra_v3 <- function(formula, data_ppp, cov_rast, cellwidth, ext_factor) {
  
  # 1. Obtener la ventana de observación original del objeto ppp
  W <- Window(data_ppp)
  
  # 2. Calcular la extensión EXACTA como lo hace lgcp internamente
  # lgcp expande el bounding box original sumando 'ext' celdas a cada lado
  x_range_raw <- W$xrange + c(-ext_factor, ext_factor) * cellwidth
  y_range_raw <- W$yrange + c(-ext_factor, ext_factor) * cellwidth
  
  # 3. Definir el número de celdas (M x N)
  # Usamos ceiling para asegurar que cubrimos todo el rango (lgcp usa lógica similar)
  M <- ceiling((x_range_raw[2] - x_range_raw[1]) / cellwidth)
  N <- ceiling((y_range_raw[2] - y_range_raw[1]) / cellwidth)
  
  # 4. Recalcular los límites finales para que encajen perfectamente con M y N
  # Esto evita errores de redondeo de punto flotante
  xmin_exact <- x_range_raw[1]
  xmax_exact <- x_range_raw[1] + M * cellwidth
  ymin_exact <- y_range_raw[1]
  ymax_exact <- y_range_raw[1] + N * cellwidth
  
  # 5. Crear el raster plantilla (Target Raster)
  r_target <- terra::rast(
    xmin = xmin_exact, xmax = xmax_exact,
    ymin = ymin_exact, ymax = ymax_exact,
    nrows = N, ncols = M,  # Forzamos dimensiones exactas
    crs = terra::crs(cov_rast)
  )
  
  # Log para verificar
  cat("--- Debug Info ---\n")
  cat("Grid calculada: ", M, "cols x ", N, "rows\n")
  cat("Extensión X: ", xmin_exact, " - ", xmax_exact, "\n")
  
  # 6. Resamplear (Alinear tus covariables a esta nueva rejilla)
  # Usamos bilinear para variables continuas
  cov_aligned <- terra::resample(cov_rast, r_target, method = "bilinear")
  
  # 7. Convertir a DataFrame y limpiar NAs
  # IMPORTANTE: lgcp es muy sensible al orden. terra::as.data.frame suele ser seguro.
  df <- terra::as.data.frame(cov_aligned, xy = TRUE, na.rm = FALSE)
  
  # Rellenar NAs (los bordes extendidos no tienen datos en tus mapas originales)
  vars <- all.vars(formula)
  vars <- vars[vars != "."] 
  
  for(v in vars) {
    if(v %in% names(df)) {
      # Rellenamos con 0 (asumiendo datos normalizados/centrados)
      df[is.na(df[[v]]), v] <- 0 
    }
  }
  
  # 8. Generar Matriz de Diseño
  Zmat <- stats::model.matrix(object = formula, data = df)
  
  return(Zmat)
}

# ==========================================
# 2. CARGA DE DATOS (MODERNIZADO)
# ==========================================

AOI  <- 1
path <- paste0("data/trainingdata/AOI", AOI, "/") 

dates <- extract_dates(path)
master_ref <- rast(paste0(path, "s1vh-", dates[1], ".tif"))

# Rasters Estáticos
dem   <- align_raster(paste0(path, "dem.tif"),        master_ref)[[1]]
slope <- align_raster(paste0(path, "slope.tif"),      master_ref)[[1]]
hand  <- align_raster(paste0(path, "hand.tif"),       master_ref)[[1]]
curv  <- align_raster(paste0(path, "curvature.tif"),  master_ref)[[1]]

# Rasters Dinámicos y Puntos
all_points_sf       <- list()
dynamic_stack_list  <- list()

cat("Processing temporal fusion...\n")

for (date in dates) {
  pp_temp <- st_read(paste0(path, "pp-", date, ".shp"), quiet = TRUE)
  pp_temp <- st_transform(pp_temp, crs(master_ref))
  all_points_sf[[date]] <- pp_temp
  
  s1_vh_t <- align_raster(paste0(path, "s1vh-",  date, ".tif"), master_ref)[[1]]
  s1_vv_t <- align_raster(paste0(path, "s1vv-",  date, ".tif"), master_ref)[[1]]
  ndvi_t  <- align_raster(paste0(path, "ndvi-",  date, ".tif"), master_ref)[[1]]
  mndwi_t <- align_raster(paste0(path, "mndwi-", date, ".tif"), master_ref)[[1]]
  awei_t  <- align_raster(paste0(path, "awei-",  date, ".tif"), master_ref)[[1]]
  
  r_stack <- c(s1_vh_t, s1_vv_t, ndvi_t, mndwi_t, awei_t)
  names(r_stack) <- c("vh", "vv", "ndvi", "mndwi", "awei")
  dynamic_stack_list[[date]] <- r_stack
}

# Fusión
merged_sf <- do.call(rbind, all_points_sf)
big_stack <- rast(dynamic_stack_list)

# Promedio temporal
# Nota: Esta lógica asume orden perfecto de capas.
num_vars  <- 5
num_dates <- nlyr(big_stack) / num_vars
indices   <- rep(1:num_vars, times = num_dates)
dynamic_mean <- tapp(big_stack, index = indices, fun = mean, na.rm = TRUE)
names(dynamic_mean) <- c("vh", "vv", "ndvi", "mndwi", "awei") 

# ==========================================
# 3. PREPARACIÓN PARA MODELO (SIMPLIFICADO)
# ==========================================

cat("Preparando stack final con Terra...\n")

# A. Stack final en Terra (Sin convertir a sp todavía)
cov_stack <- c(
  normalize_raster_terra(dem),
  normalize_raster_terra(slope),
  normalize_raster_terra(hand),
  normalize_raster_terra(curv),
  normalize_raster_terra(dynamic_mean)
)
names(cov_stack) <- c("dem", "slope", "hand", "curv", "vh", "vv", "ndvi", "mndwi", "awei")

# B. Convertir Puntos SF a ppp (Spatstat)
coords <- st_coordinates(merged_sf)
ex <- ext(master_ref)
win_owin <- owin(xrange = c(ex[1], ex[2]), yrange = c(ex[3], ex[4]))

ppp_data <- ppp(x = coords[,1], 
                y = coords[,2], 
                window = win_owin)

if(any(duplicated(ppp_data))){
  ppp_data <- unique(ppp_data)
}

# ==========================================
# 4. CONFIGURACIÓN DEL MODELO LGCP (SOLUCIÓN FINAL)
# ==========================================
library(raster)
library(sp) # Necesario para la conversión

# Parámetros (Igual que antes)
ancho_total_mapa <- xmax(master_ref) - xmin(master_ref)
CELLWIDTH <- ancho_total_mapa / 128 
EXT <- 2 

# Fórmula
FORM <- ~ dem + slope + hand + curv + vh + vv + ndvi + mndwi + awei

cat("Generando Matriz alineada (Conversión Terra -> Raster -> SP)...\n")

# --- PASO CRÍTICO: DOBLE CONVERSIÓN ---

# 1. Terra -> Raster (Legacy)
cov_stack_legacy <- raster::stack(cov_stack)

# 2. Raster -> SpatialGridDataFrame (Paquete 'sp')
# ESTE es el paso que soluciona el error "no slot of name data".
# Crea un objeto con la estructura exacta que lgcp espera.
cov_sp <- as(cov_stack_legacy, "SpatialGridDataFrame")

# Verificación rápida de nombres (deben coincidir con la fórmula)
print(names(cov_sp@data)) 

# 3. Generar Zmat con lgcp
Zmat <- lgcp::getZmat(
  formula = FORM,
  data = ppp_data,              
  regionalcovariates = cov_sp,  # Usamos el objeto 'sp'
  cellwidth = CELLWIDTH,
  ext = EXT
)

# VALIDACIÓN
cat("✅ Zmat generado exitosamente con dimensiones:", nrow(Zmat), "filas.\n")

# --- DEFINICIÓN DE PRIORS Y MODELO ---

priors <- lgcpPrior(
  etaprior = PriorSpec(LogGaussianPrior(
    mean = log(c(1, 500)), 
    variance = diag(0.15, 2)
  )), 
  betaprior = PriorSpec(GaussianPrior(
    mean = rep(0, ncol(Zmat)), 
    variance = diag(10^6, ncol(Zmat))
  ))
)

cf <- CovFunction(exponentialCovFct)

cat("Ejecutando MCMC...\n")
lg <- lgcpPredictSpatialPlusPars(
  formula = FORM,
  sd = ppp_data, 
  Zmat = Zmat, 
  model.priors = priors,
  spatial.covmodel = cf,
  cellwidth = CELLWIDTH,
  mcmc.control = mcmcpars(
    mala.length = 2000,
    burnin = 200,          
    retain = 5,            
    adaptivescheme = andrieuthomsh(inith = 1, alpha = 0.5, C = 1, targetacceptance = 0.574)
  ),
  output.control = setoutput(gridfunction = dump2dir(dirname = file.path(DIRNAME, "lgcp_output_v3"), forceSave = TRUE)),
  ext = EXT
)

save(lg, file = file.path(DIRNAME, "lgcp_output_v2", "modelo_final.RData"))

# ==========================================
# 6. VISUALIZACIÓN
# ==========================================
plot(lg, type="intensity")