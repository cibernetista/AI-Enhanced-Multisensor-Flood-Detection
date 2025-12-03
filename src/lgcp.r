# ==============================================================================
# 0. LIBRARÍAS
# ==============================================================================
library(sp)
library(terra)
library(sf)
library(raster)
library(INLA)
library(ggplot2)

# ==============================================================================
# 1. FUNCIONES AUXILIARES
# ==============================================================================

# Leer fechas a partir de los nombres de archivos .shp
extract_dates <- function(path) 
{
  files <- list.files(path, pattern = "\\.shp$|\\.SHP$", full.names = FALSE)
  pattern <- "(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dic)[0-9]{4}"
  dates <- regmatches(files, gregexpr(pattern, files, ignore.case = TRUE))
  return(tolower(unique(unlist(dates))))
}

# Alinear un ráster a un ráster maestro (crs + resolución + extensión)
align_raster <- function(raster_input, raster_master) 
{
  r_in <- rast(raster_input)
  if (!compareGeom(r_in, raster_master, stopOnError = FALSE))
  {
    if (crs(r_in) != crs(raster_master)) 
    {
      r_in <- project(r_in, raster_master)
    }
    r_in <- resample(r_in, raster_master, method = "bilinear")
  }
  return(r_in)
}

# Normalización min-max a [0, 1]
normalize_raster_terra <- function(r_input)
{
  mm <- minmax(r_input) 
  min_val <- mm[1, ]
  max_val <- mm[2, ]
  
  if (max_val == min_val)
  {
    return(r_input * 0)
  }
  
  r_norm <- (r_input - min_val) / (max_val - min_val)
  return(r_norm)
}

# Imputación de NAs por media (o 0.5 si todo es NA)
impute_na <- function(df, var_name) {
  col_vals <- df[[var_name]]
  if (any(is.na(col_vals))) {
    val_sustitucion <- mean(col_vals, na.rm = TRUE)
    if (is.nan(val_sustitucion)) val_sustitucion <- 0.5
    col_vals[is.na(col_vals)] <- val_sustitucion
  }
  return(col_vals)
}

# ==============================================================================
# 2. PARÁMETROS Y RUTA
# ==============================================================================

AOI  <- 1
path <- paste0("data/trainingdata/AOI", AOI, "/")

dates <- extract_dates(path)

# Ráster maestro de referencia (Sentinel-1 VH de la primera fecha)
master_ref <- rast(paste0(path, "s1vh-", dates[1], ".tif"))

# ==============================================================================
# 3. RÁSTER ESTÁTICOS (DERIVADOS DEL DEM)
# ==============================================================================

dem   <- align_raster(paste0(path, "dem.tif"),        master_ref)[[1]]
slope <- align_raster(paste0(path, "slope.tif"),      master_ref)
hand  <- align_raster(paste0(path, "hand.tif"),       master_ref)
curv  <- align_raster(paste0(path, "curvature.tif"),  master_ref)

# ==============================================================================
# 4. RÁSTER DINÁMICOS (ASOCIADOS A FECHA) + PUNTOS
# ==============================================================================

all_points_sf      <- list()
dynamic_stack_list <- list()

cat("Processing temporal fusion...\n")

for (date in dates) {
  # Puntos (patrón de puntos)
  pp_temp <- st_read(paste0(path, "pp-", date, ".shp"), quiet = TRUE)
  pp_temp <- st_transform(pp_temp, crs(master_ref))
  all_points_sf[[date]] <- pp_temp
  
  # Ráster satelitales
  s1_vh_t <- align_raster(paste0(path, "s1vh-",  date, ".tif"), master_ref)
  s1_vv_t <- align_raster(paste0(path, "s1vv-",  date, ".tif"), master_ref)
  ndvi_t  <- align_raster(paste0(path, "ndvi-",  date, ".tif"), master_ref)
  mndwi_t <- align_raster(paste0(path, "mndwi-", date, ".tif"), master_ref)
  awei_t  <- align_raster(paste0(path, "awei-",  date, ".tif"), master_ref)
  
  r_stack <- c(s1_vh_t, s1_vv_t, ndvi_t, mndwi_t, awei_t)
  names(r_stack) <- c("vh", "vv", "ndvi", "mndwi", "awei")
  dynamic_stack_list[[date]] <- r_stack
}

# Unir todos los puntos de todas las fechas
merged_sf <- do.call(rbind, all_points_sf)

# ==============================================================================
# 5. PROMEDIO TEMPORAL Y STACK FINAL DE COVARIABLES
# ==============================================================================

big_stack <- rast(dynamic_stack_list)
num_vars  <- 5
num_dates <- nlyr(big_stack) / num_vars
indices   <- rep(1:num_vars, times = num_dates)

dynamic_mean <- tapp(big_stack, index = indices, fun = mean, na.rm = TRUE)

nombres_vars <- c("vh", "vv", "ndvi", "mndwi", "awei")
names(dynamic_mean) <- nombres_vars

raw_covariates <- c(dem, slope, hand, curv, dynamic_mean)
names(raw_covariates) <- c("dem", "slope", "hand", "curv", nombres_vars)

cat("Normalizing rasters between [0, 1]...\n")

final_covariates_terra <- raw_covariates
for (i in 1:nlyr(raw_covariates)) {
  final_covariates_terra[[i]] <- normalize_raster_terra(raw_covariates[[i]])
}

# Verificación de rangos
print(minmax(final_covariates_terra))

# ==============================================================================
# 6. CONFIGURACIÓN INLA LGCP
# ==============================================================================

cat("\nIniciando configuración INLA LGCP...\n")

# 6.1. Boundary y malla
# ------------------------------------------------------------------------------

# Boundary a partir de la extensión de las covariables
boundary_sf <- st_as_sf(
  as.polygons(ext(final_covariates_terra), crs = crs(final_covariates_terra))
)

# Coordenadas de los puntos observados
locs_obs <- st_coordinates(merged_sf)

# Tamaño aproximado de la malla
mesh_size <- 100
diag_dist <- sqrt(diff(ext(final_covariates_terra)[1:2])^2 + 
                    diff(ext(final_covariates_terra)[3:4])^2)
max_edge  <- diag_dist / mesh_size

# Malla (sin boundary explícito; se corrige luego con las áreas)
mesh <- inla.mesh.2d(
  loc     = locs_obs,
  max.edge = c(max_edge, max_edge * 2),
  cutoff   = max_edge / 5
)

# 6.2. Modelo SPDE (Matern)
# ------------------------------------------------------------------------------
spde <- inla.spde2.matern(mesh = mesh, alpha = 2)

# 6.3. Áreas locales (FEM) y filtrado por boundary
# ------------------------------------------------------------------------------

# Áreas asociadas a cada vértice de la malla
local_areas <- diag(inla.mesh.fem(mesh, order = 1)$c0)

# Coordenadas de los vértices de la malla como sf
mesh_locs_df <- data.frame(x = mesh$loc[, 1], y = mesh$loc[, 2])
mesh_locs_sf <- st_as_sf(mesh_locs_df, coords = c("x", "y"),
                         crs = crs(final_covariates_terra))

# Verificar qué nodos están dentro del boundary
is_inside <- lengths(st_intersects(mesh_locs_sf, boundary_sf)) > 0

# Asignar área 0 a los nodos fuera del AOI
local_areas[!is_inside] <- 0

# ==============================================================================
# 7. EXTRACCIÓN DE COVARIABLES PARA PUNTOS E INTEGRACIÓN
# ==============================================================================

cat("Extrayendo covariables (ya normalizadas)...\n")

# Extracción para puntos observados y nodos de la malla
covs_obs <- terra::extract(final_covariates_terra, locs_obs, method = "bilinear")
covs_int <- terra::extract(final_covariates_terra, mesh$loc[, 1:2], method = "bilinear")

df_obs <- as.data.frame(covs_obs)
df_int <- as.data.frame(covs_int)

# Eliminar columna ID si existe
if ("ID" %in% names(df_obs)) df_obs$ID <- NULL
if ("ID" %in% names(df_int)) df_int$ID <- NULL

vars_nombres <- names(final_covariates_terra)

# Imputación de NAs
for (var in vars_nombres) {
  df_obs[[var]] <- impute_na(df_obs, var)
  df_int[[var]] <- impute_na(df_int, var)
}

df_obs_sc <- df_obs
df_int_sc <- df_int

cat("Rango de datos observados (head):\n")
print(head(df_obs_sc))

# ==============================================================================
# 8. STACKS DE INLA PARA LGCP
# ==============================================================================

cat("Creando INLA Stacks...\n")

n_obs <- nrow(locs_obs)
n_int <- mesh$n

# Stack de observaciones: y = 1, E = 0
stk_obs <- inla.stack(
  data = list(y = rep(1, n_obs), E = rep(0, n_obs)),
  A    = list(1, inla.spde.make.A(mesh, locs_obs)),
  effects = list(
    c(list(b0 = 1), df_obs_sc),
    list(s = 1:spde$n.spde)
  ),
  tag = 'obs'
)

# Stack de integración: y = 0, E = área local
stk_int <- inla.stack(
  data = list(y = rep(0, n_int), E = local_areas),
  A    = list(1, inla.spde.make.A(mesh, mesh$loc[, 1:2])),
  effects = list(
    c(list(b0 = 1), df_int_sc),
    list(s = 1:spde$n.spde)
  ),
  tag = 'int'
)

join_stack <- inla.stack(stk_obs, stk_int)

cat("Stack creado exitosamente. Continuando con INLA...\n")

# ==============================================================================
# 9. DEFINICIÓN DE LA FÓRMULA Y EJECUCIÓN DE INLA
# ==============================================================================

cov_names <- names(final_covariates_terra)

formula_lgcp <- as.formula(
  paste0(
    "y ~ 0 + b0 + ",
    paste(cov_names, collapse = " + "),
    " + f(s, model = spde)"
  )
)

cat("Ejecutando INLA (Family: Poisson)... esto puede tardar.\n")

inla_data <- inla.stack.data(join_stack)

result <- inla(
  formula_lgcp,
  family = "poisson",
  data   = inla_data,
  E      = inla_data$E,
  control.predictor = list(
    A = inla.stack.A(join_stack),
    compute = TRUE
  ),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

cat("Resumen de Efectos Fijos:\n")
print(result$summary.fixed)

cat("Análisis LGCP completado.\n")

# ==============================================================================
# 10. VISUALIZACIÓN DEL CAMPO ESPACIAL LATENTE
# ==============================================================================

proj <- inla.mesh.projector(mesh, dims = c(200, 200))
mean_field <- inla.mesh.project(proj, result$summary.random$s$mean)

df_plot <- expand.grid(x = proj$x, y = proj$y)
df_plot$value <- as.vector(mean_field)
df_plot <- df_plot[!is.na(df_plot$value), ]

ggplot(df_plot, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", name = "Spatial\nEffect") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Gaussian Latent Field",
       subtitle = "Residual process",
       x = "Lon / X", y = "Lat / Y")

# ==============================================================================
# 11. GENERACIÓN DEL MAPA FINAL DE RIESGO
# ==============================================================================

cat("Calculando mapa final de riesgo...\n")

# --- CORRECCIÓN 11.1 ---
# Primero, miremos qué nombres tiene realmente el modelo
cat("Nombres en summary.fixed:\n")
print(rownames(result$summary.fixed))

# Extraer efectos fijos de forma más robusta
fixed_effects <- result$summary.fixed[, "mean"]

# Intentar identificar el intercepto dinámicamente
# Por defecto INLA usa "(Intercept)", pero si tú lo renombraste a "b0", adáptalo.
if ("(Intercept)" %in% names(fixed_effects)) {
  intercept <- fixed_effects["(Intercept)"]
  betas <- fixed_effects[names(fixed_effects) != "(Intercept)"]
} else if ("b0" %in% names(fixed_effects)) {
  intercept <- fixed_effects["b0"]
  betas <- fixed_effects[names(fixed_effects) != "b0"]
} else {
  # Fallback: asume que el primero es el intercepto
  intercept <- fixed_effects[1]
  betas <- fixed_effects[-1]
}

cat("Intercepto:", intercept, "\n")
cat("Betas encontrados:\n")
print(betas) 
# Si esto sigue imprimiendo numeric(0), tu modelo NO estimó las covariables.

# 11.2. Predictor lineal de covariables
risk_map_log <- final_covariates_terra[[1]] * 0 + intercept

available_vars <- names(final_covariates_terra)

for (var_name in available_vars) {
  if (var_name %in% names(betas)) {
    beta_val <- betas[[var_name]]
    cat(paste("Sumando:", var_name, "*", round(beta_val, 4), "\n"))
    risk_map_log <- risk_map_log + (final_covariates_terra[[var_name]] * beta_val)
  }
}

# --- CORRECCIÓN 11.3 ---
# Asegurar que las coordenadas coinciden exactamente con las celdas del raster
spatial_raster <- rast(final_covariates_terra[[1]])
coords_raster <- crds(spatial_raster, na.rm = FALSE) # Importante: na.rm=FALSE para mantener la grilla completa

# Proyectar
proj_raster <- inla.mesh.projector(mesh, loc = as.matrix(coords_raster))
spatial_field_vals <- inla.mesh.project(proj_raster, result$summary.random$s$mean)

# Verificación de longitud
if(length(spatial_field_vals) != ncell(spatial_raster)){
  stop("ERROR DIMENSIONAL: La proyección no coincide con el número de celdas del raster.")
}

values(spatial_raster) <- spatial_field_vals
# Reemplazar NAs espaciales por 0
spatial_raster[is.na(spatial_raster)] <- 0

# ==============================================================================
# CORRECCIÓN DE ESTABILIDAD NUMÉRICA (Reemplaza desde 11.4 hasta el final)
# ==============================================================================

# 1. Sumar Log-Intensidad
total_log_intensity <- risk_map_log + spatial_raster

# --- DIAGNÓSTICO DE RANGO ---
mm_log <- minmax(total_log_intensity)
cat("Rango de valores en escala Logarítmica (antes de exp):\n")
print(mm_log)

# Si los valores son muy negativos (ej. -700), exp() dará 0.
# Si los valores son muy positivos (ej. +700), exp() dará Inf.

# 2. Truco de Estabilidad (Shift-Exp)
# Restamos el valor máximo del mapa logarítmico antes de aplicar exp.
# Esto hace que el valor máximo sea exp(0) = 1, y el resto sean proporciones relativas.
# Matemáticamente, esto preserva el patrón visual de riesgo relativo perfectamente.

max_log_val <- mm_log[2, 1] 

# Manejo de error si todo es NA o Infinito
if (!is.finite(max_log_val)) {
  stop("El mapa logarítmico contiene valores infinitos o NaNs. Revisa tus covariables.")
}

# Calculamos intensidad relativa
relative_intensity <- exp(total_log_intensity - max_log_val)

# 3. Normalización Min-Max (Ahora sí funcionará)
mm_rel <- minmax(relative_intensity)
min_v  <- mm_rel[1, 1]
max_v  <- mm_rel[2, 1]

cat("Rango de intensidad relativa:\n")
cat("Min:", min_v, " Max:", max_v, "\n")

if ((max_v - min_v) < 1e-9) {
  warning("El mapa sigue siendo plano. Verifica si solo una covariable domina todo el modelo.")
  risk_index <- relative_intensity
} else {
  risk_index <- (relative_intensity - min_v) / (max_v - min_v)
}

names(risk_index) <- "Flood_Susceptibility"

# ==============================================================================
# 12. VISUALIZACIÓN FINAL (EN ESCALA VISIBLE)
# ==============================================================================

# EN LUGAR de usar la intensidad exponencial (que colapsa a 0), 
# usaremos la intensidad logarítmica normalizada para la visualización.
# Esto permite ver gradientes donde antes solo veías ceros.

# 1. Tomamos el mapa logarítmico (total_log_intensity)
# Ya sabemos que va de -1135 a -162
risk_log_metric <- total_log_intensity

# 2. Normalizamos ESTE mapa logarítmico entre 0 y 1
mm_log <- minmax(risk_log_metric)
min_l <- mm_log[1, 1]
max_l <- mm_log[2, 1]

# Normalización Min-Max sobre el logaritmo
risk_visual <- (risk_log_metric - min_l) / (max_l - min_l)
names(risk_visual) <- "Risk_Index_LogScale"

# 3. Convertir a DataFrame
df_risk <- as.data.frame(risk_visual, xy = TRUE, na.rm = TRUE)

cat("Generando gráfico visualizable...\n")

# 4. Plotear
ggplot(df_risk, aes(x = x, y = y, fill = Risk_Index_LogScale)) +
  geom_raster() +
  # Usamos escala turbo para resaltar diferencias
  scale_fill_viridis_c(option = "turbo", 
                       name = "Índice de\nRiesgo",
                       labels = scales::percent) + 
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "Risk map",
    subtitle = "visualization of relative risk gradients",
    x = "Lon/X", y = "Lat/Y"
  )

# Guardar este resultado que SÍ se ve
terra::writeRaster(risk_visual, "Flood_Risk_Visual.tif", overwrite = TRUE)
