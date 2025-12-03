# ==============================================================================
# Script Name  : lgcp.R
# Project      : Multi-Sensor Bayesian Log-Gaussian Cox Process Modelling for Flood Monitoring
# Author       : Francisco J. Lozada
# Institutional Emails:
#   - al449182@uji.es
#   - francisco.lozada@uni-muenster.de
#
# Description:
#   This script implements a full Log-Gaussian Cox Process (LGCP) workflow for
#   flood susceptibility mapping in a given Area of Interest (AOI). The model
#   combines:
#     - Static DEM-derived covariates (DEM, slope, HAND, curvature)
#     - Multi-temporal satellite covariates (Sentinel-1 VH/VV, NDVI, MNDWI, AWEI)
#     - Observed flood point patterns (one point set per flood event/date)
#
#   The script:
#     1. Reads and harmonizes all raster layers to a common master grid
#     2. Extracts dates from shapefile names and builds a multi-temporal stack
#     3. Computes temporal means of dynamic covariates across events
#     4. Normalizes all covariates to [0, 1] (min–max scaling)
#     5. Builds an INLA-SPDE mesh over the AOI and computes local FEM areas
#     6. Constructs INLA stacks for LGCP (observation + integration points)
#     7. Fits a Poisson LGCP model with a Matérn latent field using INLA
#     8. Projects the latent field and fixed effects onto a raster grid
#     9. Produces:
#          - A flood susceptibility index raster in [0, 1]
#          - A log-scale risk raster for visualization
#
# R Version :
#   - Developed and tested under R 4.5.1
#
# Main Dependencies (R packages):
#   - sp
#   - sf
#   - terra
#   - raster
#   - INLA
#   - ggplot2
#
# Inputs (per AOI):
#   Folder structure:
#     data/trainingdata/AOI{AOI}/
#
#   Static rasters (aligned later to the master grid):
#     - dem.tif           : Digital Elevation Model
#     - slope.tif         : Slope
#     - hand.tif          : Height Above Nearest Drainage
#     - curvature.tif     : Curvature
#
#   Dynamic rasters (one set per date token "mmmYYYY"; e.g., nov2022):
#     - s1vh-{date}.tif   : Sentinel-1 VH backscatter
#     - s1vv-{date}.tif   : Sentinel-1 VV backscatter
#     - ndvi-{date}.tif   : Normalized Difference Vegetation Index
#     - mndwi-{date}.tif  : Modified Normalized Difference Water Index
#     - awei-{date}.tif   : Automated Water Extraction Index
#
#   Point patterns (one shapefile per event/date):
#     - pp-{date}.shp     : Flood occurrence points for that event
#
# Outputs:
#   - Flood_Susceptibility.tif  (or equivalent name):
#       Raster in [0, 1] representing relative flood susceptibility.
#   - Flood_Risk_Visual.tif :
#       Log-scale risk map normalized to [0, 1] for visualization.
#   - INLA result object (in memory):
#       Contains fixed effects, random field summaries, and fit diagnostics
#       (DIC, WAIC, CPO).
#
# Reproducibility Notes:
#   - This script assumes a consistent file naming convention for rasters and
#     shapefiles (pp-{date}.shp, s1vh-{date}.tif, etc.).
#   - All covariates are internally aligned to the first Sentinel-1 VH raster
#     of the AOI (“master_ref”).
#   - The LGCP is fitted per AOI; the code can be wrapped in a function to loop
#     over multiple AOIs (e.g., AOI 0–9) for comparative analysis.
#
# Last Update:
#   - 2025-12-03
#
# License / Usage:
#   - This code is part of an academic research project (MSc thesis)
# ==============================================================================


# ==============================================================================
# 0. Library Setup
# ==============================================================================
library(sp)
library(terra)
library(sf)
library(raster)
library(INLA)
library(ggplot2)

# ==============================================================================
# 1. Auxiliary Functions
# ==============================================================================

# Date extraction from filenames
# This function scan path for .shp files
# Return the unique set of lowercase date strings
extract_dates <- function(path) 
{
  files <- list.files(path, pattern = "\\.shp$|\\.SHP$", full.names = FALSE)
  pattern <- "(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dic)[0-9]{4}"
  dates <- regmatches(files, gregexpr(pattern, files, ignore.case = TRUE))
  return(tolower(unique(unlist(dates))))
}

# If geometry (CRS, resolution, extent) differs:
# If CRS differs, reproject raster_input in to CRS of raster_master
#	Resample raster_input to the grid of raster_master using bilinear interpolation.
#	Return the aligned raster.
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

# Min–max normalization of rasters [0,1]
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

# N/A imputation in data frames
# For a column v in a data frame:
# 	If v contains missing values:
#     Compute v_hat as the mean ignoring NAs.
#	  If v_hat is NaN (all values are NA), set v_hat = 0.5.
#	    Replace NAs in v with v_hat
#	Return the imputed vector.
impute_na <- function(df, var_name)
{
  col_vals <- df[[var_name]]
  if (any(is.na(col_vals)))
  {
    val_sustitucion <- mean(col_vals, na.rm = TRUE)
    if (is.nan(val_sustitucion)) val_sustitucion <- 0.5
    col_vals[is.na(col_vals)] <- val_sustitucion
  }
  return(col_vals)
}

# ==============================================================================
# 2. Paths, Dates, and Master Raster
# ==============================================================================

# Area of interest (0,9)
AOI  <- 1
path <- paste0("data/trainingdata/AOI", AOI, "/")

dates <- extract_dates(path)

# Take one raster as a reference e.g. S1-IW-VH
master_ref <- rast(paste0(path, "s1vh-", dates[1], ".tif"))

# ==============================================================================
# 3. Static Covariates (DEM-Derived)
# ==============================================================================

dem   <- align_raster(paste0(path, "dem.tif"),        master_ref)[[1]]
slope <- align_raster(paste0(path, "slope.tif"),      master_ref)
hand  <- align_raster(paste0(path, "hand.tif"),       master_ref)
curv  <- align_raster(paste0(path, "curvature.tif"),  master_ref)

# ==============================================================================
# 4. Dynamic Covariates and Point Patterns
# ==============================================================================

all_points_sf      <- list()
dynamic_stack_list <- list()

cat("Processing temporal fusion...\n")

for (date in dates)
{
  # Point Pattern
  pp_temp <- st_read(paste0(path, "pp-", date, ".shp"), quiet = TRUE)
  pp_temp <- st_transform(pp_temp, crs(master_ref))
  all_points_sf[[date]] <- pp_temp
  
  # Satellite Rasters
  s1_vh_t <- align_raster(paste0(path, "s1vh-",  date, ".tif"), master_ref)
  s1_vv_t <- align_raster(paste0(path, "s1vv-",  date, ".tif"), master_ref)
  ndvi_t  <- align_raster(paste0(path, "ndvi-",  date, ".tif"), master_ref)
  mndwi_t <- align_raster(paste0(path, "mndwi-", date, ".tif"), master_ref)
  awei_t  <- align_raster(paste0(path, "awei-",  date, ".tif"), master_ref)
  
  r_stack <- c(s1_vh_t, s1_vv_t, ndvi_t, mndwi_t, awei_t)
  names(r_stack) <- c("vh", "vv", "ndvi", "mndwi", "awei")
  dynamic_stack_list[[date]] <- r_stack
}

# Merge points 
merged_sf <- do.call(rbind, all_points_sf)

# ==============================================================================
# 5. Construction and Normalization of the Final Covariate Stack
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
for (i in 1:nlyr(raw_covariates)) 
{
  final_covariates_terra[[i]] <- normalize_raster_terra(raw_covariates[[i]])
}

# Verification
print(minmax(final_covariates_terra))

# ==============================================================================
# 6. LGCP Geometry: Boundary and Mesh
# ==============================================================================

cat("\nSetting INLA LGCP...\n")

# Boundary. Construct a polygon from spatial extent of raster data
boundary_sf <- st_as_sf
(
  as.polygons(ext(final_covariates_terra), crs = crs(final_covariates_terra))
)

# Extract coordinates of all observed points
locs_obs <- st_coordinates(merged_sf)

# Mesh construction
# Mesh_size controls mesh fineness, (e.g. 100)
mesh_size <- 100

# Compute the diagonal length of the AOI
diag_dist <- sqrt(diff(ext(final_covariates_terra)[1:2])^2 + 
                    diff(ext(final_covariates_terra)[3:4])^2)
max_edge  <- diag_dist / mesh_size

# Build a 2D triangulation mesh from the observation locations
mesh <- inla.mesh.2d(
  loc     = locs_obs,
  max.edge = c(max_edge, max_edge * 2),
  cutoff   = max_edge / 5
)

# Define a Matern SPDE model on the mesh
spde <- inla.spde2.matern(mesh = mesh, alpha = 2)

# Compute the local finite element areas associated with mesh vertices
local_areas <- diag(inla.mesh.fem(mesh, order = 1)$c0)

mesh_locs_df <- data.frame(x = mesh$loc[, 1], y = mesh$loc[, 2])
mesh_locs_sf <- st_as_sf(mesh_locs_df, coords = c("x", "y"),
                         crs = crs(final_covariates_terra))

is_inside <- lengths(st_intersects(mesh_locs_sf, boundary_sf)) > 0
local_areas[!is_inside] <- 0

# ==============================================================================
# 7. Observed points
# ==============================================================================

cat("Extracting (normalized) covariates...\n")

# Extract covariate values from covariates at observed locations using bilinear interpolation
covs_obs <- terra::extract(final_covariates_terra, locs_obs, method = "bilinear")
covs_int <- terra::extract(final_covariates_terra, mesh$loc[, 1:2], method = "bilinear")

df_obs <- as.data.frame(covs_obs)
df_int <- as.data.frame(covs_int)

# ID Column cleanup
if ("ID" %in% names(df_obs)) df_obs$ID <- NULL
if ("ID" %in% names(df_int)) df_int$ID <- NULL

vars_nombres <- names(final_covariates_terra)

# NA imputation
for (var in vars_nombres) {
  df_obs[[var]] <- impute_na(df_obs, var)
  df_int[[var]] <- impute_na(df_int, var)
}

df_obs_sc <- df_obs
df_int_sc <- df_int
print(head(df_obs_sc))

# ==============================================================================
# 8. INLA Stacks for LGCP
# ==============================================================================

cat("Creating INLA Stacks...\n")

n_obs <- nrow(locs_obs)
n_int <- mesh$n

# Observation stack: y = 1, E = 0
stk_obs <- inla.stack(
  data = list(y = rep(1, n_obs), E = rep(0, n_obs)),
  A    = list(1, inla.spde.make.A(mesh, locs_obs)),
  effects = list(
    c(list(b0 = 1), df_obs_sc),
    list(s = 1:spde$n.spde)
  ),
  tag = 'obs'
)

# Integration stack: y = 0, E = ocal integration area
stk_int <- inla.stack(
  data = list(y = rep(0, n_int), E = local_areas),
  A    = list(1, inla.spde.make.A(mesh, mesh$loc[, 1:2])),
  effects = list(
    c(list(b0 = 1), df_int_sc),
    list(s = 1:spde$n.spde)
  ),
  tag = 'int'
)

# Combine both stacks into one joined stack 
join_stack <- inla.stack(stk_obs, stk_int)


# ==============================================================================
# 9. Model Specification and INLA Estimation
# ==============================================================================

cov_names <- names(final_covariates_terra)

# LGCP model
# Specify the log-intensity model Implemented as INLA formula
formula_lgcp <- as.formula(
  paste0(
    "y ~ 0 + b0 + ",
    paste(cov_names, collapse = " + "),
    " + f(s, model = spde)"
  )
)

cat("Running INLA (Family: Poisson)...\n")

inla_data <- inla.stack.data(join_stack)

# Run INLA
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

cat("Summary of Fixed Effects:\n")
print(result$summary.fixed)

cat("LGCP analysis completed\n")

# ==============================================================================
# 10. Projection of Latent Spatial Field (Gaussian Field)
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
# 11. Construction of the Final Risk Map
# ==============================================================================

fixed_effects <- result$summary.fixed[, "mean"]

if ("(Intercept)" %in% names(fixed_effects)) 
{
  intercept <- fixed_effects["(Intercept)"]
  betas <- fixed_effects[names(fixed_effects) != "(Intercept)"]
} else if ("b0" %in% names(fixed_effects)) {
  intercept <- fixed_effects["b0"]
  betas <- fixed_effects[names(fixed_effects) != "b0"]
} else {
  intercept <- fixed_effects[1]
  betas <- fixed_effects[-1]
}

cat("Intercept:", intercept, "\n")
cat("Betas found:\n")
print(betas) 

risk_map_log <- final_covariates_terra[[1]] * 0 + intercept

available_vars <- names(final_covariates_terra)

for (var_name in available_vars) {
  if (var_name %in% names(betas)) {
    beta_val <- betas[[var_name]]
    cat(paste("Sumando:", var_name, "*", round(beta_val, 4), "\n"))
    risk_map_log <- risk_map_log + (final_covariates_terra[[var_name]] * beta_val)
  }
}

spatial_raster <- rast(final_covariates_terra[[1]])
coords_raster <- crds(spatial_raster, na.rm = FALSE) 

proj_raster <- inla.mesh.projector(mesh, loc = as.matrix(coords_raster))
spatial_field_vals <- inla.mesh.project(proj_raster, result$summary.random$s$mean)

if(length(spatial_field_vals) != ncell(spatial_raster))
{
  stop("The projection does not match the number of cells in the raster")
}

values(spatial_raster) <- spatial_field_vals
spatial_raster[is.na(spatial_raster)] <- 0

total_log_intensity <- risk_map_log + spatial_raster

mm_log <- minmax(total_log_intensity)
cat("Range of values ​​on a logarithmic scale (before exp):\n")
print(mm_log)

max_log_val <- mm_log[2, 1] 

if (!is.finite(max_log_val)) {
  stop("The logarithmic map contains infinite values or NaNs")
}

relative_intensity <- exp(total_log_intensity - max_log_val)

mm_rel <- minmax(relative_intensity)
min_v  <- mm_rel[1, 1]
max_v  <- mm_rel[2, 1]


if ((max_v - min_v) < 1e-9) 
{
  warning("Model not trained correctly")
  risk_index <- relative_intensity
} else {
  risk_index <- (relative_intensity - min_v) / (max_v - min_v)
}

names(risk_index) <- "Flood_Susceptibility"

# ==============================================================================
# 12. Visualization-Oriented Risk Map (Log Scale)
# ==============================================================================

risk_log_metric <- total_log_intensity


mm_log <- minmax(risk_log_metric)
min_l <- mm_log[1, 1]
max_l <- mm_log[2, 1]

risk_visual <- (risk_log_metric - min_l) / (max_l - min_l)
names(risk_visual) <- "Risk_Index_LogScale"

df_risk <- as.data.frame(risk_visual, xy = TRUE, na.rm = TRUE)

ggplot(df_risk, aes(x = x, y = y, fill = Risk_Index_LogScale)) +
  geom_raster() +
  scale_fill_viridis_c(option = "turbo", 
                       name = "Risk index",
                       labels = scales::percent) + 
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "Risk map",
    subtitle = "visualization of relative risk gradients",
    x = "Lon/X", y = "Lat/Y"
  )

# Save final result
terra::writeRaster(risk_visual, "Flood_Risk_Visual.tif", overwrite = TRUE)
