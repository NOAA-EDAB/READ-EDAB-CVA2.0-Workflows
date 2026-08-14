#' @title Make Timeseries and Maps of Variable Exposure
#' @description A wrapper function for \code{make_variable_exposure} that handles object loading, and generating mean SDMs from timeseries, and produces both maps and timeseries
#'
#' @param spp species name. Used to pull correct data and save outputs in species-specific folders.
#' @param spatial_temporal TRUE/FALSE to determine method for normalizing. Helps pull correct ensemble model associated with the MOM6 data with the same name
#' @param mask_bathy TRUE/FALSE indicating whether or not bathymetry data was used as a mask for raw data before normalization. Helps pull correct ensemble model associated with the MOM6 data with the same name
#' @param release release code for MOM6 data. Helps pull correct ensemble predictions associated with the MOM6 data with the same name
#' @param sdm_threshold value between 0 and 1. Will remove values lower than this threshold from average ensemble model results to help reduce weird aliasing that can occur in workflow. Defaults to 0.1.
#' @param present_time,future_time character strings indicating the present and future time series to compare. Example: '1993-2019'. Used to pull correct ranked exposure values and save the data properly
#'
#' @return Nothing is returned. The outputs from \code{make_variable_exposure(type = 'map')} and \code{make_variable_exposure(type = 'timeseries')} are saved in the appropriate folders

variable_exposures_wrapper <- function(
  spp,
  spatial_temporal, mask_bathy, release,
  sdm_threshold = 0.1,
  dyn_vars
) {

  # ==========================================================
  # STEP 0: Set Up
  # ==========================================================
  # Set up the logger to output to your specific file
  log_file <- file.path(getwd(), 'logs', 'ensemble.log')
  log_appender(appender_file(log_file))

  log_info("Calculating variable exposures for {spp}")

  #suffixes to help locate correct data
  suffix <- if(spatial_temporal) "" else "_global"
  bathy_suffix <- if(mask_bathy) "masked" else ""

  # Define standard paths
  predictions_path <- file.path('./SDMs/', spp, 'output_rasters', paste0('ENSEMBLE_hindcast_', release, '_', bathy_suffix, suffix, '.tif'))

  # ==========================================================
  # STEP 1: Load in Data
  # ==========================================================
  #model
  if (!file.exists(predictions_path) | !file.exists(test_name)) {
    log_error("Ensemble model missing for species: {spp}")
    stop("Aborting: Ensemble model not found.")
  }
  abund <- terra::rast(predictions_path)

  #avg ensemble HSM
  avgHSM <- terra::tapp(abund, rep(1:12, times = terra::nlyr(abund)/12), fun = 'mean')#assuming ensemble is predicted on monthly timesteps and encompases complete years (ie starts in a January and stops in a December), create monthly average data
  names(avgHSM) <- month.abb

  ###remove hsm with less than threshold to avoid weird aliasing
  avgHSM<- terra::ifel(avgHSM <= sdm_threshold, NA, avgHSM)

  #ranked exposure data
  exp_rasters <- vector(mode = 'list', length = length(dyn_vars))
  for (x in seq_along(dyn_vars)) {
    raster_path <- paste0('./RawExposure/Data/',
                          dyn_vars[x],
                          '_rankedexposure_r20250925_i202501_r20250715_20142023_global.tif')
    if (!file.exists(raster_path)) {
      log_error("Missing upstream raster for {spp}: {raster_path}")
      return(NULL)
    }
    exp_rasters[[x]] <- terra::rast(raster_path)
  }
  names(exp_rasters) <- d_names

  # ==========================================================
  # STEP 2: Calculate Exposures Across Space
  # ==========================================================
  #map
  mapExp <- make_variable_exposure(
    type = 'map',
    ranked_exposure = exp_rasters,
    sdm_raster = avgHSM
  )
  terra::writeRaster(
    x = mapExp,
    filename = paste0(
      file.path(getwd(), spp, 'Data'),
      '/variable_exposure_maps_r20250925_i202501_r20250715_20142023.tif'
    ),
    overwrite = TRUE
  )

  log_info('spatial variable exposures for {spp} complete.')

  # ==========================================================
  # STEP 3: Calculate Exposures Across Time
  # ==========================================================

  if(file.exists(paste0('./shpfiles/species_stock_areas/', spp, '.shp'))){
    stocks <- terra::vect(paste0('./shpfiles/species_stock_areas/', spp, '.shp'))
  } else {
    stocks <- NULL
    log_info('No stock shpfiles found for {spp}. Only calculating global variable exposure timeseries')
  }

  #timeseries
  vecExp <- make_variable_exposure(
    type = 'timeseries',
    ranked_exposure = exp_rasters,
    sdm_raster = avgHSM,
    stock_polys = stocks
  )
  save(
    vecExp,
    file = paste0(
      file.path(getwd(), spp, 'Data'),
      '/variable_exposure_timeseries_r20250925_i202501_r20250715_20142023.rds'
    )
  )

  log_info('variable exposure timeseries for {spp} complete.')
}
