#' @title Make Timeseries and Maps of Species-Specific Exposure for each variable
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
  present_time,
  future_time
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
  spp_dir        <- file.path(getwd(), spp)
  predictions_path <- file.path(spp_dir, 'output_rasters', paste0('ENSEMBLE_hindcast_', release, '_', bathy_suffix, suffix, '.tif'))

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
  #load in ranked data
  load(paste0(
    './RawExposure/Data/',
    present_time,
    ' vs ',
    future_time,
    '_exposure_ranked.RData'
  )) #expRanked

  # ==========================================================
  # STEP 2: Calculate Exposures Spatially
  # ==========================================================
  #map
  mapExp <- make_variable_exposure(
    type = 'map',
    ranked_exposure = expRanked,
    sdm_raster = avgHSM
  )
  save(
    mapExp,
    file = paste0(
      file.path(getwd(), spp, 'Data'),
      '/',
      present_time,
      ' vs ',
      future_time,
      '/variable_exposure_maps.RData'
    )
  )

  log_info('spatial variable exposures for {spp} complete.')

  # ==========================================================
  # STEP 3: Calculate Exposures Across Time
  # ==========================================================

  if(file.exists(paste0('./shpfiles/species_stock_areas/', spp, '.shp'))){
    stocks <- terra::vect(paste0('./shpfiles/species_stock_areas/', spp, '.shp'))
  } else {
    stocks <- NULL
    log_info('No stock shpfiles found for {spp}. Only calculating global exposure timeseries')
  }

  #timeseries
  vecExp <- make_variable_exposure(
    type = 'timeseries',
    ranked_exposure = expRanked,
    sdm_raster = avgHSM,
    stock_polys = stocks
  )
  save(
    vecExp,
    file = paste0(
      file.path(getwd(), spp, 'Data'),
      '/',
      present_time,
      ' vs ',
      future_time,
      '/variable_exposure_timeseries.RData'
    )
  )

  log_info('variable exposure timeseries for {spp} complete.')
}
