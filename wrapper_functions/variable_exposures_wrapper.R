#' @title Make Timeseries and Maps of Variable Exposure
#' @description A wrapper function for \code{make_variable_exposure} that handles object loading, and generating mean SDMs from timeseries, and produces both maps and timeseries
#'
#' @param spp species name. Used to pull correct data and save outputs in species-specific folders.
#' @param spatial_temporal TRUE/FALSE to determine method for normalizing. Helps pull correct ensemble model associated with the MOM6 data with the same name
#' @param mask_bathy TRUE/FALSE indicating whether or not bathymetry data was used as a mask for raw data before normalization. Helps pull correct ensemble model associated with the MOM6 data with the same name
#' @param rm_corr TRUE/FALSE indicating whether or not correlated environmental covariates were removed. Helps to pull correct training/test dataframes 
#' @param release release code for MOM6 data. Helps pull correct ensemble predictions associated with the MOM6 data with the same name
#' @param training_years vector with lengths equal to 2, indicating the maximum and minimum years that identify the desired training datasets. Used to help select correct environmental variables
#' @param sdm_threshold value between 0 and 1. Will remove values lower than this threshold from average ensemble model results to help reduce weird aliasing that can occur in workflow. Defaults to 0.1.
#' @param spp species name. Used to pull correct data and save outputs in species-specific folders.
#' @param forecast_release,hindcast_release MOM6 release codes for the (f)orecast and (h)indcasts used. Used to pull correct variable exposures
#' @param forecast_forecast_init forecast_initialization code corresponding to the forecast_initalization date of the desired forecast data. Used to pull correct variable exposures
#' @param hindcast_hindcast_yr_range character string corresponding to the years in the hindcast data used. Used to pull correct ranked exposure values and save the data properly
#'
#' @return Nothing is returned. The outputs from \code{make_variable_exposure(type = 'map')} and \code{make_variable_exposure(type = 'timeseries')} are saved in the appropriate folders

variable_exposures_wrapper <- function(
  spp, 
  forecast_release, 
  forecast_forecast_init,
  hindcast_release, 
  hindcast_hindcast_yr_range,
  spatial_temporal, 
  mask_bathy, 
  rm_corr,
  sdm_threshold = 0.1,
  dyn_vars
) {

  # ==========================================================
  # STEP 0: Set Up
  # ==========================================================
  # Set up the logger to output to your specific file
  log_file <- file.path(getwd(), 'logs', 'variable_exposure.log')
  log_appender(appender_file(log_file))

  log_info("Calculating variable exposures for {spp}")

  #suffixes to help locate correct data
  suffix <- if(spatial_temporal) "" else "_global"
  bathy_suffix <- if(mask_bathy) "masked" else ""
  corr_suffix <- if(rm_corr) "rmcorr" else ""
  
  # Define standard paths
  predictions_path <- file.path('/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs', spp, 'output_rasters', paste0('ENSEMBLE_hindcast_', hindcast_release, '_', bathy_suffix, suffix, '.tif'))
  
  # Load training data
  training_name <- file.path('/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs', spp, paste0('training_', training_years[1], '_', training_years[2], '_', corr_suffix, '_hindcast_', hindcast_release, '_', bathy_suffix, suffix, '.csv'))
  
  if (!file.exists(training_name)) {
    log_error("Data file missing for species: {spp}.")
    return(NULL) # Exit function gracefully
  }
  
  dfT <- read.csv(file.path(training_name)) 
  
  # Get covariates in dataframe
  d_names <- dyn_vars[dyn_vars %in% names(dfT)]

  # ==========================================================
  # STEP 1: Load in Data
  # ==========================================================
  #model
  if (!file.exists(predictions_path)) {
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
  exp_rasters <- vector(mode = 'list', length = length(d_names))
  for (x in seq_along(d_names)) {
    raster_path <- paste0('./RawExposure/Data/',
                          d_names[x],
                          '_rankedexposure_', forecast_release, '_', forecast_init, '_', hindcast_release,'_',hindcast_yr_range, '_global.tif')
    if (!file.exists(raster_path)) {
      log_error("Missing upstream raster for {spp}: {raster_path}")
      return(NULL)
    }
    
    exp <- terra::rast(raster_path)
    
    #reproject because there are slight differences in resolution/extent for some reason, especially with the forecasts
    exp_aligned <- terra::resample(exp, avgHSM, method = "bilinear")
    
    exp_rasters[[x]] <- exp_aligned
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
      paste0('/variable_exposure_maps_', forecast_release, '_', forecast_init, '_', hindcast_release,'_',hindcast_yr_range, '.tif')
    ),
    overwrite = TRUE
  )

  log_info('spatial variable exposures for {spp} complete.')

  # ==========================================================
  # STEP 3: Calculate Exposures Across Time
  # ==========================================================

  if(file.exists(paste0('/home/kgallagher/ClimateVulnerabilityAssessment2.0/shpfiles/species_stock_areas/', spp, '.shp'))){
    stocks <- terra::vect(paste0('/home/kgallagher/ClimateVulnerabilityAssessment2.0/shpfiles/species_stock_areas/', spp, '.shp'))
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
  saveRDS(
    vecExp,
    file = paste0(
      file.path(getwd(), spp, 'Data'),
      paste0('/variable_exposure_timeseries_', forecast_release, '_', forecast_init, '_', hindcast_release,'_',hindcast_yr_range,'.rds')
    )
  )

  log_info('variable exposure timeseries for {spp} complete.')
}
