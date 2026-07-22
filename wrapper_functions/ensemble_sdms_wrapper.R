#' @title Build, Predict, and Evaluate Ensemble Model
#' @description This is a wrapper function for \code{build_sdm}, \code{make_sdm_predictions}, and \code{calculate_sdm_auc}. This function produces log files and has skip functionality to assist in running multiple species in parallel.

#' @param spp Species name to add to log files and save data to correct directory (see vignette for recommended directory set up)
#' @param model Component model name. One of the following: gam, maxent, rf, brt, sdmtmb
#' @param training_years,test_years vectors with lengths equal to 2, indicating the maximum and minimum years that identify the desired training and test datasets
#' @param short_names a vector of shorthand names for variable to help pull desired environmental data based on naming convention
#' @param release release code for MOM6 data. Helps pull correct training/test dataset associated with the MOM6 data with the same name
#' @param spatial_temporal TRUE/FALSE to determine method for normalizing. Helps pull correct training/test dataset associated with the MOM6 data with the same name
#' @param mask_bathy TRUE/FALSE indicating whether or not bathymetry data was used as a mask for raw data before normalization. Helps pull correct training/test dataset associated with the MOM6 data with the same name
#' @param rm_corr TRUE/FALSE indicating whether or not correlated environmental covariates were removed. Helps to pull correct training/test dataframes 
#' @param static_variables a WRAPPED spatRaster containing static variables such as bathymetry. Each layer should be named.
#' @param skip TRUE/FALSE indicating whether to skip creating the raster file if file already exists

#' @return returns the AUC of the produced model. Outputs from the subsequent functions called within are saved within specific directories. See the vignette for recommended directory set up.

ensemble_sdms_wrapper <- function(spp, training_years, test_years, dyn_names, release, spatial_temporal, mask_bathy, rm_corr, static_variables, skip = TRUE) {
  # ==========================================================
  # STEP 0: Set Up
  # ==========================================================
  #suffixes to help locate correct data
  suffix <- if(spatial_temporal) "" else "_global"
  bathy_suffix <- if(mask_bathy) "masked" else ""
  corr_suffix <- if(rm_corr) "rmcorr" else ""
  
  # Define standard paths
  spp_dir        <- file.path(getwd(), spp)
  model_path     <- file.path(spp_dir, 'model_output', 'models',  'ENSEMBLE.rds')
  importance_path <- file.path(spp_dir, 'model_output', 'importance', 'ENSEMBLE.rds')
  predictions_path <- file.path(spp_dir, 'output_rasters', paste0('ENSEMBLE_hindcast_', release, '_', bathy_suffix, suffix, '.tif'))
  evaluation_path <- file.path(spp_dir, 'model_output', 'eval_metrics', 'ENSEMBLE.rds')
  
  # Set up the logger to output to your specific file
  log_file <- file.path(getwd(), 'logs', 'ensemble.log')
  log_appender(appender_file(log_file))
  
  log_info("Making {spp} ensemble")
  
  # Load training data
  training_name <- file.path(spp_dir, paste0('training_', training_years[1], '_', training_years[2], '_', corr_suffix, '_hindcast_', release, '_', bathy_suffix, suffix, '.csv'))
  test_name     <- file.path(spp_dir, paste0('test_', test_years[1], '_', test_years[2], '_', corr_suffix, '_hindcast_', release, '_', bathy_suffix, suffix, '.csv'))
  
  if (!file.exists(training_name) | !file.exists(test_name)) {
    log_error("Data file missing for species: {spp}")
    stop("Aborting: training or test dataset not found.")
  }
  dfT <- read.csv(file.path(training_name)) 
  
  # ==========================================================
  # STEP 1: Model Building
  # ==========================================================
  model_exists <- file.exists(model_path)
  
  if (skip && model_exists) {
    log_warn("Ensemble model file already exists for {spp}. Skipping model generation.")
    load(model_path) 
  } else {
    log_info("Building ensemble model for {spp}...")
    
    
    #Put together preds
    mod.preds <- dir(file.path(spp_dir, 'output_rasters'), full.names = T, pattern = paste0('_hindcast_', release, '_', bathy_suffix, suffix, '.tif')) #get list of models
    if(grepl('ENSEMBLE', mod.preds)){
      mod.preds <- mod.preds[-grep('ENSEMBLE', mod.preds)] #remove ensemble if present (should only be true if overwriting data)
    }
    
    pList <- vector('list', length = length(mod.preds)) #initiate blank list of preds 
    
    dfTest <- read.csv(file.path(test_name)) #load test data
    
    #make list of data frames
    for(x in 1:length(mod.preds)){
      r <- terra::rast(mod.preds[x])
      pList[[x]] <- build_preds_df(observations = dfTest, 
                                   xy_col = c("grid.lon", "grid.lat"),
                                   prediction_rasters = r)
    }
    
    #put together weights 
    #load in AUCs
    evalFlist <- dir(
      file.path(getwd(), spp, 'model_output', 'eval_metrics'),
      pattern = '.rds',
      full.names = T
    )
    if(grepl('ENSEMBLE', evalFlist)){
      evalFlist <- evalFlist[-grep('ENSEMBLE', evalFlist)] #remove ensemble if present (should only be true if overwriting data)
    }
    
    eval <- vector(length = length(evalFlist))
    for (y in 1:length(evalFlist)) {
      load(evalFlist[y])
      eval[y] <- ev
    }
    
    #generate weights
    weights <- eval / sum(eval) #we need to make weights like this since AUC bigger = better; whereas RMSE smaller = better
    save(
      weights,
      file = file.path(getwd(), spp, 'model_output',
                       'ensemble_weights.rds')
    )
    
    # Wrap model building in tryCatch to log errors cleanly
    mod <- tryCatch({
      build_sdm(
        model = 'ensemble', 
        pred.list = pList,
        ensemble_weights = weights
      )
    }, error = function(e) {
      log_error("Failed to build ensemble for {spp}: {e$message}")
      stop(e)
    })
    
    save(mod, file = model_path)
    log_info("Ensemble successfully saved to {model_path}")
  }
  
  # ==========================================================
  # STEP 2: Predictions
  # ==========================================================
  predictions_exists <- file.exists(predictions_path)
  
  if (skip && predictions_exists) {
    log_warn("Ensemble predictions already exist for {spp}. Skipping calculation.")
  } else {
    log_info("Predicting Ensemble for {spp}...")
    
    if (!exists("weights")) load(file.path(getwd(), spp, 'model_output', 'ensemble_weights.rds'))
    
    #load in prediction rasters from other models
    mod.preds <- dir(file.path(spp_dir, 'output_rasters'), full.names = T, pattern = paste0('_hindcast_', release, '_', bathy_suffix, suffix, '.tif')) #get list of models
    if(grepl('ENSEMBLE', mod.preds)){
      mod.preds <- mod.preds[-grep('ENSEMBLE', mod.preds)] #remove ensemble if present (should only be true if overwriting data)
    }
    
    pred_rasters <- vector(mode = 'list', length = length(mod_preds))
    for (x in 1:length(mod_preds)) {
      pred_rasters[[x]] <- terra::rast(mod.preds[x])
    }
    
    preds <- tryCatch({
      make_sdm_predictions(
        model = 'ensemble',
        rasts = pred_rasters,
        weights = weights
      )
      
    }, error = function(e) {
      log_error("Failed to predict ensemble for {spp}: {e$message}")
      stop(e)
    })
    
    
    
    terra::writeRaster(preds, file = predictions_path, overwrite = T)
    log_info("{model} predictions for {spp} successfully saved to {predictions_path}")
  }
  
  
  # ==========================================================
  # STEP 3: EVALUATE
  # ==========================================================
  evaluation_exists <- file.exists(evaluation_path)
  
  if (skip && evaluation_exists) {
    log_warn("Ensemble has already been evaluated for {spp}. Skipping calculation.")
  } else {
    log_info("Evaluating ensemble for {spp}...")
    
    if (!exists("preds")) preds <- terra::rast(predictions_path)
    
    dfTest <- read.csv(file.path(test_name)) 
    
    ev <- tryCatch({
      calculate_sdm_auc(
        model = 'ensemble',
        data_type = 'external',
        data = dfTest,
        prediction_rasters = preds
      )
    }, error = function(e) {
      log_error("Failed to evaluate ensemble for {spp}: {e$message}")
      stop(e)
    })
    
    save(ev, file = evaluation_path)
    log_info("Ensemble evaluation for {spp} successfully saved to {evaluation_path}")
  }
  
  return(ev)
}