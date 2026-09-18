#' @title Build, Get Important Variables for, Predict, and Evaluate Component Models
#' @description This is a wrapper function for \code{build_sdm}, \code{calculate_sdm_variable_importance}, \code{make_sdm_predictions}, and \code{calculate_sdm_auc}. This function produces log files and has skip functionality to assist in running multiple species in parallel.

#' @param spp Species name to add to log files and save data to correct directory (see vignette for recommended directory set up)
#' @param model Component model name. One of the following: gam, maxent, rf, brt, sdmtmb
#' @param training_years,test_years vectors with lengths equal to 2, indicating the maximum and minimum years that identify the desired training and test datasets
#' @param all_years vectors with lengths equal to 2, indicating the maximum and minimum years that will be used in training, testing, and future predictions. Used to build 'sdmtmb' models and passed to \code{build_sdm] via the \code{year_range} argument
#' @param dyn_names a vector of shorthand names for variable to help isolate desired environmental data
#' @param release release code for MOM6 data. Helps pull correct training/test dataset associated with the MOM6 data with the same name
#' @param spatial_temporal TRUE/FALSE to determine method for normalizing. Helps pull correct training/test dataset associated with the MOM6 data with the same name
#' @param mask_bathy TRUE/FALSE indicating whether or not bathymetry data was used as a mask for raw data before normalization. Helps pull correct training/test dataset associated with the MOM6 data with the same name
#' @param rm_corr TRUE/FALSE indicating whether or not correlated environmental covariates were removed. Helps to pull correct training/test dataframes 
#' @param static_variables a WRAPPED spatRaster containing static variables such as bathymetry. Each layer should be named.
#' @param skip TRUE/FALSE indicating whether to skip creating the raster file if file already exists

#' @return returns the AUC of the produced model. Outputs from the subsequent functions called within are saved within specific directories. See the vignette for recommended directory set up.

component_sdms_wrapper <- function(spp, model, training_years, test_years, all_years, dyn_names, release, spatial_temporal, mask_bathy, rm_corr, static_variables, skip = TRUE) {
  # Wrap the entire wrapper function execution in a outer tryCatch
  # to guarantee no error kills the parallel worker thread.
  tryCatch({
    
    # ==========================================================
    # STEP 0: Set Up
    # ==========================================================
    suffix <- if(spatial_temporal) "" else "_global"
    bathy_suffix <- if(mask_bathy) "masked" else ""
    corr_suffix <- if(rm_corr) "rmcorr" else ""
    
    # Define standard paths
    spp_dir          <- file.path(getwd(), spp)
    model_path       <- file.path(spp_dir, 'model_output', 'models', paste0(toupper(model), '.rds'))
    importance_path  <- file.path(spp_dir, 'model_output', 'importance', paste0(toupper(model), '.rds'))
    predictions_path <- file.path(spp_dir, 'output_rasters', paste0(toupper(model), '_hindcast_', release, '_', bathy_suffix, suffix, '.tif'))
    evaluation_path  <- file.path(spp_dir, 'model_output', 'eval_metrics', paste0(toupper(model), '.rds'))
    
    # Set up logger
    log_file <- file.path(getwd(), 'logs', paste0(model, '.log'))
    log_appender(appender_file(log_file))
    
    log_info("Processing species: {spp} for model type: {model}")
    
    # Load training data
    training_name <- file.path(spp_dir, paste0('training_', training_years[1], '_', training_years[2], '_', corr_suffix, '_hindcast_', release, '_', bathy_suffix, suffix, '.csv'))
    test_name     <- file.path(spp_dir, paste0('test_', test_years[1], '_', test_years[2], '_', corr_suffix, '_hindcast_', release, '_', bathy_suffix, suffix, '.csv'))
    
    if (!file.exists(training_name) | !file.exists(test_name)) {
      log_error("Data file missing for species: {spp}.")
      return(NULL) # Exit function gracefully
    }
    
    dfT <- read.csv(file.path(training_name)) 
    
    # Get covariates in dataframe
    static_variables <- terra::unwrap(static_variables)
    d_names <- dyn_names[dyn_names %in% names(dfT)]
    s_names <- names(static_variables)[names(static_variables) %in% names(dfT)]
    var_names <- c(d_names, s_names)
    static_variables <- terra::wrap(static_variables)
    
    # ==========================================================
    # STEP 1: Model Building
    # ==========================================================
    model_exists <- file.exists(model_path)
    
    if (skip && model_exists) {
      log_warn("Model file already exists for {spp}. Skipping model generation.")
      load(model_path) 
    } else {
      log_info("Building SDM model for {spp}...")
      
      mod <- tryCatch({
        build_sdm(
          se = dfT, pa_col = 'pa', xy_col = c("grid.lon", "grid.lat"),
          month_col = 'month', year_col = 'year', model = model, var_names = var_names,
          year_range = all_years
        )
      }, error = function(e) {
        log_error("Failed to build model for {spp}: {e$message}")
        return(NULL)
      })
      
      # If model creation failed, return early
      if (is.null(mod)) return(NULL)
      
      save(mod, file = model_path)
      log_info("Model successfully saved to {model_path}")
    }
    
    # ==========================================================
    # STEP 2: Variable Importance
    # ==========================================================
    importance_exists <- file.exists(importance_path)
    
    if (skip && importance_exists) {
      log_warn("Variable importance file already exists for {spp}. Skipping calculation.")
    } else {
      log_info("Calculating variable importance for {spp}...")
      
      if (!exists("mod")) load(model_path)
      
      imp <- tryCatch({
        calculate_sdm_variable_importance(
          mod = mod, se = dfT, pa_col = 'pa', xy_col = c("grid.lon", "grid.lat"),
          month_col = 'month', year_col = 'year', model = model, var_names = var_names
        )
      }, error = function(e) {
        log_error("Failed to calculate variable importance for {spp}: {e$message}")
        return(NULL)
      })
      
      if (is.null(imp)) return(NULL)
      
      save(imp, file = importance_path)
      log_info("Variable importance successfully saved to {importance_path}")
    }
    
    # ==========================================================
    # STEP 3: Predictions
    # ==========================================================
    predictions_exists <- file.exists(predictions_path)
    
    if (skip && predictions_exists) {
      log_warn("{model} predictions already exist for {spp}. Skipping calculation.")
    } else {
      log_info("Predicting {model} for {spp}...")
      
      if (!exists("mod")) load(model_path)
      
      env_rasters <- vector(mode = 'list', length = length(d_names))
      for (x in seq_along(d_names)) {
        raster_path <- paste0('./Data/MOM6/norm_', d_names[x], '_hindcast_', release, '_', bathy_suffix, suffix, '.tif')
        if (!file.exists(raster_path)) {
          log_error("Missing upstream raster for {spp}: {raster_path}")
          return(NULL)
        }
        env_rasters[[x]] <- terra::rast(raster_path)
      }
      names(env_rasters) <- d_names
      
      preds <- tryCatch({
        make_sdm_predictions(
          mod = mod,
          model = model,
          rasts = env_rasters,
          static_variables = terra::unwrap(static_variables),
          se = dfT,
          pa_col = 'pa',
          month_col = 'month',
          year_col = 'year',
          xy_col = c("grid.lon", "grid.lat")
        )
      }, error = function(e) {
        log_error("Failed to predict {model} for {spp}: {e$message}")
        return(NULL)
      })
      
      if (is.null(preds)) return(NULL)
      
      terra::writeRaster(preds, file = predictions_path, overwrite = TRUE)
      log_info("{model} predictions for {spp} successfully saved to {predictions_path}")
    }
    
    # ==========================================================
    # STEP 4: EVALUATE
    # ==========================================================
    evaluation_exists <- file.exists(evaluation_path)
    
    if (skip && evaluation_exists) {
      log_warn("{model} has already been evaluated for {spp}. Skipping calculation.")
      if (!exists("ev")) load(evaluation_path)
    } else {
      log_info("Evaluating {model} for {spp}...")
      
      if (!exists("preds")) preds <- terra::rast(predictions_path)
      
      dfTest <- read.csv(file.path(test_name)) 
      
      ev <- tryCatch({
        calculate_sdm_auc(
          model = model,
          data_type = 'external',
          data = dfTest,
          prediction_rasters = preds
        )
      }, error = function(e) {
        log_error("Failed to evaluate {model} for {spp}: {e$message}")
        return(NULL)
      })
      
      if (is.null(ev)) return(NULL)
      
      save(ev, file = evaluation_path)
      log_info("{model} evaluation for {spp} successfully saved to {evaluation_path}")
    }
    
    return(ev)
    
  }, error = function(e) {
    # Catches any unexpected base R errors not caught inside individual steps
    log_error("Unexpected error in wrapper for {spp}: {e$message}")
    return(NULL)
  })
}