#' @title Pull MOM6 Hindcast or Forecast Data
#' @description A wrapper function that pulls, averages, and normalizes the MOM6 hindcast or forecast data and saves the output from each step. This function adds log functionality to help with batch runs and running the encompassing functions in parallel. Note that unlike similar functions, there is no skip functionality since this function is meant to only be run once.

#' @param source 'hindcast' or 'forecast' to determine which MOM6 data to pull
#' @param var_name long variable name to pull. Must match names in the 'cefi_long_name' column provided JSON table
#' @param short_name a shorthand name for \code{var_name}. Used for saving data
#' @param json_url URL pointing to JSON table variable lists for desired MOM6 run type and domain
#' @param release release code. Must match one of the options in the 'cefi_release' column in provided JSON table
#' @param init initialization code. Must match one of the options in the 'cefi_init_date' column in provided JSON table. For forecast only
#' @param spatial.temporal TRUE/FALSE to determine method for normalizing. If TRUE, normalization is spatially and temporally explicit. If FALSE, normalization occurs using averages/standard deviations calculated across space and time
#' @param mask_bathy TRUE/FALSE indicating whether or not to use bathymetry data as a mask for raw data
#' @param bathy a spatRaster of bathymetry data. Must be same extent as results from pull_mom6_hindcast/forecast
#' @param bathy_range a vector containing the minimum and maximum desired bathymetry values  

#' @return the output from \code{normalize_model_data} - a list whose length is equal to the number of variables supplied, where each item in the list is a spatRaster of data associated with that variable, with the number of layers equal to the number of time steps available. The function also saves the output from each step - see the package website for necessary directory set up.

get_model_data_wrapper <- function(
    source,
    var_name,
    short_name,
    json_url,
    release, 
    init,
    spatial_temporal, 
    mask_bathy, 
    bathy, 
    bathy_range
) {
  #step 0 - set up logging & suffix
  suffix <- if(spatial_temporal) "" else "_global"
  
  log_appender(appender_file("./logs/mom6_hindcast_july2026.log"))
  log_info("Starting variable: {short_name} using source: {source}")
  
  # --- Step 1: Dynamic Data Pulling using switch() ---
  raw_data <- tryCatch({
    switch(tolower(source),
           "hindcast" = {
             pull_mom6_hindcast(
               var_url = json_url,
               req_var = var_name,
               release = release
             )
           },
           "forecast" = {
             pull_mom6_forecast(
               var_url = json_url,
               req_var = var_name,
               release = release,
               init = init,
               static_grid = static_grid
             )
           },
           # Default fallback error if you pass a typo 
           stop(paste("Unknown data source specified:", source))
    )
  }, error = function(e) {
    log_error("Failed to pull data for {short_name} from {source}: {e$message}")
    return(NULL)
  })
  
  # If the pull failed completely, exit early before hitting subsequent steps
  if (is.null(raw_data)){
    return(NULL)
  } else {
    
    terra::writeRaster(raw_data, 
                       filename = paste0('./Data/MOM6/raw_MOM6_',  short_name, '_', source, '_',release, suffix, '.tif'),
                       overwrite = T)
    log_info("{short_name} raw {source} data saved")
    
    #Step 1.5 - mask raw data if necessary 
    if(mask_bathy){
      raw_data <- terra::ifel(bathy <= bathy_range[1] | bathy > bathy_range[2], NA, raw_data)
    }
    
    # Step 2 & 3: Avg and SD
    avg_data <- avg_model_data(raw_data, spatial_temporal = spatial_temporal)
    sd_data  <- sd_model_data(raw_data, spatial_temporal = spatial_temporal)
    
    if (spatial.temporal) {
      terra::writeRaster(avg_data, filename = paste0('./Data/MOM6/avg_', short_name, '_', source, '_', release, '.tif'), overwrite = TRUE)
      terra::writeRaster(sd_data,  filename = paste0('./Data/MOM6/sd_', short_name, '_', source, '_', release, '.tif'),  overwrite = TRUE)
    } else {
      # If spatial.temporal is FALSE, they are global numeric values/vectors. Save as RDS.
      saveRDS(avg_data, file = paste0('./Data/MOM6/avg_', short_name, '_', source, '_', release, '_global.rds'))
      saveRDS(sd_data,  file = paste0('./Data/MOM6/sd_', short_name, '_', source, '_', release, '_global.rds'))
    }
    log_info("{short_name} average and standard deviations saved")
    
    # Step 4: Normalize
    norm_data <- normalize_model_data(
      raw = raw_data,
      avg = avg_data,
      sd = sd_data,
      spatial_temporal = spatial_temporal
    )
    terra::writeRaster(norm_data, filename = paste0('./Data/MOM6/norm_', short_name, '_', source, '_', release, suffix, '.tif'), overwrite = TRUE)
    log_info("{short_name} data normalized and saved")
    
    return(norm_data)
  }
} #end function
