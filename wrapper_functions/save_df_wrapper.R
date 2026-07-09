#' @title Save and Build Fisheries Data Frame
#' @description A wrapper function for the \code{build_fisheries_df} function. This function adds log and skip functionality to help with batch runs and running \code{build_fisheries_df} in parallel.

#' @param csv_name character string indicating which csv to use to create raster without extension (i.e. 'example' NOT 'example.csv')
#' @param is_obs TRUE/FALSE indicating whether or not the data is observer or similar fisheries-dependent data. Will force check to determine if species is observed at least 30 times throughout timeseries before creating raster
#' @param grid static link to a ncdcf object with the variables lon, lat, time - can be link to remote data - must be able to be read with nc_open
#' @param spp Species name to add to log files and save data to correct directory (see vignette for recommended directory set up)
#' @param spp_names a vector containing all possible names for the target species. Must have a length >= 1
#' @param skip TRUE/FALSE indicating whether to skip creating the raster file if file already exists

#' @return \code{save_df_wrapper} returns the range of the data.frame returned by \code{build_fisheries_df}. This should be equal to 0 1 if species is present in dataset, and 0 0 if species is not caught in dataset. This function will also save the resulting data.frame as a csv file in the species' input_csvs folder


save_df_wrapper <- function(csv_name, is_obs, grid, spp, spp_names, skip, force_overwrite = FALSE) {
  
  # 1. Dynamically route logs to a central file or individual files safely
  # logger handles multiple parallel processes writing to the same file much better than sink
  log_path <- file.path(getwd(), 'logs', 'build_dfs.log')
  log_appender(appender_file(log_path))
  
  # Define path where output is saved (saved as a variable to prevent typos and duplication)
  output_file <- file.path(getwd(), spp, 'input_csvs', paste0(csv_name, '.csv'))
  
  # 2. Check skip / overwrite logic up front
  # If force_overwrite is TRUE, we ignore the skip setting completely
  if (skip && !force_overwrite && file.exists(output_file)) {
    log_info("{csv_name} - {spp}: File exists and skip == TRUE. Skipping execution.")
    return(NA)
  }
  
  if (force_overwrite && file.exists(output_file)) {
    log_info("{csv_name} - {spp}: File exists but force_overwrite == TRUE. Re-running.")
  }
  
  log_info("{csv_name} - {spp}: Beginning processing...")
  
  # 3. Safe Execution Block using tryCatch
  result <- tryCatch({
    
    # Read the data safely inside the try block
    data_path <- paste0('./Data/csvs/standardized/', csv_name, '.csv')
    if(!file.exists(data_path)) stop(paste("Source CSV missing:", data_path))
    data <- read.csv(data_path)
    
    nms <- strsplit(spp_names, split = ',')[[1]]
    
    # Run core processing
    rast <- build_fisheries_df(
      data = data,
      is_obs = is_obs,
      grid = grid,
      all_names = nms
    )
    
    # Handle the output
    if (is.null(rast)) {
      log_warn("{csv_name} - {spp}: rast is NULL - minimum conditions not met.")
      return(NULL)
    } else {
      # Log the range instead of standard printing
      pa_range <- range(rast$pa, na.rm = TRUE)
      log_info("{csv_name} - {spp}: Success. PA range: {paste(pa_range, collapse = ' to ')}")
      
      # Ensure directories exist before writing
      dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
      
      write.csv(rast, file = output_file, row.names = FALSE)
      return(pa_range)
    }
    
  }, error = function(e) {
    # 4. CAPTURE THE CATASTROPHIC ERRORS
    # If build_fisheries_df crashes, this block catches it and logs exactly why
    log_error("{csv_name} - {spp}: CRASHED with error: {e$message}")
    return(NULL) 
  })
  
  return(result)
}
