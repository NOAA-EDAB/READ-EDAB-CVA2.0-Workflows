#' @title Combine Multiple Data.Frames of Fisheries Data
#' @description A wrapper function for the \code{merge_fisheries_df} function. This function adds log and skip functionality to help with batch runs and running \code{merge_fisheries_df} in parallel.
#'
#' @param name Species name to add to log files and save data to correct directory (see vignette for recommended directory set up)
#' @param skip TRUE/FALSE indicating whether to skip creating the raster file if file already exists
#' @param force_overwrite TRUE/FALSE designating whether or not to force the function to overwrite the existing file. Overrides skip and sets skip = FALSE. Defaults to FALSE. 
#'
#' @return \code{combine_rasters_wrapper} returns the range of the rasterBrick returned by \code{merge_fisheries_rasters}. This should be equal to 0 2, or else there are no presences in the dataset and the models will fail. This function will also save the resulting rasterBrick as a netcdf file in the species' input_rasters folder

combine_fisheries_dfs_wrapper <- function(name, skip, force_overwrite = FALSE) {
  
  # 1. Dynamically route logs to a central file or individual files safely
  # logger handles multiple parallel processes writing to the same file much better than sink
  log_path <- file.path(getwd(), 'logs', 'combine_dfs.log')
  log_appender(appender_file(log_path))
  
  # Define path where output is saved (saved as a variable to prevent typos and duplication)
  output_file <- file.path(getwd(), name, 'input_csvs', 'combined_pa.csv.csv')
  
  # 2. Check skip / overwrite logic up front
  # If force_overwrite is TRUE, we ignore the skip setting completely
  if (skip && !force_overwrite && file.exists(output_file)) {
    log_info("{name}: File exists and skip == TRUE. Skipping execution.")
    return(NA)
  }
  
  if (force_overwrite && file.exists(output_file)) {
    log_info("{name}: File exists but force_overwrite == TRUE. Re-running.")
  }
  
  log_info("{name}: Combining csvs...")
  
  # 3. Safe Execution Block using tryCatch
  result <- tryCatch({
    
    # Read the data safely inside the try block
    spp_dir <- file.path(getwd(), name, 'input_csvs')
    
    
    combinedDFs <- merge_fisheries_dfs(spp_dir)
    
    # Handle the output
      pa_range <- range(combinedDFs$pa, na.rm = TRUE)
      log_info("{name}: Success. PA range: {paste(pa_range, collapse = ' to ')}")
      
      write.csv(combinedDFs, 
                row.names = F, 
                file = output_file)
      
      return(pa_range)
    
  }, error = function(e) {
    # 4. CAPTURE THE CATASTROPHIC ERRORS
    # If build_fisheries_df crashes, this block catches it and logs exactly why
    log_error("{name}: CRASHED with error: {e$message}")
    return(NULL) 
  })
  return(result)
}
