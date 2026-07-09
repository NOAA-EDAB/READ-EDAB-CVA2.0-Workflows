#' @title Combine Multiple Data.Frames of Fisheries Data
#' @description A wrapper function for the \code{merge_fisheries_df} function. This function adds log and skip functionality to help with batch runs and running \code{merge_fisheries_df} in parallel.
#'
#' @param name Species name to add to log files and save data to correct directory (see vignette for recommended directory set up)
#' @param skip TRUE/FALSE indicating whether to skip creating the raster file if file already exists
#'
#' @return \code{combine_rasters_wrapper} returns the range of the rasterBrick returned by \code{merge_fisheries_rasters}. This should be equal to 0 2, or else there are no presences in the dataset and the models will fail. This function will also save the resulting rasterBrick as a netcdf file in the species' input_rasters folder

combine_fisheries_dfs_wrapper <- function(name, skip) {
  sink(file.path(getwd(), 'logs', 'combineDFs.log'), append = T)
  # Ensure the sinks are closed when the function exits, regardless of how it exits.
  on.exit({
    #sink(type = "message")
    sink()
  })
  print(Sys.time())
  print(name)
  
  if (skip) {
    if (
      file.exists(paste(
        file.path(getwd(), name, 'input_csvs'),
        'combined_pa.csv',
        sep = '/'
      ))
    ) {
      print('file exists and skip == T, so skipping this file!')
      return(NA)
    } else {
      spp_dir <- dir(file.path(getwd(), name, 'input_csvs'), full.names = T)
      
      combinedDFs <- merge_fisheries_dfs(spp_dir)
      print(range(combinedDFs$pa))
      write.csv(combinedDFs, 
                row.names = F, 
                file = paste(spp_dir, 
                             'combined_pa.csv', 
                             sep = '/')
      )
    } #end else
  } else {
    #if skip = F, do it anyway
    
    spp_dir <- dir(file.path(getwd(), name, 'input_csvs'), full.names = T)
    if (
      file.exists(paste(
        file.path(getwd(), name, 'input_csvs'),
        'combined_pa.csv',
        sep = '/'
      ))
    ) {
      i <- which(
        flist ==
          paste(
            file.path(getwd(), name, 'input_csvs'),
            'combined_pa.csv',
            sep = '/'
          )
      )
      spp_dir <- spp_dir[-i]
    }
    combinedDFs <- merge_fisheries_dfs(spp_dir)
    print(range(combinedDFs$pa))
    write.csv(combinedDFs, 
              row.names = F, 
              file = paste(spp_dir, 
                           'combined_pa.csv', 
                           sep = '/')
              )
    #sink()
  } #end else
}
