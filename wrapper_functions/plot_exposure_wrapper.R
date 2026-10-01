#' @title Plot Exposure Results
#' @description A wrapper function for \code{plot_variable_map_timeseries} , \code{plot_total_map_timeseries} that handles object loading, produces figures for variable-level exposure, total exposure
#'
#' @param spp species name. Used to pull correct data and save outputs in species-specific folders.
#' @param forecast_release,hindcast_release MOM6 release codes for the (f)orecast and (h)indcasts used. Used to pull correct variable exposures
#' @param forecast_init forecast_initialization code corresponding to the forecast_initalization date of the desired forecast data. Used to pull correct variable exposures
#' @param hindcast_yr_range character string corresponding to the years in the hindcast data used. Used to pull correct ranked exposure values and save the data properly
#' @param stock_key a named vector containing the abbreviations and long names of stocks to match exposure and sensitivity timeseries 
#' @param coastline shapefile used to plot land in model prediction plots
#' @param bathymetry spatRaster file of bathymetry data; used to plot bathymetry in stock boundary plots
#'
#' @return no returns; figures saved in desired folder 
#' 
plot_exposure_wrapper <- function(
    spp,
    forecast_release,
    forecast_init,
    hindcast_release,
    hindcast_yr_range,
    variable_df,
    stock_key,
    coastline, 
    bathymetry) {
  
  
  #### 1) Variable Exposures
  #### load in data 
  
  #load stocks if available
  if (file.exists(paste0('../shpfiles/species_stock_areas/', spp, '.shp'))) {
    stocks <- terra::vect(paste0(
      '../shpfiles/species_stock_areas/',
      spp,
      '.shp'
    ))
  } else {
    stocks <- NULL
  }
  
  #load maps
  varMaps <- terra::rast(file.path(
    here::here("Exposure"),
    spp,
    'Data',
    paste0(
      'variable_exposure_maps_',
      forecast_release,
      '_',
      forecast_init,
      '_',
      hindcast_release,
      '_',
      hindcast_yr_range,
      '.tif'
    )
  ))

  
  #plot maps 
  plot_variable_map_timeseries(data = varMaps,
                               stocks = stocks,
                               stock_key = stock_key,
                               coastline = coastline, 
                               bathymetry = bathymetry,
                               variable_df = variable_df,
                               fig_name = paste0(
                                 file.path(
                                   here::here("Exposure"),
                                   spp,
                                   'Figures',
                                   paste0(
                                     'variable_exposure_maps_',
                                     forecast_release,
                                     '_',
                                     forecast_init,
                                     '_',
                                     hindcast_release,
                                     '_',
                                     hindcast_yr_range,
                                     '.pdf'
                                   )
                                 )
                               ))
  
  #timeseries
  #load timeseries
  vecExp <- readRDS(
    file.path(
      here::here("Exposure"),
      spp,
      'Data',
      paste0(
        'variable_exposure_timeseries_',
        forecast_release,
        '_',
        forecast_init,
        '_',
        hindcast_release,
        '_',
        hindcast_yr_range,
        '.rds'
      )
    )
  )
  
  #plot timeseries 
  plot_variable_map_timeseries(data = vecExp,
                               stocks = stocks,
                               stock_key = stock_key,
                               coastline = coastline, 
                               bathymetry = bathymetry,
                               variable_df = variable_df,
                               fig_name = paste0(
                                 file.path(
                                   here::here("Exposure"),
                                   spp,
                                   'Figures',
                                   paste0(
                                     'variable_exposure_timeseries_',
                                     forecast_release,
                                     '_',
                                     forecast_init,
                                     '_',
                                     hindcast_release,
                                     '_',
                                     hindcast_yr_range,
                                     '.pdf'
                                   )
                                 )
                               ))
  
  ##2 total exposure 
  #load maps
  expMaps <- terra::rast(file.path(
    here::here("Exposure"),
    spp,
    'Data',
    paste0(
      'total_exposure_map_all_var_',
      forecast_release,
      '_',
      forecast_init,
      '_',
      hindcast_release,
      '_',
      hindcast_yr_range,
      '.tif'
    )
  ))
  
  #load timeseries
  vecExp <- readRDS(
    file.path(
      here::here("Exposure"),
      spp,
      'Data',
      paste0(
        'total_exposure_timeseries_all_var_',
        forecast_release,
        '_',
        forecast_init,
        '_',
        hindcast_release,
        '_',
        hindcast_yr_range,
        '.rds'
      )
    )
  )
  if (!is.null(nrow(vecExp))) {
    rownames(vecExp)[1] <- 'All Stocks'
  }
  
  #plot for all variables 
  plot_total_map_timeseries(map = expMaps,
                            timeseries = vecExp, 
                            stocks = stocks, 
                            stock_key = stock_key,
                            metric = 'exposure',
                            coastline = coastline, 
                            bathymetry = bathymetry,
                            fig_name = paste0(
                              file.path(here::here('Exposure'), spp, 'Figures'),
                              paste0(
                                '/total_exposure_map_timeseries_all_var_',
                                forecast_release,
                                '_',
                                forecast_init,
                                '_',
                                hindcast_release,
                                '_',
                                hindcast_yr_range,
                                '.pdf'
                              )
                            ))
  
  #plot just map
  plot_total_map_timeseries(map = expMaps,
                            timeseries = NULL, 
                            stocks = stocks, 
                            stock_key = stock_key,
                            metric = 'exposure',
                            coastline = coastline, 
                            bathymetry = bathymetry,
                            fig_name = paste0(
                              file.path(here::here('Exposure'), spp, 'Figures'),
                              paste0(
                                '/total_exposure_map_only_all_var_',
                                forecast_release,
                                '_',
                                forecast_init,
                                '_',
                                hindcast_release,
                                '_',
                                hindcast_yr_range,
                                '.pdf'
                              )
                            ))
  
  ##important variables 
  #load maps
  expMaps <- terra::rast(file.path(
    here::here("Exposure"),
    spp,
    'Data',
    paste0(
      'total_exposure_map_imp_var_',
      forecast_release,
      '_',
      forecast_init,
      '_',
      hindcast_release,
      '_',
      hindcast_yr_range,
      '.tif'
    )
  ))
  
  #load timeseries
  vecExp <- readRDS(
    file.path(
      here::here("Exposure"),
      spp,
      'Data',
      paste0(
        'total_exposure_timeseries_imp_var_',
        forecast_release,
        '_',
        forecast_init,
        '_',
        hindcast_release,
        '_',
        hindcast_yr_range,
        '.rds'
      )
    )
  )
  if (!is.null(nrow(vecExp))) {
    rownames(vecExp)[1] <- 'All Stocks'
  }
  #plot for all variables 
  plot_total_map_timeseries(map = expMaps,
                            timeseries = vecExp, 
                            stocks = stocks, 
                            stock_key = stock_key,
                            metric = 'exposure',
                            coastline = coastline, 
                            bathymetry = bathymetry,
                            fig_name = paste0(
                              file.path(here::here('Exposure'), spp, 'Figures'),
                              paste0(
                                '/total_exposure_map_timeseries_imp_var_',
                                forecast_release,
                                '_',
                                forecast_init,
                                '_',
                                hindcast_release,
                                '_',
                                hindcast_yr_range,
                                '.pdf'
                              )
                            ))
  
  #plot jusst map
  plot_total_map_timeseries(map = expMaps,
                            timeseries = NULL, 
                            stocks = stocks, 
                            stock_key = stock_key,
                            metric = 'exposure',
                            coastline = coastline, 
                            bathymetry = bathymetry,
                            fig_name = paste0(
                              file.path(here::here('Exposure'), spp, 'Figures'),
                              paste0(
                                '/total_exposure_map_only_imp_var_',
                                forecast_release,
                                '_',
                                forecast_init,
                                '_',
                                hindcast_release,
                                '_',
                                hindcast_yr_range,
                                '.pdf'
                              )
                            ))
  
  ####3 Plot Dynamic Variable Importance
  imp <- readRDS(
    file.path(
      here::here('Exposure'), 
      spp,
      'Data',
      'normalized_dynamic_variable_importance.rds'
    )
  )
  
  plot_variable_radars(variable_importance = imp, 
                       fig_name = paste0(
    file.path(here::here('Exposure'), spp, 'Figures'),
    paste0(
      '/dynamic_variable_importance_all.pdf'
    )
  ))
}
