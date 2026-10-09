

distribution_shifts_wrapper <- function(spp,
                                        model,
                                        hindcast_release,
                                        forecast_release,
                                        forecast_init,
                                        spatial_temporal,
                                        mask_bathy,
                                        bathymetry,
                                        coastline){
  
  #suffixes to help locate correct data
  suffix <- if (spatial_temporal) "" else "_global"
  bathy_suffix <- if (mask_bathy) "masked" else ""
  
  
  #load in rasters
  hindcast <- terra::rast(
    file.path(
      here::here('SDMs'), 
      spp,
      'output_rasters',
      paste0(toupper(model), '_hindcast_', hindcast_release, '_', bathy_suffix, suffix,
      '.tif')
    ))
  
  forecast <- terra::rast(
    file.path(
      here::here('SDMs'), 
      spp,
      'output_rasters',
      paste0(toupper(model), '_forecast_', forecast_release, '_', forecast_init,
             '.tif')
    ))
  
  forecast <- terra::resample(forecast, hindcast[[1]], method = "bilinear")
    
  #calculate dynamics on raw timeseries - we only need metrics from this one 
  hind_dyn <- calculate_distribution_shift(hindcast, poly_dir = file.path(
    here::here('AdditionalMetrics/DistributionChange'), 
    spp,
    'monthly_polys',
    'hindcast'
  ))
  fore_dyn <- calculate_distribution_shift(forecast, poly_dir = file.path(
    here::here('AdditionalMetrics/DistributionChange'), 
    spp,
    'monthly_polys',
    'forecast'
  ))
  
  hind_dyn$metrics$year <- lubridate::year(as.Date(paste0("15.", hind_dyn$metrics$layer), format = "%d.%m.%Y"))
  fore_dyn$metrics$year <- lubridate::year(as.Date(paste0("15.", fore_dyn$metrics$layer), format = "%d.%m.%Y"))
  
  #merge data for gif
  monthly_dyn <- rbind(hind_dyn$metrics, fore_dyn$metrics)
  monthly_dyn$timestamp <- as.Date(paste0("15.", monthly_dyn$layer), format = "%d.%m.%Y")
  
  monthly_core <- rbind(hind_dyn$core_polygons, fore_dyn$core_polygons)
  monthly_range <- rbind(hind_dyn$all_polygons, fore_dyn$all_polygons)
  
  ##calculate dynamics on annual timeseries
  #subset
  hind_annual <- terra::tapp(hindcast, index = hind_dyn$metrics$year, fun = 'mean', na.rm = T) 
  fore_annual <- terra::tapp(forecast, index = fore_dyn$metrics$year, fun = 'mean', na.rm = T)
  
  hind_annual_dyn <- calculate_distribution_shift(hind_annual, poly_dir = file.path(
    here::here('AdditionalMetrics/DistributionChange'), 
    spp,
    'annual_avgs',
    'hindcast'
  ))
  fore_annual_dyn <- calculate_distribution_shift(fore_annual, poly_dir = file.path(
    here::here('AdditionalMetrics/DistributionChange'), 
    spp,
    'annual_avgs',
    'forecast'
  ))
  
  #isolate metrics 
  hind_annual_dyn <- hind_annual_dyn$metrics
  fore_annual_dyn <- fore_annual_dyn$metrics
  
  #format timestamp 
  hind_annual_dyn$timestamp <- as.Date(gsub(pattern = 'X', replacement = "15.06.", x = hind_annual_dyn$layer), format = "%d.%m.%Y")
  fore_annual_dyn$timestamp <- as.Date(gsub(pattern = 'X', replacement = "15.06.", x = fore_annual_dyn$layer), format = "%d.%m.%Y")

  ##calculate dynamics on first and last 5 years of hindcast
  #create index
  find <- which(hind_dyn$metrics$year >= min(hind_dyn$metrics$year) & hind_dyn$metrics$year <= (min(hind_dyn$metrics$year)+4))
  lind <- which(hind_dyn$metrics$year >= (max(hind_dyn$metrics$year)-4) & hind_dyn$metrics$year <= max(hind_dyn$metrics$year))
  
  #subset
  hind_first5 <- terra::app(hindcast[[find]], fun = 'mean', na.rm = T) 
  hind_last5 <- terra::app(hindcast[[lind]], fun = 'mean', na.rm = T)
  
  #only do last 5 years for forecast
  lind <- which(fore_dyn$metrics$year >= (max(fore_dyn$metrics$year)-4) & fore_dyn$metrics$year <= max(fore_dyn$metrics$year))
  fore_last5 <- terra::app(forecast[[lind]], fun = 'mean', na.rm = T)
  #resample to get to the same extent as the hindcast
  fore_last5 <- terra::resample(fore_last5, hind_last5, method = "bilinear")
  
  fives <- c(hind_first5, hind_last5, fore_last5)
  fives_dyn <- calculate_distribution_shift(fives, poly_dir = file.path(
    here::here('AdditionalMetrics/DistributionChange'), 
    spp,
    'five_year_avgs'
  ))
  
  #clean up metrics and save 
  mets <- fives_dyn$metrics
  mets$range_y <- mets$leading_edge_y - mets$trailing_edge_y
  mets$range_x <- mets$leading_edge_x - mets$trailing_edge_x
  
  mets <- mets[,-1]
  mets$timestep <- c('1993-1997', '2019-2023', '2031-2035')
  
  pals <- data.frame(core = c('blue4', 'firebrick4', 'goldenrod4'), range = c('blue', 'firebrick', 'goldenrod'))

  #generate static plots 
  plot_distribution_shifts(
    plot_name = file.path(
      here::here('AdditionalMetrics/DistributionChange'), 
      spp,
      'distribution_shifts_static.pdf'
    ),
    avg_core_polys = fives_dyn$core_polygons,
    avg_range_polys = fives_dyn$all_polygons,
    poly_pals = pals,
    avg_metrics = mets,
    hindcast_timeseries =hind_annual_dyn,
    forecast_timeseries = fore_annual_dyn,
    bathymetry = bathymetry, coastline = coastline)
  
  #make gifs 
  plot_distribution_shift_drivers(plot_name = file.path(
    here::here('AdditionalMetrics/DistributionChange'), 
    spp,
    'distribution_shift_drivers.gif'
  ),
  sdm = c(hindcast, forecast),
  core_polys = monthly_core,
  range_polys = monthly_range,
  metrics = monthly_dyn,
  bathymetry = bathymetry, coastline = coastline
  )
  
  return(mets)
}