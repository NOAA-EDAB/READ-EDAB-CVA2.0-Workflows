

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
    
  #calculate dynamics on raw timeseries - we only need metrics from this one 
  hind_dyn <- calculate_distribution_shift(hindcast, poly_dir = file.path(
    here::here('AdditionalMetrics/DistributionChange'), 
    spp,
    'monthly_polys'
  ))
  fore_dyn <- calculate_distribution_shift(forecast, poly_dir = file.path(
    here::here('AdditionalMetrics/DistributionChange'), 
    spp,
    'monthly_polys'
  ))
  
  hind_dyn$metrics$year <- lubridate::year(as.Date(paste0("15.", hind_dyn$metrics$layer), format = "%d.%m.%Y"))
  fore_dyn$metrics$year <- lubridate::year(as.Date(paste0("15.", fore_dyn$metrics$layer), format = "%d.%m.%Y"))
  
  
  ##calculate dynamics on annual timeseries
  #subset
  hind_annual <- terra::tapp(hindcast, index = hind_dyn$metrics$year, fun = 'mean', na.rm = T) 
  fore_annual <- terra::tapp(forecast, index = fore_dyn$metrics$year, fun = 'mean', na.rm = T)
  
  hind_annual_dyn <- calculate_distribution_shift(hind_annual, poly_dir = file.path(
    here::here('AdditionalMetrics/DistributionChange'), 
    spp,
    'annual_avgs'
  ))
  fore_annual_dyn <- calculate_distribution_shift(fore_annual, poly_dir = file.path(
    here::here('AdditionalMetrics/DistributionChange'), 
    spp,
    'annual_avgs'
  ))
  
  #isolate metrics 
  hind_annual_dyn <- hind_annual_dyn$metrics
  fore_annual_dyn <- fore_annual_dyn$metrics
  
  #format timestamp and merge
  hind_annual_dyn$timestamp <- as.Date(gsub(pattern = 'X', replacement = "15.06.", x = hind_annual_dyn$layer), format = "%d.%m.%Y")
  fore_annual_dyn$timestamp <- as.Date(gsub(pattern = 'X', replacement = "15.06.", x = fore_annual_dyn$layer), format = "%d.%m.%Y")
  
  hind_annual_dyn$data_source <- 'hindcast'
  fore_annual_dyn$data_source <- 'forecast'
  
  annual_dyns <- rbind(hind_annual_dyn, fore_annual_dyn)
  
  
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

  
  #plot static
  pdf(file.path(
    here::here('AdditionalMetrics/DistributionChange'), 
    spp,
    'distribution_shifts_static.pdf'
  ),
   width = 12, height = 6)
  ###all together
  layout(matrix(c(1,1,1,1,2,4,3,5), byrow=F, nrow = 2, ncol = 4), heights = c(3,3), widths = c(1,1,1,1))
  par(mar=c(4,4,1,1), oma = c(0,0,3,0))
  
  #map of habitat shifts - using KDE polygons rather than change in probability 
  plot(hindcast[[1]], #initalize empty plot 
       pax = list(cex.axis = 2, cex.lab = 1.25,
                  yat = seq(35,45,by=1),
                  retro = T),
       mar = c(3.1, 3.1, 2.1, 2.1), # Remove outer right margin space
       xlab = '', 
       ylab = '',
       col = 'white',
       legend = F)
  
  #bathy contours
  terra::contour(bathymetry, filled = F, levels = c(-1000, -100, -50), add = T)
  
  #Add polygons 
  #first five of hindcast
  plot(fives_dyn$core_polygons[1], add = T, border = scales::alpha('blue4', 0.75), lwd = 3, col = NA)
  plot(fives_dyn$all_polygons[1], add = T, border = scales::alpha('blue1', 0.5), lwd = 3, lty = 3, col = NA)
  
  #last five of hindcast
  plot(fives_dyn$core_polygons[2], add = T, border = scales::alpha('firebrick4', 0.75), lwd = 3, col = NA)
  plot(fives_dyn$all_polygons[2], add = T, border = scales::alpha('firebrick1', 0.5), lwd = 3, lty = 3, col = NA)
  
  #last five of forecast
  plot(fives_dyn$core_polygons[3], add = T, border = scales::alpha('goldenrod4', 0.75), lwd = 3, col = NA)
  plot(fives_dyn$all_polygons[3], add = T, border = scales::alpha('goldenrod', 0.5), lwd = 3, lty = 3, col = NA)
  
  plot(coastline, add = T, col = 'grey25')
  points(centroid_y ~ centroid_x, data = fives_dyn$metrics, pch = 21, bg = c(scales::alpha('blue4', 0.75), scales::alpha('firebrick4', 0.75), scales::alpha('goldenrod4', 0.75)), col = 'black', cex = 2)
  
  terra::add_legend(x = -70, y = 39.5,  
             fill = c('blue4', 'firebrick4', 'goldenrod4'),
             legend = c("1993-1997", "2019-2023", '2031-2035'),
             cex = 1.5, bty = 'n')
  
  terra::add_legend(x = -70.2, y = 38, 
             pch = c(NA, NA, 21),
             lty = c(1, 3, NA),
             lwd = c(2,2,NA),
             bg = c(NA, NA, 'grey90'),
             legend = c('Core Habitat', 'Species Range', 'Weighted Centroid'),
             cex = 1.5, bty = 'n')
  
  #time series 
  #change in areas through time
  #core
  #monthly hindcast 
  plot(kde_core_area/1000 ~ timestamp, data = hind_annual_dyn, t = 'b', pch = 19, cex = 1.5, lwd = 1.5, col = 'black', ylab = 'Area (10^3 km2)', cex.axis = 1.25, xlab = '', main = 'Core Habitat Area', xlim = range(annual_dyns$timestamp), ylim = range(annual_dyns$kde_core_area/1000))
  #annual hindcast
  #lines(kde_core_area/1000 ~ timestamp, data = hind_annual_dyn, t = 'b', pch = 19, cex = 1.5, lwd = 1.5, col = 'black')
  #linear model
  m <- lm(kde_core_area ~ timestep, data = hind_annual_dyn)
  # Extract key statistics & plot if significant
  p_val     <- summary(m)$coefficients["timestep", "Pr(>|t|)"]
  if(p_val <= 0.05){
    y_pred <- predict(m)
    lines(hind_annual_dyn$timestamp, y_pred/1000, col = "firebrick", lty = 2, lwd = 2)
  }
  #repeat for forecast
  #monthly 
  lines(kde_core_area/1000 ~ timestamp, data = fore_annual_dyn, t = 'b', pch = 17, cex = 1.5, lwd = 1.5, col = 'goldenrod4', ylab = 'Area (10^3 km2)', cex.axis = 1.25, xlab = '')
  #annual 
  #lines(kde_core_area/1000 ~ timestamp, data = fore_annual_dyn, t = 'b', pch = 17, cex = 1.5, lwd = 1.5, col = 'goldenrod4')
  #linear model
  m <- lm(kde_core_area ~ timestep, data = fore_annual_dyn)
  # Extract key statistics & plot if significant
  p_val     <- summary(m)$coefficients["timestep", "Pr(>|t|)"]
  if(p_val <= 0.05){
    y_pred <- predict(m)
    lines(fore_annual_dyn$timestamp, y_pred/1000, col = "blue4", lty = 2, lwd = 2)
  }
  
  #all (95% kde)   
  #monthly hindcast 
  plot(kde_all_area/1000 ~ timestamp, data = hind_annual_dyn, t = 'b', pch = 19, cex = 1.5, lwd = 1.5, col = 'black', ylab = 'Area (10^3 km2)', cex.axis = 1.25, xlab = '', main = 'Habitat Range Area', xlim = range(annual_dyns$timestamp), ylim = range(annual_dyns$kde_all_area/1000))
  #annual hindcast
 # lines(kde_all_area/1000 ~ timestamp, data = hind_annual_dyn, t = 'b', pch = 19, cex = 1.5, lwd = 1.5, col = 'black')
  #linear model
  m <- lm(kde_all_area ~ timestep, data = hind_annual_dyn)
  # Extract key statistics & plot if significant
  p_val     <- summary(m)$coefficients["timestep", "Pr(>|t|)"]
  if(p_val <= 0.05){
    y_pred <- predict(m)
    lines(hind_annual_dyn$timestamp, y_pred/1000, col = "firebrick", lty = 2, lwd = 2)
  }
  #repeat for forecast
  #monthly 
  lines(kde_all_area/1000 ~ timestamp, data = fore_annual_dyn, t = 'b', pch = 17, cex = 1.5, lwd = 1.5, col = 'goldenrod4', ylab = 'Area (10^3 km2)', cex.axis = 1.25, xlab = '')
  #annual 
 # lines(kde_all_area/1000 ~ timestamp, data = fore_annual_dyn, t = 'b', pch = 17, cex = 1.5, lwd = 1.5, col = 'goldenrod4')
  #linear model
  m <- lm(kde_all_area ~ timestep, data = fore_annual_dyn)
  # Extract key statistics & plot if significant
  p_val     <- summary(m)$coefficients["timestep", "Pr(>|t|)"]
  if(p_val <= 0.05){
    y_pred <- predict(m)
    lines(fore_annual_dyn$timestamp, y_pred/1000, col = "blue4", lty = 2, lwd = 2)
  }
  
  #change in ranges

  #latitude
  #hindcast
  plot(range_y_deg ~ timestamp, data = hind_annual_dyn, t = 'b', pch = 19, cex = 1.5, lwd = 1.5, col = 'black', ylab = 'Degrees', cex.axis = 1.25, xlab = 'Year', main = 'Latitude Range', xlim = range(hind_annual_dyn$timestamp), ylim = range(annual_dyns$range_y))
 # lines(range_y ~ timestamp, data = hind_annual_dyn, t = 'b', pch = 19, cex = 1.5, lwd = 1.5, col = 'black')
  #linear model
  m <- lm(range_y_deg ~ timestep, data = hind_annual_dyn)
  # Extract key statistics & plot if significant
  p_val     <- summary(m)$coefficients["timestep", "Pr(>|t|)"]
  if(p_val <= 0.05){
    y_pred <- predict(m)
    lines(hind_annual_dyn$timestamp, y_pred, col = "firebrick", lty = 2, lwd = 2)
  }
  #repeat for forecast
  #monthly 
  lines(range_y_deg~ timestamp, data = fore_annual_dyn, t = 'b', pch = 17, cex = 1.5, lwd = 1.5, col = 'goldenrod4', ylab = 'Degrees', cex.axis = 1.25, xlab = '')
  #annual 
 # lines(range_y ~ timestamp, data = fore_annual_dyn, t = 'b', pch = 17, cex = 1.5, lwd = 1.5, col = 'goldenrod4')
  #linear model
  m <- lm(range_y_deg ~ timestep, data = fore_annual_dyn)
  # Extract key statistics & plot if significant
  p_val     <- summary(m)$coefficients["timestep", "Pr(>|t|)"]
  if(p_val <= 0.05){
    y_pred <- predict(m)
    lines(fore_annual_dyn$timestamp, y_pred, col = "blue4", lty = 2, lwd = 2)
  }
  
  #longitude
  
  plot(range_x_deg ~ timestamp, data = hind_annual_dyn, t = 'b', pch = 19, cex = 1.5, lwd = 1.5, col = 'black', ylab = '', cex.axis = 1.25, xlab = 'Year', main = 'Longitude Range', xlim = range(annual_dyns$timestamp), ylim = range(annual_dyns$range_x))
 # lines(range_x ~ timestamp, data = hind_annual_dyn, t = 'b', pch = 19, cex = 1.5, lwd = 1.5, col = 'black')
  #linear model
  m <- lm(range_x_deg ~ timestep, data = hind_annual_dyn)
  # Extract key statistics & plot if significant
  p_val     <- summary(m)$coefficients["timestep", "Pr(>|t|)"]
  if(p_val <= 0.05){
    y_pred <- predict(m)
    lines(hind_annual_dyn$timestamp, y_pred, col = "firebrick", lty = 2, lwd = 2)
  }
  #repeat for forecast
  #monthly 
  lines(range_x_deg~ timestamp, data = fore_annual_dyn, t = 'b', pch = 17, cex = 1.5, lwd = 1.5, col = 'goldenrod4', ylab = 'Degrees', cex.axis = 1.25, xlab = '')
  #annual 
 # lines(range_x ~ timestamp, data = fore_annual_dyn, t = 'b', pch = 17, cex = 1.5, lwd = 1.5, col = 'goldenrod4')
  #linear model
  m <- lm(range_x_deg ~ timestep, data = fore_annual_dyn)
  # Extract key statistics & plot if significant
  p_val     <- summary(m)$coefficients["timestep", "Pr(>|t|)"]
  if(p_val <= 0.05){
    y_pred <- predict(m)
    lines(fore_annual_dyn$timestamp, y_pred, col = "blue4", lty = 2, lwd = 2)
  }
  
  dev.off()
  
  return(mets)
}