

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
  hind_dyn <- calculate_distribution_shift(hindcast, poly_dir = NULL)$metrics
  fore_dyn <- calculate_distribution_shift(forecast, poly_dir = NULL)$metrics
  
  #format timestep and merge 
  hind_dyn$timestamp <- as.Date(paste0("15.", hind_dyn$layer), format = "%d.%m.%Y")
  hind_dyn$year <- lubridate::year(hind_dyn$timestamp)
  fore_dyn$timestamp <- as.Date(paste0("15.", fore_dyn$layer), format = "%d.%m.%Y")
  fore_dyn$year <- lubridate::year(fore_dyn$timestamp)
  
  hind_dyn$data_source <- 'hindcast'
  fore_dyn$data_source <- 'forecast'
  
  all_dyns <- rbind(hind_dyn, fore_dyn)
  
  #create point and color flags
  all_colors <- ifelse(all_dyns$data_source == "hindcast", "grey", "goldenrod4")
  all_shapes <- ifelse(all_dyns$data_source == "hindcast", 19, 17) # 16 = solid circle, 17 = solid triangle
  
  ##calculate dynamics on annual timeseries - we only need metrics again from this one
  #subset
  hind_annual <- terra::tapp(hindcast, index = hind_dyn$year, fun = 'mean', na.rm = T) 
  fore_annual <- terra::tapp(forecast, index = fore_dyn$year, fun = 'mean', na.rm = T)
  
  hind_annual_dyn <- calculate_distribution_shift(hind_annual, poly_dir = NULL)$metrics
  fore_annual_dyn <- calculate_distribution_shift(fore_annual, poly_dir = NULL)$metrics
  
  #format timestamp and merge
  hind_annual_dyn$timestamp <- as.Date(gsub(pattern = 'X', replacement = "15.06.", x = hind_annual_dyn$layer), format = "%d.%m.%Y")
  fore_annual_dyn$timestamp <- as.Date(gsub(pattern = 'X', replacement = "15.06.", x = fore_annual_dyn$layer), format = "%d.%m.%Y")
  
  hind_annual_dyn$data_source <- 'hindcast'
  fore_annual_dyn$data_source <- 'forecast'
  
  annual_dyns <- rbind(hind_annual_dyn, fore_annual_dyn)
  
  #create point and color flags
  annual_colors <- ifelse(annual_dyns$data_source == "hindcast", "black", "goldenrod4")
  annual_shapes <- ifelse(annual_dyns$data_source == "hindcast", 19, 17) # 16 = solid circle, 17 = solid triangle
  
  ##calculate dynamics on first and last 5 years of hindcast
  #create index
  find <- which(hind_dyn$year >= min(hind_dyn$year) & hind_dyn$year <= (min(hind_dyn$year))+4)
  lind <- which(hind_dyn$year >= (max(hind_dyn$year)-4) & hind_dyn$year <= max(hind_dyn$year))
  
  #subset
  hind_first5 <- terra::app(hindcast[[find]], fun = 'mean', na.rm = T) 
  hind_last5 <- terra::app(hindcast[[lind]], fun = 'mean', na.rm = T)
  
  #only do last 5 years for forecast
  lind <- which(fore_dyn$year >= (max(fore_dyn$year)-4) & fore_dyn$year <= max(fore_dyn$year))
  fore_last5 <- terra::app(forecast[[lind]], fun = 'mean', na.rm = T)
  #resample to get to the same extent as the hindcast
  fore_last5 <- terra::resample(fore_last5, hind_last5, method = "bilinear")
  
  fives <- c(hind_first5, hind_last5, fore_last5)
  fives_dyn <- calculate_distribution_shift(fives, poly_dir = NULL)

  
  #plot static
  pdf('SDM_static_plot_test2.pdf', width = 11, height = 8)
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
  
  terra::add_legend(x = -70.1, y = 37.5, 
             pch = c(NA, NA, 21),
             lty = c(1, 3, NA),
             lwd = c(2,2,NA),
             bg = c(NA, NA, 'grey90'),
             legend = c('Core Habitat', 'Species Range', 'Weighted Centroid'),
             cex = 1.5, bty = 'n')
  
  
  text(-71, 35.5, paste0('Centroid Displacement = ', round(fives_dyn$metrics$centroid_step_dist_km[1], digits = 1), ' km ', deg_to_cardinal_16(fives_dyn$metrics$centroid_bearing_deg[1])), adj = 0.5, cex = 1.5, font = 2)
  #needs second line on predicted future shifts
  
  #time series 
  #change in areas through time
  #core
  #monthly
  plot(kde_core_area/1000 ~ timestamp, data = all_dyns, t = 'b', pch = all_shapes, cex = 0.5, lwd = 0.5, col = all_colors, ylab = 'Area (10^3 km2)', cex.axis = 1.25, xlab = '', main = 'Core Habitat Area')
  #annually
  lines(kde_core_area/1000 ~ timestamp, data = annual_dyns, t = 'b', pch = annual_shapes, cex = 1.5, lwd = 1.5, col = annual_colors)
  #linear model
  m <- lm(kde_core_area ~ timestep, data = hind_dyn)
  # Extract key statistics & plot if significant
  p_val     <- summary(m)$coefficients["timestep", "Pr(>|t|)"]
  if(p_val <= 0.05){
    y_pred <- predict(m)
    lines(hind_dyn$timestamp, y_pred/1000, col = "firebrick", lty = 2, lwd = 2)
  }
  text(median(hind_dyn$timestamp), min(hind_dyn$kde_core_area/1000,na.rm=T), paste0(if(hind_annual_dyn$kde_core_area[nrow(hind_annual_dyn)] - hind_annual_dyn$kde_core_area[1] < 0) "Loss" else 'Gain', ' of ', abs(round( hind_annual_dyn$kde_core_area[nrow(hind_annual_dyn)] - hind_annual_dyn$kde_core_area[1], digits = 0)), ' km2'), adj = 0.5, cex = 1.2, font = 2)
  
  #all (95% kde) 
  plot(kde_all_area/1000 ~ timestamp, data = all_dyns, t = 'b', pch = all_shapes, cex = 0.5, lwd = 0.5, col = all_colors, ylab = '', cex.axis = 1.25, xlab = '', main = 'Total Habitat Area')
  lines(kde_all_area/1000 ~ timestamp, data = hind_annual_dyn, t = 'b', pch = annual_shapes, cex = 1.5, lwd = 1.5, col = annual_colors)
  #linear model
  m <- lm(kde_all_area ~ timestep, data = hind_dyn)
  # Extract key statistics & plot if significant
  p_val     <- summary(m)$coefficients["timestep", "Pr(>|t|)"]
  if(p_val <= 0.05){
    y_pred <- predict(m)
    lines(hind_dyn$timestamp, y_pred/1000, col = "firebrick", lty = 2, lwd = 2)
  }
  text(median(hind_dyn$timestamp), min(hind_dyn$kde_all_area/1000,na.rm=T), paste0(if(hind_annual_dyn$kde_all_area[nrow(hind_annual_dyn)] - hind_annual_dyn$kde_all_area[1] < 0) "Loss" else 'Gain', ' of ', abs(round( hind_annual_dyn$kde_all_area[nrow(hind_annual_dyn)] - hind_annual_dyn$kde_all_area[1], digits = 0)), ' km2'), adj = 0.5, cex = 1.2, font = 2)
  
  #change in ranges
  hind_dyn$range_y <- hind_dyn$leading_edge_y - hind_dyn$trailing_edge_y
  hind_annual_dyn$range_y <- hind_annual_dyn$leading_edge_y - hind_annual_dyn$trailing_edge_y
  #latitude
  plot(range_y ~ timestamp, data = hind_dyn, t = 'b', pch = 19, cex = 0.5, lwd = 0.5, col = 'grey', ylab = 'Degrees', cex.axis = 1.25, xlab = 'Year', main = 'Latitude Range')
  lines(range_y ~ timestamp, data = hind_annual_dyn, t = 'b', pch = 19, cex = 1.5, lwd = 1.5, col = 'black')
  #linear model
  m <- lm(range_y ~ timestep, data = hind_dyn)
  # Extract key statistics & plot if significant
  p_val     <- summary(m)$coefficients["timestep", "Pr(>|t|)"]
  if(p_val <= 0.05){
    y_pred <- predict(m)
    lines(hind_dyn$timestamp, y_pred, col = "firebrick", lty = 2, lwd = 2)
  }
  text(median(hind_dyn$timestamp), min(hind_dyn$range_y,na.rm=T), paste0(if(hind_annual_dyn$range_y[nrow(hind_annual_dyn)] - hind_annual_dyn$range_y[1] < 0) "Loss" else 'Gain', ' of ', abs(round( hind_annual_dyn$range_y[nrow(hind_annual_dyn)] - hind_annual_dyn$range_y[1], digits = 2)), ' deg'), adj = 0.5, cex = 1.2, font = 2)
  
  #longitude
  hind_dyn$range_x <- hind_dyn$leading_edge_x - hind_dyn$trailing_edge_x
  hind_annual_dyn$range_x <- hind_annual_dyn$leading_edge_x - hind_annual_dyn$trailing_edge_x
  plot(range_x ~ timestamp, data = hind_dyn, t = 'b', pch = 19, cex = 0.5, lwd = 0.5, col = 'grey', ylab = '', cex.axis = 1.25, xlab = 'Year', main = 'Longitude Range')
  lines(range_x ~ timestamp, data = hind_annual_dyn, t = 'b', pch = 19, cex = 1.5, lwd = 1.5, col = 'black')
  #linear model
  m <- lm(range_x ~ timestep, data = hind_dyn)
  # Extract key statistics & plot if significant
  p_val     <- summary(m)$coefficients["timestep", "Pr(>|t|)"]
  if(p_val <= 0.05){
    y_pred <- predict(m)
    lines(hind_dyn$timestamp, y_pred, col = "firebrick", lty = 2, lwd = 2)
  }
  text(median(hind_dyn$timestamp), min(hind_dyn$range_x,na.rm=T), paste0(if(hind_annual_dyn$range_x[nrow(hind_annual_dyn)] - hind_annual_dyn$range_x[1] < 0) "Loss" else 'Gain', ' of ', abs(round( hind_annual_dyn$range_x[nrow(hind_annual_dyn)] - hind_annual_dyn$range_x[1], digits = 2)), ' deg'), adj = 0.5, cex = 1.2, font = 2)
  
  dev.off()
  
  
}