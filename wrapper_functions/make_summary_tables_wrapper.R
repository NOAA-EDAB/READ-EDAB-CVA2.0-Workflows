#' @title Make Summary Tables
#' @description A wrapper function for \code{make_summary_table} that handles object loading/saving for sensitivity, exposure, and vulnerability tables 
#'
#' @param spp species name. Used to pull correct data and save outputs in species-specific folders.
#' @param forecast_release,hindcast_release MOM6 release codes for the (f)orecast and (h)indcasts used. Used to pull correct variable exposures
#' @param forecast_init forecast_initialization code corresponding to the forecast_initalization date of the desired forecast data. Used to pull correct variable exposures
#' @param hindcast_yr_range character string corresponding to the years in the hindcast data used. Used to pull correct ranked exposure values and save the data properly
#' @param sensitivity_df data.frame with final sensitivity scores containing \code{species_col, stock_col, and total_sens_col}. 
#' @param species_col,stock_col name of column with species and stock names to help subset data in \code{sensitivity}
#' @param total_sens_col name of column containing total sensitivity scores in \code{sensitivity}
#' @param stock_key a named vector containing the abbreviations and long names of stocks to match exposure and sensitivity timeseries 
#'
#' @return Nothing is returned. Datasets are saved in species-specific folders

build_summary_tables_wrapper <- function(
    spp,
    forecast_release,
    forecast_init,
    hindcast_release,
    hindcast_yr_range,
    variable_df,
    sensitivity_dfs,
    species_col,
    stock_col,
    total_sens_col,
    certainty_col,
    stock_key,
    stock_order
) {

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
  
  ### Sensitivity ### 
  # 1. Subset each data.frame by species
  spRaw <- sensitivity_dfs$raw[sensitivity_dfs$raw[, species_col] == spp, ]
  spSens <- sensitivity_dfs$total[sensitivity_dfs$total[, species_col] == spp, ]
  spDQ <- sensitivity_dfs$data.quality[sensitivity_dfs$data.quality[, species_col] == spp, ]
  
  #quick filter to check for NAs
  spRaw <- spRaw[!is.na(spRaw[, species_col]),]
  spSens <- spSens[!is.na(spSens[, species_col]),]
  spDQ <- spDQ[!is.na(spDQ[, species_col]),]
  
  sensTable <- make_summary_table(species = spp,
    metric = 'sensitivity', 
                                 mean_metric = spSens, 
                                 certainty_metric = spDQ, 
                                 raw_data = spRaw, 
                                 raw_names = names(spSens)[4:15], 
                                 clean_names = unique(spRaw$Attribute.Name), 
                                 var_imp = NULL,
                                 stocks = stocks, 
                                 stock_key = stock_key, 
                                 stock_order = stock_order, 
                                 stock_col = stock_col,
                                 total_sens_col = total_sens_col,
                                 certainty_col = certainty_col,
                                 save = T,
                                 table_dir = paste0(here::here('Sensitivity/Final/Summary_Tables')))
  
  ### Exposure ###
  #load variable weights
  var_imp <- readRDS(paste0(
    file.path(here::here('Exposure'), spp, 'Data'),
    '/normalized_dynamic_variable_importance.rds'
  )) 
  ens_imp <- var_imp[nrow(var_imp),]
  
  #load maps
  #variable exposure
  varExp <- terra::rast(paste0(
    file.path(here::here('Exposure'), spp, 'Data'),
    paste0(
      '/variable_exposure_maps_',
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
  
  #load total exposure maps - all vars
  totAll <- terra::rast(paste0(
    file.path(here::here('Exposure'), spp, 'Data'),
    paste0(
      '/total_exposure_map_all_var_',
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
  
  #load total exposure maps - important vars
  totImp <- terra::rast(paste0(
    file.path(here::here('Exposure'), spp, 'Data'),
    paste0(
      '/total_exposure_map_imp_var_',
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
  
  #stack all rasters
  rastStack <- c(varExp, totAll, totImp)
  nl <- terra::nlyr(rastStack)
  names(rastStack)[(nl - 1):nl] <- c('totalAll', 'totalImp')
  
  ##load means and standard deviations
  rastMeans <- readRDS(paste0(
    file.path(here::here('Exposure'), spp, 'Data'),
    paste0(
      '/total_exposure_map_averages_',
      forecast_release,
      '_',
      forecast_init,
      '_',
      hindcast_release,
      '_',
      hindcast_yr_range,
      '.rds'
    )
  ))
  
  rastSD <- readRDS(paste0(
    file.path(here::here('Exposure'), spp, 'Data'),
    paste0(
      '/total_exposure_map_stdevs_',
      forecast_release,
      '_',
      forecast_init,
      '_',
      hindcast_release,
      '_',
      hindcast_yr_range,
      '.rds'
    )
  ))
  
  expTable <- make_summary_table(species = spp, 
                                 metric = 'exposure', 
                                 mean_metric = rastMeans, 
                               certainty_metric = rastSD, 
                               raw_data = rastStack, 
                               raw_names = names(rastStack), 
                               clean_names = c(variable_df$Long.Name[variable_df$Short.Name %in% names(rastStack)],
                                               "Total Exposure", 
                                               "Total Exposure - Important Variables"), 
                               var_imp = ens_imp,
                               stocks = stocks, 
                               stock_key = stock_key, 
                               stock_order = stock_order, 
                               stock_col = stock_col,
                               total_sens_col = NA,
                               certainty_col = NA,
                               imp_threshold = 0.1,
                               save = T,
                               table_dir = paste0(here::here('Exposure'), '/', spp, '/Figures'))
  
  ### Vulnerability ###
##load means and standard deviations
rastMeans <- readRDS(paste0(
  file.path(here::here('Vulnerability'), spp, 'Data'),
  paste0(
    '/total_vulnerability_map_averages_',
    forecast_release,
    '_',
    forecast_init,
    '_',
    hindcast_release,
    '_',
    hindcast_yr_range,
    '.rds'
  )
))


rastSD <- readRDS(paste0(
  file.path(here::here('Vulnerability'), spp, 'Data'),
  paste0(
    '/total_vulnerability_map_stdevs_',
    forecast_release,
    '_',
    forecast_init,
    '_',
    hindcast_release,
    '_',
    hindcast_yr_range,
    '.rds'
  )
))

#make sure everything is a data.frame
rastMeans <- as.data.frame(rastMeans)
rastSD <- as.data.frame(rastSD)

#load in maps
rasts <- terra::rast(paste0(
  file.path(here::here('Vulnerability'), spp, 'Data'),
  paste0(
    '/vulnerability_',
    forecast_release,
    '_',
    forecast_init,
    '_',
    hindcast_release,
    '_',
    hindcast_yr_range,
    '_scaled.tif'
  )
)
)

vTable <- make_summary_table(species = spp, 
                             metric = 'vulnerability', 
                             mean_metric = rastMeans, 
                   certainty_metric = rastSD, 
                   raw_data = rasts, 
                   raw_names = names(rasts), 
                   clean_names = c("Total Vulnerability", 
                                   "Total Vulnerability - Important Variables"), 
                   stocks = stocks, 
                   stock_key = stock_key, 
                   stock_order = ns_stocks, 
                   stock_col = stock_col,
                   total_sens_col = NA,
                   certainty_col = NA,
                   var_imp = NA,
                   save = T,
                   table_dir = paste0(here::here('Vulnerability'), '/', spp, '/Figures'))


### combine the images 
# 1. Read the saved images
img_exp <- magick::image_read(paste0(here::here('Exposure'), 
                                     '/', spp, '/Figures/exposure_table_', spp, '.png'))

img_sens <- magick::image_read(paste0(here::here('Sensitivity/Final/Summary_Tables'), '/sensitivity_table_', spp, '.png'))

img_vuln <- magick::image_read(paste0(here::here('Vulnerability'), 
                                     '/', spp, '/Figures/vulnerability_table_', spp, '.png'))

# 2. Trim the baked-in white margins off all sides
img_exp <- magick::image_trim(img_exp)
img_sens <- magick::image_trim(img_sens)
img_vuln <- magick::image_trim(img_vuln)

# 3. Stack them vertically (stack = TRUE) & save
stacked_img <- magick::image_append(c(img_sens, img_exp, img_vuln), stack = TRUE)
magick::image_write(stacked_img, paste0(here::here('Vulnerability'), 
                                        '/', spp, '/Figures/combined_table_', spp, '.png'))

} #end function