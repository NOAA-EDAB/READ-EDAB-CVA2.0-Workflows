#' @title Make Timeseries and Maps of Total Exposure
#' @description A wrapper function for \code{make_total_exposure} that handles object loading, produces both maps and timeseries, and averages total spatial maps globally and within stock polygons if desired. automatically does calculation both with all variables and with only the important variables
#'
#' @param spp species name. Used to pull correct data and save outputs in species-specific folders.
#' @param forecast_release,hindcast_release MOM6 release codes for the (f)orecast and (h)indcasts used. Used to pull correct variable exposures
#' @param forecast_init forecast_initialization code corresponding to the forecast_initalization date of the desired forecast data. Used to pull correct variable exposures
#' @param hindcast_yr_range character string corresponding to the years in the hindcast data used. Used to pull correct ranked exposure values and save the data properly
#'
#' @return returns a data.frame containing the spatial averages of total exposure, plus variable exposure across the entire domain, and within stock polygons if shpfiles exists. The outputs from \code{make_total_exposure(type = 'map')} and \code{make_total_exposure(type = 'timeseries')} are saved in the appropriate folders

total_exposures_wrapper <- function(
    spp,
    forecast_release, 
    forecast_init,
    hindcast_release, 
    hindcast_yr_range
) {

  # ==========================================================
  # STEP 0: Set Up
  # ==========================================================
  # Set up the logger to output to your specific file
  log_file <- file.path(getwd(), 'logs', 'total_exposure.log')
  log_appender(appender_file(log_file))

  log_info("Calculating total exposures for {spp}")

  # ==========================================================
  # STEP 1: Load in Data
  # ==========================================================
  #ensemble weights
  load(file.path('../SDMs/', spp, 'model_output',
            'ensemble_weights.rds')) #weights

  #variable maps
  mapExp <- terra::rast(paste0(
    file.path(getwd(), spp, 'Data'),
    paste0('/variable_exposure_maps_', forecast_release, '_', forecast_init, '_', hindcast_release,'_',hindcast_yr_range, '.tif')
    )
  ) # mapExp

  #variable timeseries
  vecExp <- readRDS(
    file = paste0(
      file.path(getwd(), spp, 'Data'),
      paste0('/variable_exposure_timeseries_', forecast_release, '_', forecast_init, '_', hindcast_release,'_',hindcast_yr_range, '.rds')
    )
  ) #vecExp

  #calculate normalized variable importance
    #read in variable importance outputs & create list
    flist <- dir(
      file.path('../SDMs/', spp, 'model_output/importance'),
      full.names = T,
      pattern = 'rds'
    )
    imp_list <- vector(mode = 'list', length = length(flist))
    for(x in 1:length(flist)){
      load(flist[x])
      imp_list[[x]] <- imp
    }
    names(imp_list) <- gsub('.rds', '', dir(
      file.path('../SDMs/', spp, 'model_output/importance'),
      full.names = F,
      pattern = 'rds'
    ))

    #pull variable names from mapexp
    dyn_vars <- names(mapExp)

    #create variable importance
    var_imp <- normalize_variable_importance(vars = dyn_vars, ens_weights = weights, imp_list = imp_list)

    #save
    saveRDS(var_imp, file = file.path(getwd(), spp, 'Data',
                                   'normalized_dynamic_variable_importance.rds'))

  # ==========================================================
  # STEP 2: Calculate Exposures Across Space
  # ==========================================================

  #with all variables
  mapTot <- make_total_exposure(
    type = 'map',
    variable_exposure = mapExp,
    count_all = T,
    variable_weights = NA,
    weight_threshold = NA
  )
  terra::writeRaster(
   x = mapTot,
    filename = paste0(
      file.path(getwd(), spp, 'Data'),
      paste0('/total_exposure_map_all_var_', forecast_release, '_', forecast_init, '_', hindcast_release,'_',hindcast_yr_range,'.tif')
    ),
   overwrite = TRUE
  )

  #with important variables
  mapImp <- make_total_exposure(
    type = 'map',
    variable_exposure = mapExp,
    count_all = F,
    variable_weights = var_imp[nrow(var_imp),match(names(mapExp), colnames(var_imp))], #last row is always the weighted average of the ensemble. reordered to match the names of the variable exposure rasters
    weight_threshold = 0.1
  )
  terra::writeRaster(
    x = mapImp,
    filename = paste0(
      file.path(getwd(), spp, 'Data'),
      paste0('/total_exposure_map_imp_var_', forecast_release, '_', forecast_init, '_', hindcast_release,'_',hindcast_yr_range,'.tif')
    ),
    overwrite = TRUE
  )

  log_info('spatial total exposures for {spp} complete.')

  # ==========================================================
  # STEP 3: Average Exposures Across Space within Stock Regions
  # ==========================================================
  #add names and combine rasts
  names(mapTot) <- 'totalAll'
  names(mapImp) <- 'totalImp'
  allRasts <- c(mapTot, mapImp, mapExp)

  #calculate global means
  globals <- terra::global(allRasts, fun = 'mean', na.rm = T)

  #if stock polygons exist for the species, calculate averages within stocks
  if(file.exists(paste0('../shpfiles/species_stock_areas/', spp, '.shp'))){
    stocks <- terra::vect(paste0('../shpfiles/species_stock_areas/', spp, '.shp'))
    #average within polygons
  expAvg <- terra::extract(allRasts, stocks, fun = 'mean', na.rm = T)

    #combine
  expAvg <- rbind(t(globals), expAvg[,-1])
  #add stock column
  expAvg$stock <- c('global', stocks$stock_area)

  } else {
    log_info('No stock shpfiles found for {spp}. Only calculating global total exposure from maps')
    expAvg <- t(globals)
  }

  saveRDS(
    expAvg,
    file = paste0(
      file.path(getwd(), spp, 'Data'),
      paste0('/total_exposure_map_averages_' , forecast_release, '_', forecast_init, '_', hindcast_release,'_',hindcast_yr_range, '.rds')
    )
  )
  # ==========================================================
  # STEP 4: Calculate Exposures Across Time
  # ==========================================================

  #timeseries - all variables
  vecAll <- make_total_exposure(
    type = 'timeseries',
    variable_exposure = vecExp,
    count_all = T,
    variable_weights = NA,
    weight_threshold = NA
  )
  saveRDS(
    vecAll,
    file = paste0(
      file.path(getwd(), spp, 'Data'),
      paste0('/total_exposure_timeseries_all_var_', forecast_release, '_', forecast_init, '_', hindcast_release,'_',hindcast_yr_range,'.rds')
    )
  )

  #timeseries for important variables
  vecImp <- make_total_exposure(
    type = 'timeseries',
    variable_exposure = vecExp,
    count_all = F,
    variable_weights = var_imp[nrow(var_imp),match(rownames(vecExp), colnames(var_imp))], #last row is always the weighted average of the ensemble
    weight_threshold = 0.1
  )
  saveRDS(
    vecImp,
    file = paste0(
      file.path(getwd(), spp, 'Data'),
      paste0('/total_exposure_timeseries_imp_var_', forecast_release, '_', forecast_init, '_', hindcast_release,'_',hindcast_yr_range,'.rds')
    )
  )

  log_info('total exposure timeseries for {spp} complete.')

}

