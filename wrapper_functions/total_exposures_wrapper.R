#' @title Make Timeseries and Maps of Total Exposure
#' @description A wrapper function for \code{make_total_exposure} that handles object loading, produces both maps and timeseries, and averages total spatial maps globally and within stock polygons if desired. automatically does calculation both with all variables and with only the important variables
#'
#' @param spp species name. Used to pull correct data and save outputs in species-specific folders.
#' @param present_time,future_time character strings indicating the present and future time series to compare. Example: '1993-2019'. Used to pull correct ranked exposure values and save the data properly
#'
#' @return returns a data.frame containing the spatial averages of total exposure, plus variable exposure across the entire domain, and within stock polygons if shpfiles exists. The outputs from \code{make_total_exposure(type = 'map')} and \code{make_total_exposure(type = 'timeseries')} are saved in the appropriate folders

total_exposures_wrapper <- function(
    spp,
    present_time,
    future_time
) {

  # ==========================================================
  # STEP 0: Set Up
  # ==========================================================
  # Set up the logger to output to your specific file
  log_file <- file.path(getwd(), 'logs', 'ensemble.log')
  log_appender(appender_file(log_file))

  log_info("Calculating total exposures for {spp}")

  # ==========================================================
  # STEP 1: Load in Data
  # ==========================================================
  #ensemble weights
  weights <- load(file.path('./SDMs/', spp, 'model_output',
            'ensemble_weights.rds'))

  #variable maps
  mapExp <- terra::rast(paste0(
    file.path(getwd(), spp, 'Data'),
    '/',
    present_time,
    ' vs ',
    future_time,
    '/variable_exposure_maps.tif'
    )
  ) # mapExp - will change when naming convention does

  #variable timeseries
  load(
    file = paste0(
      file.path(getwd(), spp, 'Data'),
      '/',
      present_time,
      ' vs ',
      future_time,
      '/variable_exposure_timeseries.rds'
    )
  ) #vecExp

  #normalized variable importance; load if file exists, make it if it doesn't
  if(!file.exists(file.path('./SDMs/', spp, 'model_output',
                            'normalized_variable_importance.rds'))){
    #read in variable importance outputs & create list
    flist <- dir(
      file.path('./SDMs/', spp, 'model_output/importance'),
      full.names = T,
      pattern = 'rds'
    )
    imp_list <- vector(mode = 'list', length = length(flist))
    for(x in 1:length(flist)){
      load(flist[x])
      imp_list[[x]] <- imp
    }

    #pull variable names from mapexp
    dyn_vars <- names(mapExp)

    #create variable importance
    var_imp <- normalize_variable_importance(vars = dyn_vars, ens_weights = weights, imp_list = imp_list)

    #save
    save(var_imp, file = file.path('./SDMs/', spp, 'model_output',
                                   'normalized_variable_importance.rds'))
  } else {
    var_imp <- load(file.path('./SDMs/', spp, 'model_output',
                              'normalized_variable_importance.rds'))
  }

  # ==========================================================
  # STEP 2: Calculate Exposures Across Space
  # ==========================================================

  #with all variables
  mapTot <- make_total_exposure(
    type = 'map',
    variable_exposure = mapExp,
    count_all = F,
    weights = NA,
    weights_threshold = NA
  )
  terra::writeRaster(
   x = mapTot,
    filename = paste0(
      file.path(getwd(), spp, 'Data'),
      '/',
      present_time,
      ' vs ',
      future_time,
      '/total_exposure_map_all_var.tif'
    ),
   overwrite = TRUE
  )

  #with important variables
  mapImp <- make_total_exposure(
    type = 'map',
    variable_exposure = mapExp,
    count_all = T,
    weights = var_imp[nrow(var_imp),], #last row is always the weighted average of the ensemble
    weights_threshold = 0.1
  )
  terra::writeRaster(
    x = mapImp,
    filename = paste0(
      file.path(getwd(), spp, 'Data'),
      '/',
      present_time,
      ' vs ',
      future_time,
      '/total_exposure_map_imp_var.tif'
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
  if(file.exists(paste0('./shpfiles/species_stock_areas/', spp, '.shp'))){
    stocks <- terra::vect(paste0('./shpfiles/species_stock_areas/', spp, '.shp'))
    #average within polygons
  expAvg <- terra::extract(allRasts, stocks, fun = 'mean', na.rm = T)

    #combine
  expAvgs <- rbind(t(globals), expAvg[,-1])
  #add stock column
  expAvg$stock <- c('global', stocks$stock_area)

  } else {
    log_info('No stock shpfiles found for {spp}. Only calculating global total exposure from maps')
    expAvg <- t(globals)
  }

  save(
    expAvg,
    file = paste0(
      file.path(getwd(), spp, 'Data'),
      '/',
      present_time,
      ' vs ',
      future_time,
      '/total_exposure_map_averages.rds'
    )
  )
  # ==========================================================
  # STEP 4: Calculate Exposures Across Time
  # ==========================================================

  #timeseries - all variables
  vecAll <- make_total_exposure(
    type = 'timeseries',
    variable_exposure = vecExp,
    count_all = F,
    weights = NA,
    weights_threshold = NA
  )
  save(
    vecAll,
    file = paste0(
      file.path(getwd(), spp, 'Data'),
      '/',
      present_time,
      ' vs ',
      future_time,
      '/total_exposure_timeseries_all_var.rds'
    )
  )

  #timeseries for important variables
  vecImp <- make_total_exposure(
    type = 'timeseries',
    variable_exposure = vecExp,
    count_all = T,
    weights = var_imp[nrow(var_imp),], #last row is always the weighted average of the ensemble
    weights_threshold = 0.1
  )
  save(
    vecImp,
    file = paste0(
      file.path(getwd(), spp, 'Data'),
      '/',
      present_time,
      ' vs ',
      future_time,
      '/total_exposure_timeseries_imp_var.rds'
    )
  )

  log_info('total exposure timeseries for {spp} complete.')

  return(expAvg)
}

