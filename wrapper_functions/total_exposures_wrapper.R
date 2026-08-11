#' @title Make Timeseries and Maps of Total Exposure
#' @description A wrapper function for \code{make_total_exposure} that handles object loading, produces both maps and timeseries, and averages total spatial maps globally and within stock polygons if desired. automatically does calculation both with all variables and with only the important variables
#'
#' @param spp species name. Used to pull correct data and save outputs in species-specific folders.
#' @param present_time,future_time character strings indicating the present and future time series to compare. Example: '1993-2019'. Used to pull correct ranked exposure values and save the data properly
#'
#' @return Nothing is returned. The outputs from \code{make_total_exposure(type = 'map')} and \code{make_total_exposure(type = 'timeseries')} are saved in the appropriate folders

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
  #model weights
  weights <- load(file.path('./SDMs/', spp, 'model_output',
            'ensemble_weights.rds'))

  #variable maps
  load(
    file = paste0(
      file.path(getwd(), spp, 'Data'),
      '/',
      present_time,
      ' vs ',
      future_time,
      '/variable_exposure_maps.RData'
    )
  ) # mapExp - will likely change when naming convention does

  #variable timeseries
  load(
    file = paste0(
      file.path(getwd(), spp, 'Data'),
      '/',
      present_time,
      ' vs ',
      future_time,
      '/variable_exposure_timeseries.RData'
    )
  ) #vecExp

  # ==========================================================
  # STEP 2: Calculate Exposures Spatially
  # ==========================================================

  #with all variables
  mapTot <- make_total_exposure(
    type = 'map',
    variable_exposure = mapExp,
    count_all = F,
    weights = NA,
    weights_threshold = NA
  )
  save(
    mapTot,
    file = paste0(
      file.path(getwd(), spp, 'Data'),
      '/',
      present_time,
      ' vs ',
      future_time,
      '/total_exposure_map_all_var.RData'
    )
  )

  #with important variables
  mapImp <- make_total_exposure(
    type = 'map',
    variable_exposure = mapExp,
    count_all = T,
    weights = weights,
    weights_threshold = 0.1
  )
  save(
    mapImp,
    file = paste0(
      file.path(getwd(), spp, 'Data'),
      '/',
      present_time,
      ' vs ',
      future_time,
      '/total_exposure_map_imp_var.RData'
    )
  )

  log_info('spatial total exposures for {spp} complete.')

  # ==========================================================
  # STEP 3: Calculate Exposures Across Time
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
    weights = weights,
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
}

