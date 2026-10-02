#' @title Plot SDM Results
#' @description A wrapper function for \code{plot_sdm} and \code{plot_variable_radars}
#'
#' @param spp species name. Used to pull correct data and save outputs in species-specific folders.
#' @param hindcast_release MOM6 release codes for the (f)orecast and (h)indcasts used. Used to pull correct variable exposures
#' @param type character string designating what kind of results to plot. Options are 'prob' or 'residuals'
#' @param coastline shapefile used to plot land in model prediction plots
#' @param bathymetry spatRaster file of bathymetry data; used to plot bathymetry in stock boundary plots
#'
#' @return no returns; figures saved in desired folder 
#' 
plot_sdm_wrapper <- function(
    spp,
    model, 
    type = 'hindcast',
    release,
    init = NULL,
    spatial_temporal = FALSE,
    mask_bathy = TRUE,
    rm_corr = TRUE,
    var_names,
    coastline, 
    bathymetry) {
  
  suffix <- if (spatial_temporal) "" else "_global"
  bathy_suffix <- if (mask_bathy) "_masked" else ""
  corr_suffix <- if (rm_corr) "rmcorr" else ""
  
  #### 1) Model results 
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
  abund <- terra::rast(file.path(
    here::here("SDMs"),
    spp,
    'output_rasters',
    paste0(toupper(model),
      '_',
      type,
      '_',
      release,
      ifelse(type == 'hindcast', paste0(bathy_suffix, suffix), init),
      '.tif'
    )
  ))
  
  #avg ensemble HSM
  avgHSM <- terra::tapp(
    abund,
    index = rep(1:12, times = terra::nlyr(abund) / 12),
    fun = 'mean'
  ) #average by month => assuming a monthly timeseries
  names(avgHSM) <- month.abb
  
  
  #plot maps 
  plot_sdms(sdm = avgHSM,
                               stocks = stocks,
                               coastline = coastline, 
                               bathymetry = bathymetry,
                               type = 'model',
                               fig_name = paste0(
                                 file.path(
                                   here::here("SDMs"),
                                   spp,
                                   'Figures',
                                   paste0(
                                     'mean_SDM_',
                                     toupper(model), 
                                     '_',
                                     type,
                                     '_',
                                     release,
                                     ifelse(type == 'forecast', paste0('_', init), ''),
                                     '.pdf'
                                   )
                                 )
                               ))
  

  
  #### 2 plot residuals 
  #load observations
  obs <- utils::read.csv(file.path(
    here::here("SDMs"),
    spp,
    paste0(
      'fisheries_environment',
      '_',
      corr_suffix,
      '_hindcast_',
      release,
      bathy_suffix,
      suffix,
      '.csv'
    )
  ))

  #plot  
  plot_sdms(sdm = abund, #need to pass raw data here to match observations/sdm correctly
            obs = obs,
            xy_col = c("grid.lon", "grid.lat"),
            month_col = 'month',
            stocks = stocks,
            coastline = coastline, 
            bathymetry = bathymetry,
            type = 'residuals',
            fig_name = paste0(
              file.path(
                here::here("SDMs"),
                spp,
                'Figures',
                paste0(
                  'mean_residuals_',
                  toupper(model), 
                  '_',
                  type,
                  '_',
                  release,
                  '.pdf'
                )
              )
            ), 
            hist_name = paste0(
              file.path(
                here::here("SDMs"),
                spp,
                'Figures',
                paste0(
                  'hist_residuals_',
                  toupper(model), 
                  '_',
                  type,
                  '_',
                  release,
                  '.pdf'
                )
              )
            ))
  
  ####3 Plot Variable Importance
  training_name <- file.path(
    here::here('SDMs'), 
    spp,
    paste0(
      'training_',
      training_years[1],
      '_',
      training_years[2],
      '_',
      corr_suffix,
      '_hindcast_',
      release,
      bathy_suffix,
      suffix,
      '.csv'
    )
  )
  
  dfT <- utils::read.csv(file.path(training_name))
  
  #normalized variable importance
  #read in variable importance outputs & create list
  flist <- dir(
    file.path(here::here('SDMs'), 
              spp, 'model_output/importance'),
    full.names = T,
    pattern = 'rds'
  )
  imp_list <- vector(mode = 'list', length = length(flist))
  for (x in 1:length(flist)) {
    load(flist[x])
    imp_list[[x]] <- imp
  }
  names(imp_list) <- gsub(
    '.rds',
    '',
    dir(
      file.path(here::here('SDMs'), 
                spp, 'model_output/importance'),
      full.names = F,
      pattern = 'rds'
    )
  )
  
  imp_list <- imp_list[names(imp_list) %in% toupper(component_models)]
  
  load(file.path(here::here('SDMs'), 
                 spp, 'model_output/ensemble_weights.rds')) #weights
  
  #pull variable names from mapexp
  dyn_vars <- names(dfT)[names(dfT) %in% var_names]
  
  #create variable importance
  var_imp <- normalize_variable_importance(
    vars = dyn_vars,
    ens_weights = weights,
    imp_list = imp_list
  )
  
  #save
  saveRDS(var_imp,
    file.path(
      here::here('SDMs'), 
      spp,
      'model_output',
      'normalized_variable_importance.rds'
    )
  )
  
  plot_variable_radars(variable_importance = var_imp, 
                       fig_name = paste0(
                         file.path(here::here('SDMs'), spp, 'Figures'),
                         paste0(
                           '/variable_importance_all.pdf'
                         )
                       ))
}
