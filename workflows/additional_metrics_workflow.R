###additional metrics workflow
#includes directionality & changes in distributions
#data quality is included in sensitivity_workflow
#model confidence is included in sdm_workflow

library(spatialcva)

here::i_am('workflows/READ-EDAB-CVA2.0-Workflows/workflows/additional_metrics_workflow.R')

##############################
####### DIRECTIONALITY #######
##############################

setwd(
  here::here("/AdditionalMetrics/Directionality")
)

#create combined data.frame
flist <- dir('./raw_csvs', pattern = '.csv', full.names = T)
scorers <- sub(".*/NECVA2.0_Directional_Scores_(.*)\\.csv$", "\\1", flist)
direct <- NULL
#this presumes a similar set up and naming scheme to the model confidence spreadsheets
for (x in 1:length(flist)) {
  #load in data frame & clean
  f <- read.csv(flist[x], skip = 2) #remove header when loading in

  #add scorer column in case you want that information
  f$Scorer <- scorers[x]

  #append to data.frame
  direct <- rbind(direct, f)
}
write.csv(direct, file = 'combined_directionality.csv') #save for prosperity (the above should be quick but just in case)

#now we calculate metric similar to sensitivity
species.data.list <- split(direct, direct$Species)
species.direct <- lapply(species.data.list, calculate_directionality, bootstrap = F) #calculate sensitivity w/o bootstrap
direct.bootstrap <- lapply(species.data.list, calculate_directionality, bootstrap = T) #this only takes ~5 minutes for 42 species

#get certainty
direct.certainty <- mapply(
  calculate_directionality_certainty,
  direct.bootstrap,
  species.direct,
  SIMPLIFY = F
)
directDF <- as.data.frame(do.call(rbind, direct.certainty))
directDF$Certainty <- as.numeric(directDF$Certainty)
write.csv(directDF, 'directionality_scores.csv') #save results


##############################

##############################
#### DISTRIBUTION CHANGE #####
##############################
setwd(
  here::here("AdditionalMetrics/DistributionChange")
)

spp.list <- read.csv(
  file.path(here::here('SDMs'), 'spp_list.csv')
)
spp.list$Name <- gsub(' ', '', spp.list$Common.Name)
spp.list <- spp.list[-42,]

#create species specific folders
for (x in 1:nrow(spp.list)) {
  dir.create(
    file.path(
      here::here("AdditionalMetrics/DistributionChange"),
      spp.list$Name[x]
    ),
    showWarnings = T
  ) #main folder
}

##########################################
##### PREDICT MODELS TO FORECAST  ########
##########################################

statics <- terra::rast('./Data/staticVariables_masked_norm_terra.tif')
bathy <- terra::wrap(statics$bathy)

#first, pull forecast data
###decadal forecast
forecast.list <- data.frame(
  Long.Name = c(
    'Sea Water Potential Temperature at Sea Floor',
    'Bottom Oxygen',
    'Sea Water Salinity at Sea Floor',
    'Bottom Aragonite Solubility',
    'Sea Surface Temperature',
    'Sea Surface Salinity',
    'Surface pH',
    'Mixed layer depth (delta rho = 0.03)',
    'Diazotroph new (NO3-based) prim. prod. integral in upper 100m',
    'Small phyto. new (NO3-based) prim. prod. integral in upper 100m',
    'Medium phyto. new (NO3-based) prim. prod. integral in upper 100m',
    'Large phyto. new (NO3-based) prim. prod. integral in upper 100m',
    'Small zooplankton nitrogen biomass in upper 100m',
    'Medium zooplankton nitrogen biomass in upper 100m',
    'Large zooplankton nitrogen biomass in upper 100m',
    'Water column net primary production vertical integral',
    'Downward Flux of Particulate Organic Carbon'
  ),
  Short.Name = c(
    'bottomT',
    'bottomO2',
    'bottomS',
    'bottomArg',
    'surfaceT',
    'surfaceS',
    'surfacepH',
    'MLD',
    'diazPP',
    'smallPP',
    'mediumPP',
    'largePP',
    'smallZoo',
    'mediumZoo',
    'largeZoo',
    'intNPP',
    'POC'
  )
)

#parallel version
plan(multisession, workers = 8)
mom6_results <- future_map(
  1:nrow(forecast.list),
  ~ get_model_data_wrapper(
    var_name = forecast.list$Long.Name[.x],
    short_name = forecast.list$Short.Name[.x],
    json_url = "https://psl.noaa.gov/cefi_portal/data_index/cefi_data_indexing.Projects.CEFI.regional_mom6.cefi_portal.northwest_atlantic.full_domain.decadal_forecast.json",
    release = 'r20250925',
    init = 'i202501',
    spatial_temporal = FALSE,
    source = "forecast",
    mask_bathy = T,
    bathy = bathy,
    bathy_range = c(-1000, 0),
    force_overwrite = T
  ),
  .progress = T
)
plan(sequential)

#because the forecasts have a lot more data to pull from the servers (300+ timestamps for 10 ensemble members), the servers can get angry and the pulls can fail, especially when you are making a lot of requests at the same time. Since the forecasts aren't necessary until calculating exposure and predicting future habitat change, the forecast pulls can happen over a longer period (aka overnight if you're in between steps, etc), so below is the option to run the code in sequence if you want to do that

#for(x in c(9, 15)){
#print(Sys.time())
#get_model_data_wrapper(
#   var_name = forecast.list$Long.Name[x],
#  short_name = forecast.list$Short.Name[x],
# json_url = "https://psl.noaa.gov/cefi_portal/data_index/cefi_data_indexing.Projects.CEFI.regional_mom6.cefi_portal.northwest_atlantic.full_domain.decadal_forecast.json",
#release = 'r20250925',
#    init = 'i202501',
#   spatial_temporal = FALSE,
#  source = "forecast",
# mask_bathy = T,
#bathy = bathy,
#    bathy_range = c(-1000, 0),
#   force_overwrite = T
#)
#  print(x)
# print(Sys.time())
#}

#second, normalize forecast data to HINDCAST MEAN/SD
norm_forecast <- vector(
  mode = 'list',
  length = length(forecast.list$Short.Name)
)
for (x in 1:length(forecast.list$Short.Name)) {
  raw <- terra::rast(
    './Data/MOM6/raw_MOM6_',
    forecast.list$Short.Name[x],
    '_forecast_r20250925_i202501_global.tif'
  )
  hind_avg <- load(
    './Data/MOM6/avg_',
    forecast.list$Short.Name[x],
    '_hindcast_r20250715_masked_global.rds'
  )
  hind_sd <- load(
    './Data/MOM6/sd_',
    forecast.list$Short.Name[x],
    '_hindcast_r20250715_masked_global.rds'
  )

  norm <- normalize_model_data(
    raw = raw,
    avg = hind_avg,
    sd = hind_sd,
    spatial_temporal = F
  )
  terra::writeRaster(
    norm,
    filename = paste0(
      './Data/MOM6/norm_',
      forecast.list$Short.Name[x],
      '_forecast_r20250925_i202501_hindcast_r20250715_global.tif'
    )
  )
  #add to big list to pass to predictions
  norm_forecast[[x]] <- norm
}

names(norm_forecast) <- forecast.list$Short.Name

#third, predict models
statics <- terra::rast('./Data/staticVariables_masked_norm_terra.tif')
#reproject statics to forecast grid because somehow they are always different
statics <- resample(statics, norm_forecast[[1]], method = "bilinear") #using raw data from pull_mom6_hindcast
statics <- terra::wrap(statics)

mods <- c("BRT", "GAM", "MAXENT", "SDMTMB")

# 1. Load parallel packages
library(foreach)
library(doParallel)
library(terra)

# 2. Set up the parallel cluster
num_cores <- 6
cl <- parallel::makeCluster(num_cores)
doParallel::registerDoParallel(cl)

# 3. CRITICAL: Wrap any SpatRaster objects in the global environment

# New: Apply wrap to each raster in the list
norm_forecast_wrapped <- lapply(norm_forecast, terra::wrap)
# Assuming static_variables is already wrapped based on your original code:
# static_variables_wrapped <- static_variables

# 4. Execute the parallel loop
foreach(
  s = 1:nrow(spp.list),
  .packages = c("terra"),
  .export = c(
    "make_sdm_predictions",
    'prep_time_step_df',
    'prep_time_step_stack'
  ),
  .errorhandling = "pass"
) %dopar%
  {
    # a. Unwrap the spatial data inside the worker environment

    # New: Apply unwrap to each wrapped object in the list
    norm_forecast_worker <- lapply(norm_forecast_wrapped, terra::unwrap)
    static_vars_worker <- terra::unwrap(statics)

    # b. Load in training data for the species
    dfT <- read.csv(file.path(
      here::here("SDMs"),
      spp.list$Name[s],
      'training_1993_2019_rmcorr_hindcast_r20250715_masked_global.csv'
    ))
    preds <- vector(mode = 'list', length = length(mods))

    # c. Predict component models
    for (m in 1:length(mods)) {
      # FIX: Use readRDS() for .rds files, not load()
      mod_path <- file.path(
        here::here("SDMs"),
        spp.list$Name[s],
        'model_output',
        'models',
        paste0(mods[m], '.rds')
      )
      load(mod_path) #mod

      p <- make_sdm_predictions(
        mod = mod,
        model = tolower(mods[m]),
        rasts = norm_forecast_worker,
        static_variables = static_vars_worker,
        se = dfT,
        pa_col = 'pa',
        month_col = 'month',
        year_col = 'year',
        xy_col = c("grid.lon", "grid.lat")
      )

      # Save prediction
      out_path <- file.path(
        here::here("SDMs"),
        spp.list$Name[s],
        'output_rasters',
        paste0(mods[m], '_forecast_r20250925_i202501.tif')
      )
      terra::writeRaster(p, file = out_path, overwrite = TRUE)

      # Add to list for ensemble
      preds[[m]] <- p
    } #end m

    # d. Now predict ensemble
    # FIX: Assigning weights via load() returns a character string. Use readRDS() instead.
    weights_path <- file.path(
      here::here("SDMs"),
      spp.list$Name[s],
      'model_output',
      'ensemble_weights.rds'
    )
    load(weights_path) #weights

    ens <- make_sdm_predictions(
      model = 'ensemble',
      rasts = preds,
      weights = weights
    )

    # Save ensemble
    ens_path <- file.path(
      here::here("SDMs"),
      spp.list$Name[s],
      'output_rasters',
      'ENSEMBLE_forecast_r20250925_i202501.tif'
    )
    terra::writeRaster(ens, file = ens_path, overwrite = TRUE)

    # Return NULL to prevent foreach from saving massive raster lists into RAM
    return(NULL)
  }

# 5. Stop the cluster when finished
parallel::stopCluster(cl)
##########################################

###############################
#### CALCULATE SHIFTS #########
###############################
statics <- terra::rast(here::here('SDMs/Data/staticVariables_cropped_terra_reproj.tif'))
bathy <- statics$bathy

land <- vect(here::here('shpfiles/gshhg-shp-2.3.7/GSHHS_shp/i/GSHHS_i_L1.shp'))
landNE <- terra::crop(land, bathy)


##############################
