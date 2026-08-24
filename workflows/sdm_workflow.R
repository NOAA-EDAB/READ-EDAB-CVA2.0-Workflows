###building out targets workflow independently of targets
#uses functions built for targets and saves everything in species specific folders

##################################
#####SET UP - LOAD EVERY TIME ####
##################################

### set working directory
setwd('/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs')
#setwd('/home/oneapi/ClimateVulnerabilityAssessment2.0/SDMs')

### load package
#library(devtools)
#devtools::load_all('~/ClimateVulnerabilityAssessment2.0/functions/READ-EDAB-CVA2.0')
library(spatialcva)

#load additional packages needed for workflows
library(future)
library(furrr)
library(logger)

#create species folders and appropriate subfolders
spp.list <- read.csv('spp_list.csv')
spp.list$Name <- gsub(' ', '', spp.list$Common.Name)

#make directory for each species if it doesn't exist; if directory exists, it is not changed
for (x in 1:nrow(spp.list)) {
  dir.create(file.path(getwd(), spp.list$Name[x]), showWarnings = T) #main folder
  #dir.create(
<<<<<<< HEAD
   # file.path(getwd(), spp.list$Name[x], 'input_rasters'),
    #showWarnings = T
 # ) #input raster folder
=======
  # file.path(getwd(), spp.list$Name[x], 'input_rasters'),
  #showWarnings = T
  # ) #input raster folder
>>>>>>> dev
  dir.create(
    file.path(getwd(), spp.list$Name[x], 'input_csvs'),
    showWarnings = T
  )
  dir.create(
    file.path(getwd(), spp.list$Name[x], 'output_rasters'),
    showWarnings = T
  ) #output data folder
  dir.create(
    file.path(getwd(), spp.list$Name[x], 'model_output'),
    showWarnings = T
  ) #model_output folder
  dir.create(
    file.path(getwd(), spp.list$Name[x], 'model_output', 'models'),
    showWarnings = T
  ) #model_output/models folder
  dir.create(
    file.path(getwd(), spp.list$Name[x], 'model_output', 'cvs'),
    showWarnings = T
  ) #model_output/cvs folder
  dir.create(
    file.path(getwd(), spp.list$Name[x], 'model_output', 'preds'),
    showWarnings = T
  ) #model_output/preds folder
  dir.create(
    file.path(getwd(), spp.list$Name[x], 'model_output', 'eval_metrics'),
    showWarnings = T
  ) #model_output/eval_metrics folder
  dir.create(
    file.path(getwd(), spp.list$Name[x], 'model_output', 'importance'),
    showWarnings = T
  ) #model_output/importance folder
  dir.create(file.path(getwd(), spp.list$Name[x], 'figures'), showWarnings = T) #model_output folder
}


##################################

##############################
#####GET FISHERIES DATA ######
##############################

####1993-2023
#survey data - needs VPN
surv <- standardize_fisheries_data(
  data_type = 'NESurveys',
  channel = dbutils::connect_to_database(
    server = "NEFSC_pw_oraprod",
    uid = "KGALLAGHER"
  ),
  yr_range = c(1993, 2023)
)
write.csv(surv, './Data/csvs/standardized/Survey_1993_2023.csv')

#observer data - needs VPN
obs <- standardize_fisheries_data(
  data_type = 'NEObserver',
  channel = dbutils::connect_to_database(
    server = "NEFSC_pw_oraprod",
    uid = "KGALLAGHER"
  ),
  yr_range = c(1993, 2023)
)
write.csv(obs, './Data/csvs/standardized/Observer_1993_2023.csv')

## State run surveys
#maine/new hampshire
menh <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = "./Data/csvs/raw/MaineDMR_Trawl_Survey_Tow_Catch_2025-07-17.csv",
  csv_columns = c(
    'towID',
    'Start_Longitude',
    'Start_Latitude',
    'Start_Date',
    'Number_Caught',
    'Common_Name'
  ),
  yr_range = c(1993, 2023)
)
write.csv(menh, './Data/csvs/standardized/MENH_1993_2023.csv')

#mass
mass <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = "./Data/csvs/raw/MABottom_Trawl_02_2026.csv",
  csv_columns = c('towID', 'Lon', 'Lat', 'Date', 'Num', 'SCI_NAME'),
  yr_range = c(1993, 2023)
)
write.csv(mass, './Data/csvs/standardized/MA_1993_2023.csv')

#new jersey
nj <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = "./Data/csvs/raw/NJOT_Tow_Catch_2025-07-01.csv",
  csv_columns = c(
    'TOW_ID',
    'START_LON',
    'START_LAT',
    'DATE.FORMAT',
    'NUMBER',
    'LATIN_NAME'
  ),
  yr_range = c(1993, 2023)
)
write.csv(nj, './Data/csvs/standardized/NJ_1993_2023.csv')

#ct
ct <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = "./Data/csvs/raw/CT_Tow_Catch_Feb_2026.csv",
  csv_columns = c(
    'Sample.Number',
    'Longitude',
    'Latitude',
    'Date',
    'TotalCount',
    'name'
  ),
  yr_range = c(1993, 2023)
)
write.csv(ct, './Data/csvs/standardized/CT_1993_2023.csv')

#delaware
de <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = "./Data/csvs/raw/DE_Tow_Catch_2025-07-18.csv",
  csv_columns = c('towID', 'LONDD', 'LATDD', 'date', 'number', 'SCI_NAME'),
  yr_range = c(1993, 2023)
)
write.csv(de, './Data/csvs/standardized/DE_1993_2023.csv')

#neamap
neamap <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = "./Data/csvs/raw/NEAMAP_Tow_Catch_Feb2026.csv",
  csv_columns = c(
    'station',
    'lon',
    'lat',
    'date',
    'present_absent',
    'SCI_NAME'
  ),
  yr_range = c(1993, 2023)
)
write.csv(neamap, './Data/csvs/standardized/NEAMAP_1993_2023.csv')

#ny
ny <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = "./Data/csvs/raw/NYDEC_Tow_Catch_Feb2026.csv",
  csv_columns = c('STATION', 'LONDD', 'LATDD', 'time', 'Presence', 'COM_NAME'),
  yr_range = c(1993, 2023)
)
write.csv(ny, './Data/csvs/standardized/NY_1993_2023.csv')

####pull in older NEAMAP & MA w/bft
#neamap
neamapBFT <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = "./Data/csvs/raw/NEAMAP_Tow_Catch_2025-09-15-wBFT.csv",
  csv_columns = c(
    'station',
    'lon',
    'lat',
    'date',
    'present_absent',
    'SCI_NAME'
  ),
  yr_range = c(1993, 2023)
)
write.csv(neamapBFT, './Data/csvs/standardized/NEAMAP.BFT_1993_2023.csv')

#mass
massBFT <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = "./Data/csvs/raw/MABottom_Trawl_2025-08-6-wBFT.csv",
  csv_columns = c('towID', 'Lon', 'Lat', 'Date', 'Num', 'SCI_NAME'),
  yr_range = c(1993, 2023)
)
write.csv(massBFT, './Data/csvs/standardized/MA.BFT_1993_2023.csv')

###additional smooth dogfish presences from HMS
hmsSD <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = './Data/csvs/raw/HMS_SmoothDogfish_09-12-25.csv',
  csv_columns = c('id', 'LON', 'LAT', 'date', 'pa', 'SCI_NAME'),
  yr_range = c(1993, 2023)
)
write.csv(hmsSD, './Data/csvs/standardized/HMS_1993_2023.csv')

##shrimp survey
shrimp <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = './Data/csvs/raw/NEFSC_NShrimp_092025.csv',
  csv_columns = c(
    'towID',
    'DECDEG_BEGLON',
    'DECDEG_BEGLAT',
    'BEGIN_EST_TOWDATE',
    'EXPCATCHNUM',
    'SCINAME'
  ),
  yr_range = c(1993, 2023)
)
write.csv(shrimp, './Data/csvs/standardized/Shrimp_1993_2023.csv')

#gom bll
gom <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = "./Data/csvs/raw/GOM_BLLS_092025.csv",
  csv_columns = c(
    'ID',
    'DECDEG_BEGLON_SET',
    'DECDEG_BEGLAT_SET',
    'startDate',
    'CATCHNUM',
    'COMMON_NAME'
  ),
  yr_range = c(1993, 2023)
)
write.csv(gom, './Data/csvs/standardized/GOM.LL_1993_2023.csv')

#GOP
gop <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = "./Data/csvs/raw/GOP_092025.csv",
  csv_columns = c(
    'ID',
    'SET_BEGIN_LONG_CONV',
    'SET_BEGIN_LAT_CONV',
    'startDate',
    'NUM_FISH',
    'SPECIES_NAME'
  ),
  yr_range = c(1993, 2023)
)
write.csv(gop, './Data/csvs/standardized/GOP_1993_2023.csv')

#POP
pop <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = "./Data/csvs/raw/POP_092025.csv",
  csv_columns = c(
    'ID',
    'LONDD',
    'LATDD',
    'startDate',
    'NUM_FISH',
    'SPECIES_NAME'
  ),
  yr_range = c(1993, 2023)
)
write.csv(pop, './Data/csvs/standardized/POP_1993_2023.csv')

#logbooks
log <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = "./Data/csvs/raw/Logbook_122025.csv",
  csv_columns = c(
    'TRIPN',
    'LONDD',
    'LATDD',
    'DATE',
    'BFT_TOTAL',
    'NAME'
  ),
  yr_range = c(1993, 2023)
)
write.csv(log, './Data/csvs/standardized/LOGBOOK_1993_2023.csv')

#lps
lps <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = "./Data/csvs/raw/LPS_Oct2025.csv",
  csv_columns = c(
    'ID',
    'decdeg_lat',
    'decdeg_long',
    'startDate',
    'caught',
    'COMNAME'
  ),
  yr_range = c(1993, 2023)
)
write.csv(lps, './Data/csvs/standardized/LPS_1993_2023.csv')

##############################

##############################
#####GET MOM6 DATA ###########
##############################

var.list <- data.frame(
  Long.Name = c(
    'Bottom Temperature',
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

#load in bathy for masking
staticR <- terra::rast('~/ClimateVulnerabilityAssessment2.0/SDMs/Data/staticVariables_cropped_terra_reproj.tif')#staticR
#bathy object = staticR$bathy
bathy <- terra::wrap(staticR$bathy) #this is required because of the way terra holds rasters in memory and how things are distributed in parallel with future_map; the bathy raster gets unwrapped within the wrapper function

####hindcast
<<<<<<< HEAD
log_appender(appender_file("mom6_hindcast.log"))

# Set up the cluster ONCE outside the loop
plan(multisession, workers = 6)
norm_results <- future_map(
  1:nrow(var_df),
  ~get_model_data_wrapper(
    source = 'hindcast',
    var_name = var.list$Long.Name[.x],
    short_name = var.list$Short.Name[.x],
    json_url = "https://psl.noaa.gov/cefi_portal/data_index/cefi_data_indexing.Projects.CEFI.regional_mom6.cefi_portal.northwest_atlantic.full_domain.hindcast.json",
    release = 'r20250715',
    init = NA,
    spatial_temporal = FALSE
  )
)
names(norm_results) <- var_df$Short.Name

# Explicitly close cluster when entirely finished
plan(sequential)

###decadal forecast
  normF <- get_model_forecast_wrapper(
    var_df = var.list,
    in_par = T,
    n_cores = 5,
    json_url = "https://psl.noaa.gov/cefi_portal/data_index/cefi_data_indexing.Projects.CEFI.regional_mom6.cefi_portal.northwest_atlantic.full_domain.decadal_forecast.json",
    release = 'r20250925',
    init = 'i202001',
    ens = x
  )

##############################

###################################
##### BUILD FISHERIES DATA FRAMES #
###################################
#load new key document to determine which sources should be used for which species
source.key <- read.csv('sources.csv')
source.names <- colnames(source.key)[-1] #isolate source names
is.obs.key <- c(F, T, F, F, F, F, F, F, F, T, F, F, T, T, T, T, F, F) #flags which sources are observer based (ie fisheries dependent datasets)

#build argument matrix to pass along to wrapper function through furrr
args <- NULL
for(x in 1:nrow(spp.list)){
  sFlag <- unlist(as.vector(source.key[which(source.key$Common.Name == spp.list$Common.Name[x]),-c(1)])) #pull T/F flags from sources key
  sSources <- source.names[sFlag] #use flags to subset source names
  sObs <- is.obs.key[sFlag] #use flags to subset obs.key
  altNames <- paste(
    spp.list$Common.Name[x],
    spp.list$COM_NAME[x],
    spp.list$Scientific.Name[x],
    spp.list$Alternate.Name[x],
    spp.list$SCI_NAME[x],
    spp.list$SCI_NAME_ALT[x],
    spp.list$SCI_NAME_ALT2[x],
    sep = ','
  )

  a <- data.frame(spp = spp.list$Name[x],
                  is_obs = sObs,
                  source = sSources,
                  all_names = altNames)
  args <- rbind(args, a)
}
args$source <- paste0(args$source, '_1993_2023')
=======
plan(multisession, workers = 8)
mom6_results <- future_map(
  1:nrow(var.list),
  ~get_model_data_wrapper(
    var_name = var.list$Long.Name[.x],
    short_name = var.list$Short.Name[.x],
    json_url = "https://psl.noaa.gov/cefi_portal/data_index/cefi_data_indexing.Projects.CEFI.regional_mom6.cefi_portal.northwest_atlantic.full_domain.hindcast.json",
    release = "r20250715",
    init = NA,
    spatial_temporal = FALSE,
    source = "hindcast",
    mask_bathy = T,
    bathy = bathy,
    bathy_range = c(-1000, 0),
    force_overwrite = F
  ),
  .progress = T
)
plan(sequential)


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
  ~get_model_data_wrapper(
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

#because the forecasts have a lot more data to pull from the servers (300+ timestamps for 10 ensemble members), the servers can get angry and the pulls can fail, especially when you are making a lot of requests at the same time. Since the forecasts aren't necessary until the prediction step, the forecast pulls can happen over a longer period (aka overnight if you're in between steps, etc), so below is the option to run the code in sequence if you want to do that

#for(x in 1:nrow(forecast.list)){
 # print(Sys.time())
#  get_model_data_wrapper(
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
##############################
>>>>>>> dev

plan(multisession, workers = 8)
checks <- future_pmap(
  list(
    ..1 = args$source,
    ..2 = args$spp,
    ..3 = args$all_names,
    ..4 = args$is_obs
  ),
  ~ save_df_wrapper(
    csv_name = ..1,
    spp = ..2,
    spp_names = ..3,
    is_obs = ..4,
    skip = F,
    grid = "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northwest_atlantic/full_domain/hindcast/monthly/regrid/r20250715/tos.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc",
    force_overwrite = TRUE
  ),
  .progress = T
)
plan(sequential)


### Combine all source data frames for each species
plan(multisession, workers = 8)
combs <- future_map(
  1:nrow(spp.list),
  ~ combine_fisheries_dfs_wrapper(name = spp.list$Name[.x],
                                  skip = F,
                                  force_overwrite = T),
  .progress = T
)
plan(sequential)


###quick sanity check because the results can get lost in the log - load each csv in and print range - all should be 0-1
flist <- dir(
  path = getwd(),
  pattern = 'combined_pa.csv',
  recursive = T,
  full.names = T
)
for (x in 1:length(flist)) {
  r <- read.csv(flist[x])
  print(range(r$pa, na.rm = T))
}

###################################

###############################
##### PREPARE DATA FRAMES #####
###############################

var.list <- data.frame(
  Long.Name = c(
    'Bottom Temperature',
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

statics <- terra::rast('./Data/staticVariables_masked_norm_terra.tif')
statics <- terra::wrap(statics) #to help with parallelization

feeding <- read.csv('feeding_guilds.csv')
habitat <- read.csv('habitat_guilds.csv')

options(future.globals.maxSize = Inf) #remove check for sharing large files so that norm is shared across workers since this is a relatively low memory intensive job otherwise
plan(multisession, workers = 4)
combs <- future_map(
  1:nrow(spp.list),
  ~prepare_dataframe_wrapper(name = spp.list$Name[.x],
                             source = 'hindcast',
                             short_names = var.list$Short.Name,
                             release = 'r20250715',
                             spatial_temporal = FALSE,
                             mask_bathy = T,
                             all_env = F,
                             spp_key = spp.list,
                             feed_key = feeding,
                             hab_key = habitat,
                             add_static = T,
                             static_variables = statics,
                             rm_corr = T,
                             training_years = c(1993, 2019),
                             test_years = c(2020, 2023),
                             skip = F,
                             force_overwrite = F),
  .progress = T
)
#sink()
plan(sequential)


##############################

##############################
##### MAKE MODELS  ###########
##############################

models <- c('gam', 'maxent', 'rf', 'brt')

load('norm_MOM6_082025.RData') #norm

args <- tidyr::expand_grid(model = models, spp = spp.list$Name[37:42], skip = F)

plan(multisession, workers = 4)
#sink(file = 'rasters.log', append = T)
checks <- future_pmap(
  list(..1 = args$spp, ..2 = args$model, ..3 = args$skip),
  ~ makeMods(spp = ..1, model = ..2, skip = ..3),
  .progress = T,
  .options = furrr_options(seed = 2025)
)
#sink()
plan(sequential)

### run sdmtmb in sequence, step by step because it doesn't like makeMods for some reason
for (x in 37:nrow(spp.list)) {
  sink(file = file.path(getwd(), 'logs', paste0('sdmtmb', '.log')), append = T)
  print(Sys.time())
  print(spp.list$Name[x])

  load(paste(file.path(getwd(), spp.list$Name[x]), 'pa_clean.RData', sep = '/')) #load data - dfC
  print(paste0(spp.list$Name[x], '- making model - ', Sys.time()))
  mod <- make_sdm(
    se = dfC,
    pa_col = 'value',
    xy_col = c('x', 'y'),
    month_col = 'month',
    year_col = 'year',
    model = 'sdmtmb'
  )
  save(
    mod,
    file = paste0(
      file.path(getwd(), spp.list$Name[x], 'model_output', 'models'),
      '/',
      toupper('sdmtmb'),
      '.RData'
    )
  )
  rm(mod)
  gc(reset = T) #help clean up memory so hopefully this can run sequentially?
}

for (x in 40:nrow(spp.list)) {
  # s <- makeMods(spp = spp.list$Name[x], model = 'sdmtmb', skip = T)
  sink(file = file.path(getwd(), 'logs', paste0('sdmtmb', '.log')), append = T)
  print(Sys.time())
  print(spp.list$Name[x])

  # load(paste(file.path(getwd(),spp.list$Name[x]), 'pa_clean.RData', sep = '/')) #load data - dfC

  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'model_output', 'models'),
    '/',
    toupper('sdmtmb'),
    '.RData'
  )) #mod

  if (class(mod) == 'sdmTMB') {
    #CV
    # print(paste0(spp.list$Name[x], '- performing CV - ', Sys.time()))

    # cv <- sdm_cv(mod = mod, se = dfC, pa_col = 'value', xy_col = c('x', 'y'), month_col = 'month', year_col = 'year', model = 'sdmtmb')
    # save(cv, file = paste0(file.path(getwd(),spp.list$Name[x], 'model_output', 'cvs'), '/', toupper('sdmtmb'), '.RData'))

    load(paste0(
      file.path(getwd(), spp.list$Name[x], 'model_output', 'cvs'),
      '/',
      toupper('sdmtmb'),
      '.RData'
    )) #cv
    print(paste0(spp.list$Name[x], '- Getting Preds - ', Sys.time()))
    preds <- sdm_preds(cv = cv, model = 'sdmtmb')
    save(
      preds,
      file = paste0(
        file.path(getwd(), spp.list$Name[x], 'model_output', 'preds'),
        '/',
        toupper('sdmtmb'),
        '.RData'
      )
    )

    print(paste0(spp.list$Name[x], '- Evaluating Model - ', Sys.time()))
    ev <- sdm_eval(preds = preds, metric = 'auc', model = 'sdmtmb')
    save(
      ev,
      file = paste0(
        file.path(getwd(), spp.list$Name[x], 'model_output', 'eval_metrics'),
        '/',
        toupper('sdmtmb'),
        '.RData'
      )
    )
    rm(cv)
    rm(preds)
    rm(ev) #remove objects after they've been saved to help with memory (hopefully)
  } else {
    #end if
    print('model did not converge. cannot perform cv')
  } #end else
  gc(reset = T) #help clean up memory so hopefully this can run sequentially?
  print(x)
  print(Sys.time())
  sink()
}

for (x in 37:nrow(spp.list)) {
  # s <- makeMods(spp = spp.list$Name[x], model = 'sdmtmb', skip = T)
  sink(file = file.path(getwd(), 'logs', paste0('sdmtmb', '.log')), append = T)
  print(Sys.time())
  print(spp.list$Name[x])

  load(paste(file.path(getwd(), spp.list$Name[x]), 'pa_clean.RData', sep = '/')) #load data - dfC

  print(paste0(
    spp.list$Name[x],
    '- Getting Variable Importance - ',
    Sys.time()
  ))
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'model_output', 'models'),
    '/',
    toupper('sdmtmb'),
    '.RData'
  ))
  imp <- sdm_importance(
    mod = mod,
    se = dfC,
    pa_col = 'value',
    xy_col = c('x', 'y'),
    month_col = 'month',
    year_col = 'year',
    model = 'sdmtmb'
  )
  save(
    imp,
    file = paste0(
      file.path(getwd(), spp.list$Name[x], 'model_output', 'importance'),
      '/',
      toupper('sdmtmb'),
      '.RData'
    )
  )
  rm(imp)
  gc(reset = T) #help clean up memory so hopefully this can run sequentially?
  print(x)
  print(Sys.time())
  sink()
}

##############################

##############################
##### PREDICT MODELS  ########
##############################
##1993-2019 (training time series)
load('./Data/MOM6/norm_MOM6_082025.RData') #norm
load('./Data/staticVariables_cropped.RData') #staticVars
bathyR <- staticVars$bathy

#load NORMALIZED staticVars
load('./Data/staticVariables_cropped_normZ.RData')

models <- c(
  #'gam',
  #'rf',
  'brt'
)

args <- tidyr::expand_grid(
  model = models,
  spp = spp.list$Name,
  skip = F,
  yrMin = 1993,
  yrMax = 2019
)

options(future.globals.maxSize = Inf) #remove check for sharing large files so that norm is shared across workers since this is a relatively low memory intensive job otherwise
plan(multisession, workers = 3)
#sink(file = 'rasters.log', append = T)
checks <- future_pmap(
  list(
    ..1 = args$spp,
    ..2 = args$model,
    ..3 = args$skip,
    ..4 = args$yrMin,
    ..5 = args$yrMax
  ),
  ~ predictMods(spp = ..1, model = ..2, skip = ..3, yrMin = ..4, yrMax = ..5),
  .progress = T,
  .options = furrr_options(seed = 2025)
)
#sink()
plan(sequential)

#maxent refuses to work in parallel so:
for (x in 1:nrow(spp.list)) {
  Sys.time()
  predictMods(
    spp = spp.list$Name[x],
    model = 'maxent',
    skip = F,
    yrMin = 1993,
    yrMax = 2019
  )
  Sys.time()
  print(x)
}

###sdmtmb in my remote container is dumb, so here's the faster way:
load('./Data/staticVariables_cropped.RData') #staticVars
bathyR <- staticVars$bathy
Sys.time()
allDF <- makePredDF(
  norm,
  bathyR = bathyR,
  bathy_max = 1000,
  staticData = './Data/staticVariables_cropped_normZ.RData',
  mask = T
)
Sys.time()

for (x in 37:nrow(spp.list)) {
  Sys.time()
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'model_output', 'models'),
    '/SDMTMB.RData'
  )) #load model - mod
  #predict mod
  if (class(mod) == 'sdmTMB') {
    require(sdmTMB)
    pred <- predict(mod, newdata = allDF, type = 'response') #predict everything all at once
    pred$my <- paste(pred$month, pred$year, sep = '.')
    abund <- predictSDM(
      mod = mod,
      df = pred,
      staticData = './Data/staticVariables_cropped_normZ.RData'
    ) #make into rasters
    save(
      abund,
      file = paste(
        file.path(getwd(), spp.list$Name[x], 'output_rasters'),
        '/SDMTMB.RData',
        sep = ''
      )
    )
  } else {
    print('sdmTMB did not converge; no predictions made')
  }
  rm(pred, abund)
  Sys.time()
}

##############################
##predict to future time series

##doing sdmtmb first because it takes the longest and we need all the memory for it
##2020-2023
load('./Data/MOM6/norm_9319_MOM6_1993_2023_102025.RData')
#this is 1993-2023 inclusive - we just need 2020-2023
norm20 <- vector(mode = 'list', length = length(norm))
for (x in 1:length(norm)) {
  norm20[[x]] <- list(raster::subset(norm[[x]][[1]], 325:372))
}
names(norm20) <- names(norm) #even though names aren't showing up like they do in norm 93-19 it still works

load('./Data/staticVariables_cropped.RData') #staticVars
bathyR <- staticVars$bathy
Sys.time()
all2023 <- makePredDF(
  norm20,
  bathyR = bathyR,
  bathy_max = 1000,
  staticData = './Data/staticVariables_cropped_normZ.RData',
  mask = T
)
Sys.time()

for (x in 37:nrow(spp.list)) {
  Sys.time()
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'model_output', 'models'),
    '/SDMTMB.RData'
  )) #load model - mod
  #predict mod
  if (class(mod) == 'sdmTMB') {
    require(sdmTMB)
    pred <- predict(mod, newdata = all2023, type = 'response') #predict everything all at once
    pred$my <- paste(pred$month, pred$year, sep = '.')
    abund <- predictSDM(
      mod = mod,
      df = pred,
      staticData = './Data/staticVariables_cropped_normZ.RData'
    ) #make into rasters
    save(
      abund,
      file = paste(
        file.path(getwd(), spp.list$Name[x], 'output_rasters'),
        '/SDMTMB_2020_2023.RData',
        sep = ''
      )
    )
  } else {
    print('sdmTMB did not converge; no predictions made')
  }
  rm(pred, abund)
  Sys.time()
}
rm(norm, norm20, all2023)

##2020-2030
load('./Data/MOM6/norm_9319_MOM6_decadalforecast_2020_2030_102025.RData')
#layer names need to be fixed
my <- expand.grid(1:12, 2020:2029)
nm <- paste(my[, 1], my[, 2], sep = '.')
for (x in 1:length(norm)) {
  names(norm[[x]][[1]]) <- nm
}
load('./Data/staticVariables_cropped.RData') #staticVars
bathyR <- staticVars$bathy
Sys.time()
all2030 <- makePredDF(
  norm,
  bathyR = bathyR,
  bathy_max = 1000,
  staticData = './Data/staticVariables_cropped_normZ.RData',
  mask = T
)
Sys.time()

for (x in 37:nrow(spp.list)) {
  Sys.time()
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'model_output', 'models'),
    '/SDMTMB.RData'
  )) #load model - mod
  #predict mod
  if (class(mod) == 'sdmTMB') {
    require(sdmTMB)
    pred <- predict(mod, newdata = all2030, type = 'response') #predict everything all at once
    pred$my <- paste(pred$month, pred$year, sep = '.')
    abund <- predictSDM(
      mod = mod,
      df = pred,
      staticData = './Data/staticVariables_cropped_normZ.RData'
    ) #make into rasters
    save(
      abund,
      file = paste(
        file.path(getwd(), spp.list$Name[x], 'output_rasters'),
        '/SDMTMB_2020_2029.RData',
        sep = ''
      )
    )
  } else {
    print('sdmTMB did not converge; no predictions made')
  }
  rm(pred, abund)
  Sys.time()
}
rm(norm, all2030)

##2025-2035
load('./Data/MOM6/norm_9319_MOM6_decadalforecast_2025_2035_102025.RData')
#layer names need to be fixed
my <- expand.grid(1:12, 2025:2034)
nm <- paste(my[, 1], my[, 2], sep = '.')
for (x in 1:length(norm)) {
  names(norm[[x]][[1]]) <- nm
}
load('./Data/staticVariables_cropped.RData') #staticVars
bathyR <- staticVars$bathy
Sys.time()
all2535 <- makePredDF(
  norm,
  bathyR = bathyR,
  bathy_max = 1000,
  staticData = './Data/staticVariables_cropped_normZ.RData',
  mask = T
)

Sys.time()
for (x in 37:nrow(spp.list)) {
  Sys.time()
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'model_output', 'models'),
    '/SDMTMB.RData'
  )) #load model - mod
  #predict mod
  if (class(mod) == 'sdmTMB') {
    require(sdmTMB)
    pred <- predict(mod, newdata = all2535, type = 'response') #predict everything all at once
    pred$my <- paste(pred$month, pred$year, sep = '.')
    abund <- predictSDM(
      mod = mod,
      df = pred,
      staticData = './Data/staticVariables_cropped_normZ.RData'
    ) #make into rasters
    save(
      abund,
      file = paste(
        file.path(getwd(), spp.list$Name[x], 'output_rasters'),
        '/SDMTMB_2025_2034.RData',
        sep = ''
      )
    )
  } else {
    print('sdmTMB did not converge; no predictions made')
  }
  rm(pred, abund)
  Sys.time()
}
rm(norm, all2535)
##############################
##get other component models (all but sdmtmb) using the original method (predictMods) - predictMods and args are defined using code above for 93-19 prediction
##2020-2023
load('./Data/MOM6/norm_9319_MOM6_1993_2023_102025.RData')
#this is 1993-2023 inclusive - we just need 2020-2023
norm20 <- vector(mode = 'list', length = length(norm))
for (x in 1:length(norm)) {
  norm20[[x]] <- list(raster::subset(norm[[x]][[1]], 325:372))
}
names(norm20) <- names(norm) #even though names aren't showing up like they do in norm 93-19 it still works
norm <- norm20 #overwrite just to keep predictMods the same

models <- c('gam', 'rf', 'brt')
args <- tidyr::expand_grid(
  model = models,
  spp = spp.list$Name,
  skip = F,
  yr_min = 2020,
  yr_max = 2023
)

options(future.globals.maxSize = Inf) #remove check for sharing large files so that norm is shared across workers since this is a relatively low memory intensive job otherwise
plan(multisession, workers = 3)
#sink(file = 'rasters.log', append = T)
checks <- future_pmap(
  list(
    ..1 = args$spp,
    ..2 = args$model,
    ..3 = args$skip,
    ..4 = args$yr_min,
    ..5 = args$yr_max
  ),
  ~ predict_sdms_wrapper(
    spp = ..1,
    model = ..2,
    skip = ..3,
    yr_min = ..4,
    yr_max = ..5
  ),
  .progress = T,
  .options = furrr_options(seed = 2025)
)
#sink()
plan(sequential)


##2020-2030
load('./Data/MOM6/norm_9319_MOM6_decadalforecast_2020_2030_102025.RData')
#layer names need to be fixed
my <- expand.grid(1:12, 2020:2029)
nm <- paste(my[, 1], my[, 2], sep = '.')
for (x in 1:length(norm)) {
  names(norm[[x]][[1]]) <- nm
} #even though names aren't showing up like they do in norm 93-19 it still works

models <- c('gam', 'rf', 'brt')
args <- tidyr::expand_grid(
  model = models,
  spp = spp.list$Name,
  skip = F,
  yr_min = 2020,
  yr_max = 2029
)

options(future.globals.maxSize = Inf) #remove check for sharing large files so that norm is shared across workers since this is a relatively low memory intensive job otherwise
plan(multisession, workers = 3)
#sink(file = 'rasters.log', append = T)
checks <- future_pmap(
  list(
    ..1 = args$spp,
    ..2 = args$model,
    ..3 = args$skip,
    ..4 = args$yr_min,
    ..5 = args$yr_max
  ),
  ~ predict_sdms_wrapper(
    spp = ..1,
    model = ..2,
    skip = ..3,
    yr_min = ..4,
    yr_max = ..5
  ),
  .progress = T,
  .options = furrr_options(seed = 2025)
)
#sink()
plan(sequential)

##2025-2035
load('./Data/MOM6/norm_9319_MOM6_decadalforecast_2025_2035_102025.RData')
#layer names need to be fixed
my <- expand.grid(1:12, 2025:2034)
nm <- paste(my[, 1], my[, 2], sep = '.')
for (x in 1:length(norm)) {
  names(norm[[x]][[1]]) <- nm
}

models <- c('gam', 'rf', 'brt')
args <- tidyr::expand_grid(
  model = models,
  spp = spp.list$Name,
  skip = F,
  yr_min = 2025,
  yr_max = 2035
)

options(future.globals.maxSize = Inf) #remove check for sharing large files so that norm is shared across workers since this is a relatively low memory intensive job otherwise
plan(multisession, workers = 3)
#sink(file = 'rasters.log', append = T)
checks <- future_pmap(
  list(
    ..1 = args$spp,
    ..2 = args$model,
    ..3 = args$skip,
    ..4 = args$yr_min,
    ..5 = args$yr_max
  ),
  ~ predict_sdms_wrapper(
    spp = ..1,
    model = ..2,
    skip = ..3,
    yr_min = ..4,
    yr_max = ..5
  ),
  .progress = T,
  .options = furrr_options(seed = 2025)
)
#sink()
plan(sequential)

##maxent
##2020-2023
load('./Data/MOM6/norm_9319_MOM6_1993_2023_102025.RData')
#this is 1993-2023 inclusive - we just need 2020-2023
norm20 <- vector(mode = 'list', length = length(norm))
for (x in 1:length(norm)) {
  norm20[[x]] <- list(raster::subset(norm[[x]][[1]], 325:372))
}
names(norm20) <- names(norm) #even though names aren't showing up like they do in norm 93-19 it still works
norm <- norm20 #overwrite just to keep predictMods the same

for (x in 1:nrow(spp.list)) {
  Sys.time()
  predict_sdms_wrapper(
    spp = spp.list$Name[x],
    model = 'maxent',
    skip = F,
    yr_min = 2020,
    yr_max = 2023
  )
  Sys.time()
}

##2020-2030
load('./Data/MOM6/norm_9319_MOM6_decadalforecast_2020_2030_102025.RData')
#layer names need to be fixed
my <- expand.grid(1:12, 2020:2029)
nm <- paste(my[, 1], my[, 2], sep = '.')
for (x in 1:length(norm)) {
  names(norm[[x]][[1]]) <- nm
} #even though names aren't showing up like they do in norm 93-19 it still works

for (x in 37:nrow(spp.list)) {
  Sys.time()
  predict_sdms_wrapper(
    spp = spp.list$Name[x],
    model = 'maxent',
    skip = F,
    yr_min = 2020,
    yr_max = 2029
  )
  Sys.time()
}

##2025-2035
load('./Data/MOM6/norm_9319_MOM6_decadalforecast_2025_2035_102025.RData')
#layer names need to be fixed
my <- expand.grid(1:12, 2025:2034)
nm <- paste(my[, 1], my[, 2], sep = '.')
for (x in 1:length(norm)) {
  names(norm[[x]][[1]]) <- nm
}

for (x in 37:nrow(spp.list)) {
  Sys.time()
  predictMods(
    spp = spp.list$Name[x],
    model = 'maxent',
    skip = F,
    yr_min = 2025,
    yr_max = 2035
  )
  Sys.time()
}

##############################

##############################
##### MAKE ENSEMBLE  #########
##############################

#1993-2019
args <- tidyr::expand_grid(
  spp = spp.list$Name,
  yrMin = 1993,
  yrMax = 2019,
  buildEns = T
)

plan(multisession, workers = 3)
#sink(file = 'rasters.log', append = T)
checks <- future_pmap(
  list(..1 = args$spp, ..2 = args$yrMin, ..3 = args$yrMax, ..4 = args$buildEns),
  ~ makeEns(spp = ..1, yrMin = ..2, yrMax = ..3, buildEns = ..4),
  .progress = T,
  .options = furrr_options(seed = 2025)
)
#sink()
plan(sequential)

for (x in 1:nrow(spp.list)) {
  Sys.time()
  print(spp.list$Name[x])
  makeEns(spp = spp.list$Name[x], yrMin = 1993, yrMax = 2019, buildEns = F)
  Sys.time()
}

##2020-23, 2020-30, 2025-35
args1 <- tidyr::expand_grid(
  spp = spp.list$Name,
  yrMin = 2020,
  yrMax = 2023,
  buildEns = F
)
args2 <- tidyr::expand_grid(
  spp = spp.list$Name[1:36],
  yrMin = 2020,
  yrMax = 2029,
  buildEns = F
)
args3 <- tidyr::expand_grid(
  spp = spp.list$Name[1:36],
  yrMin = 2025,
  yrMax = 2034,
  buildEns = F
)

args <- rbind(args1, args2, args3)

plan(multisession, workers = 6)
#sink(file = 'rasters.log', append = T)
checks <- future_pmap(
  list(..1 = args$spp, ..2 = args$yrMin, ..3 = args$yrMax, ..4 = args$buildEns),
  ~ makeEns(spp = ..1, yrMin = ..2, yrMax = ..3, buildEns = ..4),
  .progress = T,
  .options = furrr_options(seed = 2025)
)
#sink()
plan(sequential)

for (x in 1:nrow(spp.list)) {
  Sys.time()
  print(spp.list$Name[x])
  build_ensemble_wrapper(
    spp = spp.list$Name[x],
    yr_min = 2020,
    yr_max = 2023,
    build_ens = F
  )
  Sys.time()
  print(x)
}

##############################

##############################
##### TEST ENSEMBLE  #########
##############################

altNames <- paste(
  spp.list$Common.Name,
  spp.list$COM_NAME,
  spp.list$Scientific.Name,
  spp.list$Alternate.Name,
  spp.list$SCI_NAME,
  spp.list$SCI_NAME_ALT,
  spp.list$SCI_NAME_ALT2,
  sep = ','
)

make_performanceCSV(spp.list, testEns = T, yrMin = 2020, yrMax = 2023)

#distribution of AUCs for component models
boxplot(sppEval[, 7:11])

sppEval$Feeding.Guild <- as.factor(sppEval$Feeding.Guild)
sppEval$Habitat.Guild <- as.factor(sppEval$Habitat.Guild)

#by groups
par(mfrow = c(5, 2), par = c(2, 2, 2, 2))
boxplot(BRT ~ Feeding.Guild, data = sppEval)
boxplot(BRT ~ Habitat.Guild, data = sppEval)

boxplot(GAM ~ Feeding.Guild, data = sppEval)
boxplot(GAM ~ Habitat.Guild, data = sppEval)

boxplot(MAXENT ~ Feeding.Guild, data = sppEval)
boxplot(MAXENT ~ Habitat.Guild, data = sppEval)

boxplot(RF ~ Feeding.Guild, data = sppEval)
boxplot(RF ~ Habitat.Guild, data = sppEval)

boxplot(SDMTMB ~ Feeding.Guild, data = sppEval)
boxplot(SDMTMB ~ Habitat.Guild, data = sppEval)

##distribution of AUCs for ensembles, both within and outside
boxplot(sppEval[, 16:18])

sppEval$Common.Name[which(sppEval$ENS.AUC < 0.7)]
sppEval$Common.Name[which(sppEval$AUC.2020.2023 < 0.7)]

##############################

##############################
##### PLOT ENSEMBLE - OLD  ###
##############################
######average ensembles
load(
  "~/ClimateVulnerabilityAssessment2.0/Exposure/RawExposure/Data/coastline.RData"
)
load(
  "~/ClimateVulnerabilityAssessment2.0/SDMs/Data/staticVariables_cropped.RData"
)
bathyR <- staticVars$bathy

#contemporary
for (x in 1:nrow(spp.list)) {
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/output_rasters/ENSEMBLE_1993_2019.RData'
  )) #abund

  #avg ensemble HSM
  avgHSM <- vector(mode = 'list', length = 12)
  for (y in 1:12) {
    mn <- seq(y, length(abund), by = 12)
    MNS <- raster::stack(abund[mn])
    avgHSM[[y]] <- raster::calc(MNS, fun = mean, na.rm = T)
  } #end for
  avgHSM <- stack(avgHSM)
  names(avgHSM) <- month.abb

  avgHSM <- replace(avgHSM, abs(bathyR) > 1000, NA)

  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'figures'),
      '/mean_SDM_1993_2019_vert.pdf'
    ),
    width = 8,
    height = 11
  )
  par(mfrow = c(4, 3), mar = c(3, 3, 1, 0))
  for (y in 1:12) {
    plot(
      raster::subset(avgHSM, y),
      zlim = c(0, 1),
      col = cmocean('matter')(64),
      legend = F
    )
    plot(coastCropped['id'], col = 'grey', add = T)
    legend('topleft', bty = 'n', legend = month.abb[y], cex = 2)
  }
  image.plot(
    matrix(seq(0, 1, by = 0.1), 11, 11),
    legend.only = T,
    horizontal = T,
    legend.shrink = 0.7,
    smallplot = c(0.4, 0.8, 0.25, 0.35),
    legend.args = list(
      text = 'Probability of\nOccurance',
      cex = 0.75,
      side = 3,
      line = 0.1
    ),
    axis.args = list(cex.axis = 1, mgp = c(3, 0.5, 0)),
    col = cmocean('matter')(64)
  )
  dev.off()

  print(x)
}

#decade 1
for (x in 1:nrow(spp.list)) {
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/output_rasters/ENSEMBLE_2020_2029.RData'
  )) #abund

  #avg ensemble HSM
  avgHSM <- vector(mode = 'list', length = 12)
  for (y in 1:12) {
    mn <- seq(y, length(abund), by = 12)
    MNS <- raster::stack(abund[mn])
    avgHSM[[y]] <- raster::calc(MNS, fun = mean, na.rm = T)
  } #end for
  avgHSM <- stack(avgHSM)
  names(avgHSM) <- month.abb

  avgHSM <- replace(avgHSM, abs(bathyR) > 1000, NA)

  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'figures'),
      '/mean_sdm_2020_2029.pdf'
    ),
    width = 8,
    height = 11
  )
  par(mfrow = c(4, 3), mar = c(3, 3, 1, 0))
  for (y in 1:12) {
    plot(
      raster::subset(avgHSM, y),
      zlim = c(0, 1),
      col = cmocean('matter')(64),
      legend = F
    )
    plot(coastCropped['id'], col = 'grey', add = T)
    legend('topleft', bty = 'n', legend = month.abb[y], cex = 2)
  }
  image.plot(
    matrix(seq(0, 1, by = 0.1), 11, 11),
    legend.only = T,
    horizontal = T,
    legend.shrink = 0.7,
    smallplot = c(0.4, 0.8, 0.25, 0.35),
    legend.args = list(
      text = 'Probability of\nOccurance',
      cex = 0.75,
      side = 3,
      line = 0.1
    ),
    axis.args = list(cex.axis = 1, mgp = c(3, 0.5, 0)),
    col = cmocean('matter')(64)
  )
  dev.off()

  print(x)
}

#decade 2
for (x in 1:nrow(spp.list)) {
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/output_rasters/ENSEMBLE_2025_2034.RData'
  )) #abund

  #avg ensemble HSM
  avgHSM <- vector(mode = 'list', length = 12)
  for (y in 1:12) {
    mn <- seq(y, length(abund), by = 12)
    MNS <- raster::stack(abund[mn])
    avgHSM[[y]] <- raster::calc(MNS, fun = mean, na.rm = T)
  } #end for
  avgHSM <- stack(avgHSM)
  names(avgHSM) <- month.abb

  avgHSM <- replace(avgHSM, abs(bathyR) > 1000, NA)

  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'figures'),
      '/mean_sdm_2024_2034.pdf'
    ),
    width = 8,
    height = 11
  )
  par(mfrow = c(4, 3), mar = c(3, 3, 1, 0))
  for (y in 1:12) {
    plot(
      raster::subset(avgHSM, y),
      zlim = c(0, 1),
      col = cmocean('matter')(64),
      legend = F
    )
    plot(coastCropped['id'], col = 'grey', add = T)
    legend('topleft', bty = 'n', legend = month.abb[y], cex = 2)
  }
  image.plot(
    matrix(seq(0, 1, by = 0.1), 11, 11),
    legend.only = T,
    horizontal = T,
    legend.shrink = 0.7,
    smallplot = c(0.4, 0.8, 0.25, 0.35),
    legend.args = list(
      text = 'Probability of\nOccurance',
      cex = 0.75,
      side = 3,
      line = 0.1
    ),
    axis.args = list(cex.axis = 1, mgp = c(3, 0.5, 0)),
    col = cmocean('matter')(64)
  )
  dev.off()

  print(x)
}

#####changes in distributions
#contemporary within (93-08 vs 09-19)
for (x in 1:nrow(spp.list)) {
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/output_rasters/ENSEMBLE_1993_2019.RData'
  )) #abund

  abund93 <- abund[1:192]
  abund09 <- abund[193:324]

  #avg ensemble HSM
  avg93 <- vector(mode = 'list', length = 12)
  for (y in 1:12) {
    mn <- seq(y, length(abund93), by = 12)
    MNS <- raster::stack(abund93[mn])
    avg93[[y]] <- raster::calc(MNS, fun = mean, na.rm = T)
  } #end for
  avg93 <- stack(avg93)
  names(avg93) <- month.abb

  #avg ensemble HSM
  avg09 <- vector(mode = 'list', length = 12)
  for (y in 1:12) {
    mn <- seq(y, length(abund09), by = 12)
    MNS <- raster::stack(abund[mn])
    avg09[[y]] <- raster::calc(MNS, fun = mean, na.rm = T)
  } #end for
  avg09 <- stack(avg09)
  names(avg09) <- month.abb

  diffHSM <- avg09 - avg93
  diffHSM <- replace(diffHSM, abs(bathyR) > 1000, NA)

  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'figures'),
      '/change_sdm_1993_2008_v_2009_2019.pdf'
    ),
    width = 8,
    height = 11
  )
  par(mfrow = c(4, 3), mar = c(3, 3, 1, 0))
  for (y in 1:12) {
    plot(
      raster::subset(diffHSM, y),
      zlim = c(-0.5, 0.5),
      col = cmocean('balance')(64),
      legend = F
    )
    plot(coastCropped['id'], col = 'grey', add = T)
    legend('topleft', bty = 'n', legend = month.abb[y], cex = 2)
  }
  image.plot(
    matrix(seq(-0.5, 0.5, by = 0.1), 11, 11),
    legend.only = T,
    horizontal = T,
    legend.shrink = 0.7,
    smallplot = c(0.4, 0.8, 0.25, 0.35),
    legend.args = list(
      text = 'Change in \nProbability',
      cex = 0.75,
      side = 3,
      line = 0.1
    ),
    axis.args = list(cex.axis = 1, mgp = c(3, 0.5, 0)),
    col = cmocean('balance')(64)
  )
  dev.off()

  print(x)
  #print(quantile(avg09[] - avg93[], na.rm = T, seq(0,1,0.1)))
}

#contemporary - decade 1
for (x in 1:nrow(spp.list)) {
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/output_rasters/ENSEMBLE_1993_2019.RData'
  )) #abund

  #avg ensemble HSM
  avgHSM <- vector(mode = 'list', length = 12)
  for (y in 1:12) {
    mn <- seq(y, length(abund), by = 12)
    MNS <- raster::stack(abund[mn])
    avgHSM[[y]] <- raster::calc(MNS, fun = mean, na.rm = T)
  } #end for
  avgHSM <- stack(avgHSM)
  names(avgHSM) <- month.abb
  pHSM <- avgHSM

  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/output_rasters/ENSEMBLE_2020_2029.RData'
  )) #abund

  #avg ensemble HSM
  avgHSM <- vector(mode = 'list', length = 12)
  for (y in 1:12) {
    mn <- seq(y, length(abund), by = 12)
    MNS <- raster::stack(abund[mn])
    avgHSM[[y]] <- raster::calc(MNS, fun = mean, na.rm = T)
  } #end for
  avgHSM <- stack(avgHSM)
  names(avgHSM) <- month.abb
  fHSM <- avgHSM

  diffHSM <- fHSM - pHSM
  diffHSM <- replace(diffHSM, abs(bathyR) > 1000, NA)

  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'figures'),
      '/change_sdm_1993_2019_v_2020_2029.pdf'
    ),
    width = 8,
    height = 11
  )
  par(mfrow = c(4, 3), mar = c(3, 3, 1, 0))
  for (y in 1:12) {
    plot(
      raster::subset(diffHSM, y),
      zlim = c(-0.5, 0.5),
      col = cmocean('balance')(64),
      legend = F
    )
    plot(coastCropped['id'], col = 'grey', add = T)
    legend('topleft', bty = 'n', legend = month.abb[y], cex = 2)
  }
  image.plot(
    matrix(seq(-0.5, 0.5, by = 0.1), 11, 11),
    legend.only = T,
    horizontal = T,
    legend.shrink = 0.7,
    smallplot = c(0.4, 0.8, 0.25, 0.35),
    legend.args = list(
      text = 'Change in \nProbability',
      cex = 0.75,
      side = 3,
      line = 0.1
    ),
    axis.args = list(cex.axis = 1, mgp = c(3, 0.5, 0)),
    col = cmocean('balance')(64)
  )
  dev.off()

  print(x)
  #print(quantile(fHSM[]-pHSM[], na.rm = T, seq(0,1,0.1)))
}

#contemporary - decade 2
for (x in 1:nrow(spp.list)) {
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/output_rasters/ENSEMBLE_1993_2019.RData'
  )) #abund

  #avg ensemble HSM
  avgHSM <- vector(mode = 'list', length = 12)
  for (y in 1:12) {
    mn <- seq(y, length(abund), by = 12)
    MNS <- raster::stack(abund[mn])
    avgHSM[[y]] <- raster::calc(MNS, fun = mean, na.rm = T)
  } #end for
  avgHSM <- stack(avgHSM)
  names(avgHSM) <- month.abb
  pHSM <- avgHSM

  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/output_rasters/ENSEMBLE_2025_2034.RData'
  )) #abund

  #avg ensemble HSM
  avgHSM <- vector(mode = 'list', length = 12)
  for (y in 1:12) {
    mn <- seq(y, length(abund), by = 12)
    MNS <- raster::stack(abund[mn])
    avgHSM[[y]] <- raster::calc(MNS, fun = mean, na.rm = T)
  } #end for
  avgHSM <- stack(avgHSM)
  names(avgHSM) <- month.abb
  fHSM <- avgHSM

  diffHSM <- fHSM - pHSM
  diffHSM <- replace(diffHSM, abs(bathyR) > 1000, NA)

  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'figures'),
      '/change_sdm_1993_2019_v_2025_2034.pdf'
    ),
    width = 8,
    height = 11
  )
  par(mfrow = c(4, 3), mar = c(3, 3, 1, 0))
  for (y in 1:12) {
    plot(
      raster::subset(diffHSM, y),
      zlim = c(-0.5, 0.5),
      col = cmocean('balance')(64),
      legend = F
    )
    plot(coastCropped['id'], col = 'grey', add = T)
    legend('topleft', bty = 'n', legend = month.abb[y], cex = 2)
  }
  image.plot(
    matrix(seq(-0.5, 0.5, by = 0.1), 11, 11),
    legend.only = T,
    horizontal = T,
    legend.shrink = 0.7,
    smallplot = c(0.4, 0.8, 0.25, 0.35),
    legend.args = list(
      text = 'Change in \nProbability',
      cex = 0.75,
      side = 3,
      line = 0.1
    ),
    axis.args = list(cex.axis = 1, mgp = c(3, 0.5, 0)),
    col = cmocean('balance')(64)
  )
  dev.off()

  print(x)
}

#####radar plots for importance
library(fmsb)
range01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
for (s in 37:nrow(spp.list)) {
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[s],
    '/pa_clean.RData'
  )) #dfC
  flist <- dir(
    paste0(
      '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
      spp.list$Name[s],
      '/model_output/importance'
    ),
    full.names = T
  )
  flistClean <- dir(
    paste0(
      '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
      spp.list$Name[s],
      '/model_output/importance/'
    ),
    full.names = F
  )

  #set up data frame
  vars <- names(dfC)[-c(1:3)]
  dfI <- as.data.frame(matrix(nrow = length(flist), ncol = length(vars)))
  colnames(dfI) <- vars

  for (x in 1:length(flist)) {
    load(flist[x]) #imp
    if (class(imp) == 'data.frame') {
      imp.vec <- imp$rel.inf ### need to remove spatial variables
      names(imp.vec) <- imp$var
      for (y in 1:length(names(imp.vec))) {
        if (names(imp.vec)[y] %in% vars) {
          i <- which(vars == names(imp.vec)[y])
          dfI[x, i] <- imp.vec[y]
        }
      }
    } else {
      #imp <- range01(imp)
      for (y in 1:length(names(imp))) {
        if (names(imp)[y] %in% vars) {
          i <- which(vars == names(imp)[y])
          dfI[x, i] <- imp[y]
        }
      }
    }
    #print(x)
  }

  rownames(dfI) <- gsub('.RData', '', flistClean)

  dfI <- replace(dfI, is.na(dfI), 0)

  dfI <- t(apply(dfI, 1, FUN = function(x) {
    x / sum(x)
  }))

  dfI <- rbind(rep(max(dfI, na.rm = T), ncol(dfI)), rep(0, ncol(dfI)), dfI)

  pal <- brewer.pal(n = 5, 'Set1')

  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[s], 'figures'),
      '/variable_importance_radars.pdf'
    ),
    width = 8,
    height = 11
  )
  par(mfrow = c(2, 1), mar = c(1, 4, 1, 4))
  #for(x in 3:nrow(dfI)){
  radarchart(
    as.data.frame(dfI),
    pfcol = alpha(pal, 0.1),
    pty = 15:19,
    pcol = alpha(pal, 1),
    plty = 1,
    title = "Component Models",
    vlcex = 1.25
  )
  legend(
    'topleft',
    legend = rownames(dfI)[3:7],
    lty = 1,
    col = pal,
    pch = 15:19,
    bty = 'n'
  )
  #}
  #add weighted mean from ensemble
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[s],
    '/model_output/ensemble_weights.RData'
  ))
  dfW <- dfI[-c(1, 2), ]
  ws <- apply(dfW, MARGIN = 2, FUN = weighted.mean, w = weights, na.rm = T)
  ws <- rbind(dfI[1:2, ], ws)
  radarchart(
    as.data.frame(ws),
    pfcol = alpha(pal, 0.5),
    pty = 19,
    pcol = alpha(pal, 1),
    plty = 1,
    title = 'ENSEMBLE',
    vlcex = 1.25
  )
  dev.off()
  print(s)
} #end s

##### residuals
#contemporary
for (x in 37:nrow(spp.list)) {
  #model predictions
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/output_rasters/ENSEMBLE_1993_2019.RData'
  )) #abund
  abund <- stack(abund)

  #observations
  obs <- stack(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/input_rasters/combined_rasters_1993_2019.nc'
  ))

  #manipulate obs a bit to clean it up
  names(obs) <- names(abund)
  obsC <- crop(obs, extent(abund))
  obsC[obsC == 0] <- NA
  obsC[obsC == 1] <- 0
  obsC[obsC == 2] <- 1

  resids <- obsC - abund

  #avg residuals
  avgR <- vector(mode = 'list', length = 12)
  for (y in 1:12) {
    mn <- seq(y, nlayers(resids), by = 12)
    MNS <- raster::subset(resids, mn)
    avgR[[y]] <- raster::calc(MNS, fun = mean, na.rm = T)
  } #end for
  avgR <- stack(avgR)
  names(avgR) <- month.abb

  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'figures'),
      '/mean_residuals_1993_2019.pdf'
    ),
    width = 8,
    height = 11
  )
  par(mfrow = c(4, 3), mar = c(3, 3, 1, 0))
  for (y in 1:12) {
    plot(
      raster::subset(avgR, y),
      zlim = c(-1, 1),
      col = cmocean('balance')(64),
      legend = F
    )
    plot(coastCropped['id'], col = 'grey', add = T)
    legend('topleft', bty = 'n', legend = month.abb[y], cex = 2)
  }
  image.plot(
    matrix(seq(-1, 1, by = 0.1), 11, 11),
    legend.only = T,
    horizontal = T,
    legend.shrink = 0.7,
    smallplot = c(0.4, 0.8, 0.25, 0.35),
    legend.args = list(text = 'Residuals', cex = 0.75, side = 3, line = 0.1),
    axis.args = list(cex.axis = 1, mgp = c(3, 0.5, 0)),
    col = cmocean('balance')(64)
  )
  dev.off()

  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'figures'),
      '/histogram_residuals_1993_2019.pdf'
    ),
    width = 6,
    height = 6
  )
  hist(resids[], main = '', xlab = 'Residuals')
  dev.off()

  print(x)
}

##### model weights/aucs
metrics <- read.csv('species_evaluation_metrics.csv')
metrics$Name <- gsub(' ', '', metrics$Common.Name)

for (x in 37:nrow(metrics)) {
  m <- as.matrix(metrics[x, c(12:16)])

  pdf(
    paste0(
      file.path(getwd(), metrics$Name[x], 'figures'),
      '/component_model_weights.pdf'
    ),
    width = 6,
    height = 6
  )
  barplot(
    m,
    names = c("BRT", 'GAM', 'MAXENT', 'RF', 'SDMTMB'),
    ylab = 'Weight',
    xlab = 'Component Model',
    ylim = c(0, 0.3)
  )
  box()
  dev.off()

  aucs <- as.matrix(metrics[x, c(7:11)])
  pdf(
    paste0(
      file.path(getwd(), metrics$Name[x], 'figures'),
      '/component_model_aucs.pdf'
    ),
    width = 6,
    height = 6
  )
  barplot(
    aucs,
    names = c("BRT", 'GAM', 'MAXENT', 'RF', 'SDMTMB'),
    ylab = 'AUC',
    xlab = 'Component Model',
    ylim = c(0, 1)
  )
  box()
  dev.off()

  print(x)
  # print(m)
}


##gifs
library(gifski)
#contemporary
for (x in 37:nrow(spp.list)) {
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/output_rasters/ENSEMBLE_1993_2019.RData'
  )) #abund

  abund <- stack(abund)
  abund <- replace(abund, abs(bathyR) > 1000, NA)

  save_gif(
    expr = for (y in 1:nlayers(abund)) {
      plot(
        raster::subset(abund, y),
        zlim = c(0, 1),
        col = cmocean('matter')(64),
        legend = F
      )
      plot(coastCropped['id'], col = 'grey', add = T)
      legend('topleft', bty = 'n', legend = names(abund)[y], cex = 2)
      image.plot(
        matrix(seq(0, 1, by = 0.1), 11, 11),
        legend.only = T,
        horizontal = T,
        legend.shrink = 0.7,
        smallplot = c(0.4, 0.8, 0.15, 0.20),
        legend.args = list(
          text = 'Probability of Occurance',
          cex = 1.25,
          side = 3,
          line = 0.1
        ),
        axis.args = list(cex.axis = 1, mgp = c(3, 0.5, 0)),
        col = cmocean('matter')(64)
      )
    },
    width = 720,
    height = 720,
    delay = 0.5,
    loop = T,
    progress = T,
    gif_file = paste0(
      file.path(getwd(), spp.list$Name[x], 'figures'),
      '/mean_SDM_1993_2019.gif'
    )
  )

  print(x)
}

#2025-2035
for (x in 37:nrow(spp.list)) {
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/output_rasters/ENSEMBLE_2025_2034.RData'
  )) #abund

  abund <- stack(abund)
  abund <- replace(abund, abs(bathyR) > 1000, NA)

  save_gif(
    expr = for (y in 1:nlayers(abund)) {
      plot(
        raster::subset(abund, y),
        zlim = c(0, 1),
        col = cmocean('matter')(64),
        legend = F
      )
      plot(coastCropped['id'], col = 'grey', add = T)
      legend('topleft', bty = 'n', legend = names(abund)[y], cex = 2)
      image.plot(
        matrix(seq(0, 1, by = 0.1), 11, 11),
        legend.only = T,
        horizontal = T,
        legend.shrink = 0.7,
        smallplot = c(0.4, 0.8, 0.15, 0.20),
        legend.args = list(
          text = 'Probability of Occurance',
          cex = 1.25,
          side = 3,
          line = 0.1
        ),
        axis.args = list(cex.axis = 1, mgp = c(3, 0.5, 0)),
        col = cmocean('matter')(64)
      )
    },
    width = 720,
    height = 720,
    delay = 0.5,
    loop = T,
    progress = T,
    gif_file = paste0(
      file.path(getwd(), spp.list$Name[x], 'figures'),
      '/mean_SDM_2025_2034.gif'
    )
  )

  print(x)
}


##############################

##############################
##### PLOT ENSEMBLE - NEW  ###
##############################

#get bathymetry for plotting
statics <- terra::rast('./Data/staticVariables_cropped_terra_reproj.tif')
bathy <- statics$bathy

#get coastline for plotting
land <- terra::vect('../shpfiles/gshhg-shp-2.3.7/GSHHS_shp/i/GSHHS_i_L1.shp')
landNE <- terra::crop(land, bathy)

metrics <- read.csv('species_evaluation_metrics.csv')


var.list <- data.frame(
  Long.Name = c(
    'Bottom Temperature',
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


make_sdm_plots(
  species = spp.list$Name[1],
  type = c('importance'),
  release = 'r20250715',
  spatial_temporal = F,
  mask_bathy = T,
  rm_corr = T,
  training_years = c(1993, 2019),
  coastline = landNE,
  model_metrics = metrics,
  var_names = var.list$Short.Name
)

##gifs - same as above, if desired, they can be integrated into the plotting function
library(gifski)
#contemporary 93-2019
for (x in 37:nrow(spp.list)) {
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/output_rasters/ENSEMBLE_1993_2019.RData'
  )) #abund

  abund <- stack(abund)
  abund <- replace(abund, abs(bathyR) > 1000, NA)

  save_gif(
    expr = for (y in 1:nlayers(abund)) {
      plot(
        raster::subset(abund, y),
        zlim = c(0, 1),
        col = cmocean('matter')(64),
        legend = F
      )
      plot(coastCropped['id'], col = 'grey', add = T)
      legend('topleft', bty = 'n', legend = names(abund)[y], cex = 2)
      image.plot(
        matrix(seq(0, 1, by = 0.1), 11, 11),
        legend.only = T,
        horizontal = T,
        legend.shrink = 0.7,
        smallplot = c(0.4, 0.8, 0.15, 0.20),
        legend.args = list(
          text = 'Probability of Occurance',
          cex = 1.25,
          side = 3,
          line = 0.1
        ),
        axis.args = list(cex.axis = 1, mgp = c(3, 0.5, 0)),
        col = cmocean('matter')(64)
      )
    },
    width = 720,
    height = 720,
    delay = 0.5,
    loop = T,
    progress = T,
    gif_file = paste0(
      file.path(getwd(), spp.list$Name[x], 'figures'),
      '/mean_SDM_1993_2019.gif'
    )
  )

  print(x)
}

#2025-2035
for (x in 37:nrow(spp.list)) {
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/output_rasters/ENSEMBLE_2025_2034.RData'
  )) #abund

  abund <- stack(abund)
  abund <- replace(abund, abs(bathyR) > 1000, NA)

  save_gif(
    expr = for (y in 1:nlayers(abund)) {
      plot(
        raster::subset(abund, y),
        zlim = c(0, 1),
        col = cmocean('matter')(64),
        legend = F
      )
      plot(coastCropped['id'], col = 'grey', add = T)
      legend('topleft', bty = 'n', legend = names(abund)[y], cex = 2)
      image.plot(
        matrix(seq(0, 1, by = 0.1), 11, 11),
        legend.only = T,
        horizontal = T,
        legend.shrink = 0.7,
        smallplot = c(0.4, 0.8, 0.15, 0.20),
        legend.args = list(
          text = 'Probability of Occurance',
          cex = 1.25,
          side = 3,
          line = 0.1
        ),
        axis.args = list(cex.axis = 1, mgp = c(3, 0.5, 0)),
        col = cmocean('matter')(64)
      )
    },
    width = 720,
    height = 720,
    delay = 0.5,
    loop = T,
    progress = T,
    gif_file = paste0(
      file.path(getwd(), spp.list$Name[x], 'figures'),
      '/mean_SDM_2025_2034.gif'
    )
  )

  print(x)
}


##############################

##############################
##### MAKE SDM REPORTS  ######
##############################
library(tinytex)
#if you get an error that a Tex distribution isn't available, re-run this:
#tinytex::install_tinytex()
#can happen if container needs a reboot

### load in metrics, feeding and habitat guilds
metrics <- read.csv('species_evaluation_metrics.csv')
feeding <- read.csv('feeding_guilds.csv')
habitat <- read.csv('habitat_guilds.csv')

### set up variable dataframe
varDF <- data.frame(
  Long.Name = c(
    'Bottom Temperature',
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

### set up data source dataframe
sourceDF <- data.frame(
  Long.Name = c(
    'NEFSC Bottom Trawl',
    'NEFSC Observer Program',
    'Maine-New Hampshire Inshore Trawl Survey',
    'Massachusetts Division of Marine Fisheries Bottom Trawl Survey',
    'Long Island Sound Trawl Survey',
    'New York Nearshore Trawl Survey',
    'New Jersey Ocean Stock Assessment Survey',
    'Delaware State Trawl Surveys',
    'Northeast Area Monitoring and Assessment Program (NEAMAP) & Chesapeake Bay Multispecies Monitoring and Assessment Program (ChesMMAP)',
    'Additional trawl and tagging data from the Highly Migratory Species Program',
    'NEFSC Gulf of Maine Long Line Survey',
    'NEFSC Northern Shrimp Survey',
    'SEFSC Gillnet Observer Program',
    'SEFSC Pelagic Observer Program',
    'SEFSC Logbook Program',
    'NMFS Large Pelagics Survey',
    'Northeast Area Monitoring and Assessment Program (NEAMAP) & Chesapeake Bay Multispecies Monitoring and Assessment Program (ChesMMAP)',
    'Massachusetts Division of Marine Fisheries Bottom Trawl Survey',
    'NEFSC Atlantic Surfclam and Ocean Quahog Survey'
  ),
  Short.Name = c(
    'Survey',
    'Observer',
    'MENH',
    'MA',
    'CT',
    'NY',
    'NJ',
    'DE',
    'NEAMAP',
    "HMS",
    'GOM LL',
    'Shrimp',
    'GOP',
    'POP',
    "LOGBOOK",
    "LPS",
    'NEAMAP-BFT',
    'MA-BFT',
    "Clam"
  )
)

#load in source key
sources <- read.csv('sources.csv')

#plots are made above - this just pulls them in and renders the report (original function also handled plotting)
make_sdm_reports(
  species_list = spp.list[1,],
  release = 'r20250715',
  model_metrics = metrics,
  sources = sources,
  source_key = sourceDF,
  feeding_key = feeding,
  habitat_key = habitat,
  variable_key = varDF,
  template = 'SDM_report_template.qmd',
  report_path = './Reports'
)


##############################
##### CALCULATE/PLOT MODEL CONFIDENCE ######
##############################
##preliminary
###all submitted csvs should be added to the SDMs/ConfidenceScores/Preliminary folder
### make sure to grab only the third tab in the csv - the other two are just instructions; the third one is the only one with data

#create combined data.frame
flist <- dir('./ConfidenceScores/Preliminary/submitted_csvs', pattern = '.csv')
modConf <- NULL
for (x in 1:length(flist)) {
  #load in data frame & clean
  f <- read.csv(flist[x], skip = 2) #remove header when loading in
  f <- f[, 1:3] #remove key in later columns
  #add scorer column in case you want that information
  fname <- gsub('.csv', '', flist[x])
  f$Scorer <- paste(
    str_split(fname, "_")[[1]][4],
    str_split(fname, "_")[[1]][5],
    sep = '.'
  )

  #append to data.frame
  modConf <- rbind(modConf, f)
}
write.csv(
  modConf,
  file = './ConfidenceScores/Preliminary/raw_combined_scores.csv'
) #save for prosperity (the above should be quick but just in case)

#run model.confidence - similar to sensitivity score workflows with lapply
species.data.list <- split(modConf, modConf$Species)
species.conf <- lapply(species.data.list, model.confidence)
speciesMC <- do.call(rbind, species.conf)
speciesMC$Species <- rownames(speciesMC)
write.csv(speciesMC, file = './ConfidenceScores/Preliminary/mean_sd_scores.csv')

#make histograms
pdf(
  file = './ConfidenceScores/Preliminary/preliminary_histograms.pdf',
  height = 11,
  width = 8
)
par(mfrow = c(3, 2))
for (x in 1:nrow(speciesMC)) {
  #pull data from mean/sd scores data.frame
  m <- speciesMC[x, 'meanConfidence']
  s <- speciesMC[x, 'sdConfidence']
  sp <- speciesMC[x, 'Species']

  #use species.data.list to subset since that's already done and the list is in the same order as the summary stats spreadsheet
  spDF <- species.data.list[[x]]

  #plot histogram
  hist(
    spDF$Scores,
    xlim = c(0, 3),
    breaks = seq(0, 3, by = 1),
    ylim = c(0, 5),
    xlab = '', #make sure breaks and limits are consistent
    main = paste0(sp, '\nMean: ', m, " | SD: ", s)
  ) #add species, and summary stats to main
}
dev.off()
