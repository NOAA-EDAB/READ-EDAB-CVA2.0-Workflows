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

##clam survey
clam <- standardize_fisheries_data(
  data_type = 'CSV',
  csv = './Data/csvs/raw/NEFSC_Clam_072026.csv',
  csv_columns = c(
    'towID',
    'DECDEG_BEGLON',
    'DECDEG_BEGLAT',
    'BEGIN_EST_TOWDATE',
    'EXPCATCHNUM',
    'SCI_NAME'
  ),
  yr_range = c(1993, 2023)
)
write.csv(clam, './Data/csvs/standardized/Clam_1993_2023.csv')


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


##############################

###################################
##### BUILD FISHERIES DATA FRAMES #
###################################
#load new key document to determine which sources should be used for which species
source.key <- read.csv('sources.csv')
source.names <- colnames(source.key)[-1] #isolate source names
is.obs.key <- c(F, T, F, F, F, F, F, F, F, T, F, F, T, T, T, T, F, F, F) #flags which sources are observer based (ie fisheries dependent datasets)

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

###example of just one - adding clam dredge survey
args <- args[args$source == 'Clam_1993_2023',]

plan(multisession, workers = 8)
checks <- future_map(
  1:nrow(args),
  ~ save_df_wrapper(
    csv_name = 'Clam_1993_2023',
    spp = args$spp[.x],
    spp_names = args$all_names[.x],
    is_obs = F,
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
  print(table(r$pa))
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

statics <- terra::wrap(terra::rast('./Data/staticVariables_masked_norm_terra.tif'))

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

#running each model type seperately to help with troubleshooting if needed; plus sdmtmb needs more memory to using less cores in parallel
#going in increasing order of time needed to run

### can pass spp.list$Name directly to future_map, or a subsetted list of names like below to run just a few species
## this subset was made to re-run groundfish and benthic species after the addition of the clam survey dataset
#sppnames <- spp.list$Name[which(spp.list$Habitat.Guild == 'Groundfish' | spp.list$Habitat.Guild == "Benthic")]

#RF
#started: 12:12 PM 7/22
#ended: 3:58 PM 7/22
#runtime: 3 hrs 46 min  = average 40 min per species
#notes: softshell clam failed due to lack of presence data
plan(multisession, workers = 8)
combs <- future_map(
  1:length(sppnames),
  ~component_sdms_wrapper(spp = sppnames[.x],
                          model = 'rf',
                          dyn_names = var.list$Short.Name,
                          release = 'r20250715',
                          spatial_temporal = FALSE,
                          mask_bathy = T,
                          rm_corr = T,
                          static_variables = statics,
                          training_years = c(1993, 2019),
                          test_years = c(2020, 2023),
                          all_years = c(1993, 2035),
                          skip = F),
  .progress = T
)
plan(sequential)


#BRT
#started: 8:42 AM 7/27
#ended: (approx) 13:00 PM 7/28 (stopped with one model left to go due to adding clam survey)
#runtime: (approx 28 hrs)
plan(multisession, workers = 8)
combs <- future_map(
  1:length(sppnames),
  ~component_sdms_wrapper(spp = sppnames[.x],
                          model = 'brt',
                          dyn_names = var.list$Short.Name,
                          release = 'r20250715',
                          spatial_temporal = FALSE,
                          mask_bathy = T,
                          rm_corr = T,
                          static_variables = statics,
                          training_years = c(1993, 2019),
                          test_years = c(2020, 2023),
                          all_years = c(1993, 2035),
                          skip = F),
  .progress = T
)
plan(sequential)


#GAM
#started: 4:55 PM 7/22
#ended: 7:45 AM 7/26
#runtime: 86.8 hrs (3.6 days) = average 14.2 hrs per species but a lot of variability
#softshell clam also failed here.
plan(multisession, workers = 8)
combs <- future_map(
  1:length(sppnames),
  ~component_sdms_wrapper(spp = sppnames[.x],
                          model = 'gam',
                          dyn_names = var.list$Short.Name,
                          release = 'r20250715',
                          spatial_temporal = FALSE,
                          mask_bathy = T,
                          rm_corr = T,
                          static_variables = statics,
                          training_years = c(1993, 2019),
                          test_years = c(2020, 2023),
                          all_years = c(1993, 2035),
                          skip = F),
  .progress = T
)
plan(sequential)

#sdmtmb
#runtime:
sppnames <- spp.list$Name[c(7,8,12,22,32)]
plan(multisession, workers = 4)
combs <- future_map(
  1:length(sppnames), #cod was used as a test to troubleshoot new year_range/all_years arguments, so not re-running that one 
  ~component_sdms_wrapper(spp = sppnames[.x],
                          model = 'sdmtmb',
                          dyn_names = var.list$Short.Name,
                          release = 'r20250715',
                          spatial_temporal = FALSE,
                          mask_bathy = T,
                          rm_corr = T,
                          static_variables = statics,
                          training_years = c(1993, 2019),
                          test_years = c(2020, 2023),
                          all_years = c(1993, 2035),
                          skip = T),
  .progress = T
)
plan(sequential)

#maxent
#runtime:
plan(multisession, workers = 8)
combs <- future_map(
  1:nrow(spp.list),
  ~component_sdms_wrapper(spp = spp.list$Name[.x],
                          model = 'maxent',
                          dyn_names = var.list$Short.Name,
                          release = 'r20250715',
                          spatial_temporal = FALSE,
                          mask_bathy = T,
                          rm_corr = T,
                          static_variables = statics,
                          training_years = c(1993, 2019),
                          test_years = c(2020, 2023),
                          all_years = c(1993, 2035),
                          skip = F),
  .progress = T
)
plan(sequential)

##combined approach to get both sdmtmb finished and maxtent started 
sppnames <- spp.list$Name[c(7,8,12,22,32)]
sdmtmb <- data.frame(spp = sppnames, mod = 'sdmtmb')

maxent <- data.frame(spp = spp.list$Name, mod = 'maxent')

runs <- rbind(sdmtmb, maxent)

plan(multisession, workers = 8)
combs <- future_map(
  1:nrow(runs),
  ~component_sdms_wrapper(spp = runs$spp[.x],
                          model = runs$mod[.x],
                          dyn_names = var.list$Short.Name,
                          release = 'r20250715',
                          spatial_temporal = FALSE,
                          mask_bathy = T,
                          rm_corr = T,
                          static_variables = statics,
                          training_years = c(1993, 2019),
                          test_years = c(2020, 2023),
                          all_years = c(1993, 2035),
                          skip = T),
  .progress = T
)
plan(sequential)

#ENSEMBLE
#runtime:
plan(multisession, workers = 8)
combs <- future_map(
  1:nrow(spp.list),
  ~ensemble_sdms_wrapper(spp = spp.list$Name[.x],
                         dyn_names = var.list$Short.Name,
                         release = 'r20250715',
                         spatial_temporal = FALSE,
                         mask_bathy = T,
                         rm_corr = T,
                         static_variables = statics,
                         training_years = c(1993, 2019),
                         test_years = c(2020, 2023),
                         all_years = c(1993, 2035),
                         skip = F),
  .progress = T
)
plan(sequential)

#run on the "side" as models finish up remotely 
sppnames <- spp.list$Name[c(27)]
for(x in 1:length(sppnames)){
  ensemble_sdms_wrapper(spp = sppnames[x],
                        dyn_names = var.list$Short.Name,
                        release = 'r20250715',
                        spatial_temporal = FALSE,
                        mask_bathy = T,
                        rm_corr = T,
                        static_variables = statics,
                        training_years = c(1993, 2019),
                        test_years = c(2020, 2023),
                        skip = T)
}

#evalulate ensemble and combine statistics for model reports
make_evaluation_csv(spp_list = spp.list,
                    training_years = c(1993, 2019),
                    pa_col = 'pa',
                    release = 'r20250715',
                    spatial_temporal = FALSE,
                    mask_bathy = T,
                    rm_corr = T,
                    add_data = F, 
                    additional_data = NULL)

##############################

##########################################
##### PREDICT MODELS TO FORECAST  ########
##########################################

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

#because the forecasts have a lot more data to pull from the servers (300+ timestamps for 10 ensemble members), the servers can get angry and the pulls can fail, especially when you are making a lot of requests at the same time. Since the forecasts aren't necessary until calculating exposure and predicting future habitat change, the forecast pulls can happen over a longer period (aka overnight if you're in between steps, etc), so below is the option to run the code in sequence if you want to do that

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

#second, normalize forecast data to HINDCAST MEAN/SD
norm_forecast <- vector(mode = 'list', length = length(forecast.list$Short.Name))
for(x in 1:length(forecast.list$Short.Name)){
  if(!file.exists(paste0('./Data/MOM6/norm_', forecast.list$Short.Name[x], '_forecast_r20250925_i202501_hindcast_r20250715_global.tif'))){
    #if the normalized file doesn't exist, make it
    raw <- terra::rast('./Data/MOM6/raw_MOM6_', forecast.list$Short.Name[x], '_forecast_r20250925_i202501_global.tif')
    hind_avg <- load('./Data/MOM6/avg_', forecast.list$Short.Name[x], '_hindcast_r20250715_masked_global.rds')
    hind_sd <- load('./Data/MOM6/sd_', forecast.list$Short.Name[x], '_hindcast_r20250715_masked_global.rds')
    
    norm <- (raw - avg) / sd
    terra::writeRaster(norm, filename = paste0('./Data/MOM6/norm_', forecast.list$Short.Name[x], '_forecast_r20250925_i202501_hindcast_r20250715_global.tif'))
  } else{
    #if it does exist, load it in
    norm <- terra::rast(paste0('./Data/MOM6/norm_', forecast.list$Short.Name[x], '_forecast_r20250925_i202501_hindcast_r20250715_global.tif'))
  }
  #add to big list to pass to predictions
  norm_forecast[[x]] <- norm
}

#third, predict models
statics <- terra::wrap(terra::rast('./Data/staticVariables_masked_norm_terra.tif'))
mods <- c("GAM", "MAXENT", "SDMTMB", "RF", "BRT")

for(x in 1:nrow(spp.list)){
#load in training data for each species (needed to predict some models)
 dfT <- read.csv(file.path(getwd(), spp.list$Name[x], 'training_1993_2019_rmcorr_hindcast_r20250715_masked_global.csv'))
 preds <- vector(mode = 'list', length = length(mods))
 #predict component models 
 for(m in 1:length(mods)){
   mod <- load(file.path(getwd(), spp.list$Name[x], 'model_output', 'models', paste0(mods[m], '.rds')))
   p <- make_sdm_predictions(
      mod = mod,
      model = tolower(mods[m]),
      rasts = norm_forecast,
      static_variables = terra::unwrap(static_variables),
      se = dfT,
      pa_col = 'pa',
      month_col = 'month',
      year_col = 'year',
      xy_col = c("grid.lon", "grid.lat")
    )
   #save prediction
   terra::writeRaster(p, file = file.path(getwd(), spp.list$Name[x], 'output_rasters', paste0(mods[m], '_forecast_r20250925_i202501.tif')), overwrite = TRUE)
   #and add to list
   preds[[m]] <- p
 } #end m
 
 #now predict ensemble 
 #load in weights 
 weights <- load(file.path(getwd(), spp.list$Name[x], 'model_output',
                      'ensemble_weights.rds'))
 #predict
 ens <- make_sdm_predictions(
   model = 'ensemble',
   rasts = preds,
   weights = weights 
 )
 #save
 terra::writeRaster(ens, file = file.path(getwd(), spp.list$Name[x], 'output_rasters', 'ENSEMBLE_forecast_r20250925_i202501.tif'), overwrite = TRUE)
} #end p


##########################################

##############################
##### PLOT MODEL RESULTS  ####
##############################
######average ensembles
load(
  "~/ClimateVulnerabilityAssessment2.0/Exposure/RawExposure/Data/coastline.RData"
)
load(
  "~/ClimateVulnerabilityAssessment2.0/SDMs/Data/staticVariables_cropped.RData"
)
bathyR <- staticVars$bathy

metrics <- read.csv('species_evaluation_metrics.csv')

plot_SDMS(
  species = spp.list$Name,
  yrStart = 1993,
  yrEnd = 2019,
  coastline = coastCropped,
  bathy = bathyR,
  model.metrics = metrics
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

#get coastline and bathy objects for plotting
load(
  "~/ClimateVulnerabilityAssessment2.0/Exposure/RawExposure/Data/coastline.RData"
) #coastline
load(
  "~/ClimateVulnerabilityAssessment2.0/SDMs/Data/staticVariables_cropped.RData"
) #staticVars
bathyR <- staticVars$bathy

#plots are made above - this just pulls them in and renders the report (original function also handled plotting)
make_sdm_reports(
  species_list = spp.list,
  yr_min = 1993,
  yr_max = 2019,
  model_metrics = metrics,
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
