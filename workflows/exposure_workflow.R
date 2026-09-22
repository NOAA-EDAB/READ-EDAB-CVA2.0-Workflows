###calculate exposure w/ functions

##################################
#####SET UP - LOAD EVERY TIME ####
##################################

setwd(here::here('Exposure'))
#load in package
library(spatialcva)

#load additional packages needed for workflows
library(future)
library(furrr)
library(logger)

#load species list for loops
spp.list <- read.csv(
  '../SDMs/spp_list.csv'
)
#spp.list <- spp.list[,c(1:6)]
spp.list$Name <- gsub(' ', '', spp.list$Common.Name) #make clean names to make folders if necessary/match to folder names
#save yourself the headache and remove the one that fails
spp.list <- spp.list[-42, ]

#make directory for each species if it doesn't exist; if directory exists, it is not changed
for (x in 1:nrow(spp.list)) {
  dir.create(
    file.path(here::here('Exposure'), spp.list$Name[x]),
    showWarnings = T
  ) #main species folder
  dir.create(
    file.path(here::here('Exposure'), spp.list$Name[x], 'Data'),
    showWarnings = T
  ) #data folder
  dir.create(
    file.path(here::here('Exposure'), spp.list$Name[x], 'Figures'),
    showWarnings = T
  ) #figures folder
}
##################################

##################################
### calculate & rank exposure
##################################

#load in variables
var.names <- c(
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

#load in bathy for masking
staticR <- terra::rast('../SDMs/Data/staticVariables_cropped_terra_reproj.tif') #staticR
#bathy object = staticR$bathy
bathy <- terra::wrap(staticR$bathy) #this is required because of the way terra holds rasters in memory and how things are distributed in parallel with future_map; the bathy raster gets unwrapped within the wrapper function


#only needs to be done once for each time period
#2014-2023 v 2025-2035
for (x in var.names) {
  hindcast_path <- paste0(
    '../SDMs/Data/MOM6/raw_MOM6_',
    x,
    '_hindcast_r20250715_global.tif'
  )
  hindcast <- terra::rast(hindcast_path)
  hindcast <- hindcast[[253:372]] #last ten years of hindcast (2014-2023)

  #get years from names to help with naming output
  yrs <- as.numeric(sub(".*\\.", "", names(hindcast)))

  forecast_path <- paste0(
    '../SDMs/Data/MOM6/raw_MOM6_',
    x,
    '_forecast_r20250925_i202501_global_average.tif'
  )
  forecast <- terra::rast(forecast_path)

  #raw exposure
  raw_exp <- calculate_raw_exposure(
    present = hindcast,
    future = forecast,
    spatial_temporal = T,
    mask_bathy = T,
    bathy = bathy,
    bathy_range = c(-1000, 0)
  )
  terra::writeRaster(
    raw_exp,
    filename = paste0(
      './RawExposure/Data/',
      x,
      '_rawexposure_r20250925_i202501_r20250715_',
      min(yrs),
      max(yrs),
      '_global.tif'
    ),
    overwrite = T
  )

  #rank exposure
  ranked_exp <- rank_exposure(
    exposure = raw_exp,
    flip = !(x %in% c('bottomT', 'surfaceT', 'bottomArg', 'MLD'))
  ) #if x is one of these names, set flip to F; if not, flip will be T
  terra::writeRaster(
    ranked_exp,
    filename = paste0(
      './RawExposure/Data/',
      x,
      '_rankedexposure_r20250925_i202501_r20250715_',
      min(yrs),
      max(yrs),
      '_global.tif'
    ),
    overwrite = T
  )
}

#make and save nice plots of raw exposure, ranked exposure, and climatologies
#climatologies
#2014-2023
for (x in var.names) {
  hindcast_path <- paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/Data/MOM6/raw_MOM6_',
    x,
    '_hindcast_r20250715_global.tif'
  )
  hindcast <- terra::rast(hindcast_path)
  hindcast <- hindcast[[253:372]] #last ten years of hindcast (2014-2023)

  avgs <- terra::tapp(
    hindcast,
    rep(1:12, times = terra::nlyr(hindcast) / 12),
    fun = 'mean'
  )

  pdf(
    paste0(
      './RawExposure/Figures/climatologies/',
      x,
      '_climatology_hindcast_r20250715.pdf'
    ),
    width = 11,
    height = 8
  )
  terra::plot(avgs, main = month.abb, range = range(avgs[], na.rm = T))
  dev.off()
  print(x)
}

#2025-2035
for (x in var.names) {
  forecast_path <- paste0(
    '../SDMs/Data/MOM6/raw_MOM6_',
    x,
    '_forecast_r20250925_i202501_global_average.tif'
  )
  forecast <- terra::rast(forecast_path)

  avgs <- terra::tapp(
    forecast,
    rep(1:12, times = terra::nlyr(forecast) / 12),
    fun = 'mean'
  )

  pdf(
    paste0(
      './RawExposure/Figures/climatologies/',
      x,
      '_climatology_forecast_r20250925_i202501.pdf'
    ),
    width = 11,
    height = 8
  )
  terra::plot(avgs, main = month.abb, range = range(avgs[], na.rm = T))
  dev.off()
  print(x)
}

#forecast - hindcast
for (x in var.names) {
  hindcast_path <- paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/Data/MOM6/raw_MOM6_',
    x,
    '_hindcast_r20250715_global.tif'
  )
  hindcast <- terra::rast(hindcast_path)
  hindcast <- hindcast[[253:372]] #last ten years of hindcast (2014-2023)
  hAvg <- avg_model_data(hindcast, spatial_temporal = T)

  forecast_path <- paste0(
    '../SDMs/Data/MOM6/raw_MOM6_',
    x,
    '_forecast_r20250925_i202501_global_average.tif'
  )
  forecast <- terra::rast(forecast_path)

  fAvg <- terra::tapp(
    forecast,
    rep(1:12, times = terra::nlyr(forecast) / 12),
    fun = 'mean'
  )
  fAvg <- terra::resample(fAvg, hAvg, method = "bilinear")

  diff_rast <- fAvg - hAvg

  pdf(
    paste0(
      './RawExposure/Figures/differences/',
      x,
      '_differences.pdf'
    ),
    width = 11,
    height = 8
  )
  terra::plot(
    diff_rast,
    main = month.abb,
    range = range(diff_rast[], na.rm = T)
  )
  dev.off()
  print(x)
}

#raw exposure
for (x in var.names) {
  raw <- terra::rast(paste0(
    './RawExposure/Data/',
    x,
    '_rawexposure_r20250925_i202501_r20250715_20142023_global.tif'
  ))
  pdf(
    paste0(
      './RawExposure/Figures/raw/',
      x,
      '_exposure.pdf'
    ),
    width = 11,
    height = 8
  )
  terra::plot(raw, main = month.abb, range = range(raw[], na.rm = T))
  dev.off()
  print(x)
}

#ranked exposure
for (x in var.names) {
  ranked <- terra::rast(paste0(
    './RawExposure/Data/',
    x,
    '_rankedexposure_r20250925_i202501_r20250715_20142023_global.tif'
  ))

  # 1. Create a duplicate of your raster specifically for plotting
  plot_ranked <- ranked

  # 2. Define your 4 categories
  categories <- data.frame(id = 1:4, class = as.character(1:4))

  # 3. Apply these categorical levels ONLY to the temporary plot object
  levels(plot_ranked) <- replicate(
    terra::nlyr(plot_ranked),
    categories,
    simplify = FALSE
  )

  pdf(
    paste0(
      './RawExposure/Figures/ranked/',
      x,
      '_ranked_exposure.pdf'
    ),
    width = 11,
    height = 8
  )
  # 4. Plot the temporary object
  terra::plot(plot_ranked, main = month.abb, range = c(1, 4), all_levels = T)
  dev.off()
  print(x)
}

##################################

##################################
### create stock polygons for all species
##################################
#similar to calculating raw exposure, this should only need to happen once as it saves the shp files
setwd(
  "/home/kgallagher/ClimateVulnerabilityAssessment2.0/shpfiles/species_stock_areas"
)

#get species/stocks/polygons lists
#NEFMC list
nefmc <- read.csv('NEFMC_species_stock_assessment_areas.csv')
#fix some names to match spp.list since that is what the directories are based out of
nefmc$COMMON_NAME <- replace(
  nefmc$COMMON_NAME,
  nefmc$COMMON_NAME == 'American sea scallop',
  'Atlantic sea scallop'
)
nefmc$COMMON_NAME <- replace(
  nefmc$COMMON_NAME,
  nefmc$COMMON_NAME == 'Atlantic menhaden',
  'Atlantic Menhaden'
)
nefmc$COMMON_NAME <- replace(
  nefmc$COMMON_NAME,
  nefmc$COMMON_NAME == 'Atlantic surf clam',
  'Atlantic surfclam'
)
nefmc$COMMON_NAME <- replace(
  nefmc$COMMON_NAME,
  nefmc$COMMON_NAME == 'Blueline Tilefish',
  'Blueline tilefish'
)
nefmc$COMMON_NAME <- replace(
  nefmc$COMMON_NAME,
  nefmc$COMMON_NAME == 'Atlantic chub mackerel',
  'Chub mackerel'
)
nefmc$COMMON_NAME <- replace(
  nefmc$COMMON_NAME,
  nefmc$COMMON_NAME == 'Red drum',
  'Red Drum'
)
nefmc$COMMON_NAME <- replace(
  nefmc$COMMON_NAME,
  nefmc$COMMON_NAME == 'Northern shortfin squid',
  'Shortfin squid'
)
nefmc <- nefmc[-which(nefmc$COMMON_NAME == 'Black seabass'), ]
nefmc$Name <- gsub(' ', '', nefmc$COMMON_NAME)

#Black Sea Bass from MAFMC
bsb <- read.csv('BSB_Assessment_StockAreas.csv')
#add/rename columns to bsb to match nefmc
bsb$Name <- 'Blackseabass'
bsb$AREA <- bsb$STOCK_AREA
bsb$ASSESSMENT_STOCK_AREA <- bsb$STOCK_ABBREV

#combine all stock keys
stock_key <- rbind(
  nefmc[, c('Name', "ASSESSMENT_STOCK_AREA", 'AREA')],
  bsb[, c('Name', "ASSESSMENT_STOCK_AREA", 'AREA')]
)
#remove species with UNIT stocks  or with NAs as a result of the match; both indicate that there are no subunits
stock_key <- stock_key[
  -which(
    stock_key$ASSESSMENT_STOCK_AREA == 'UNIT' |
      is.na(stock_key$ASSESSMENT_STOCK_AREA)
  ),
]

#merge with spp.list to subset to just species list
stock_key <- merge(stock_key, spp.list, by = 'Name', all.x = F, all.y = T)

##load in CAM polygons
stat_areas <- terra::vect('../NEFSC_GIS/Statistical_Areas_2010_withNames.shp')

#get bathymetry for plotting
statics <- terra::rast('../SDMs/Data/staticVariables_cropped_terra_reproj.tif')
bathy <- statics$bathy

#get coastline for plotting
land <- terra::vect('../shpfiles/gshhg-shp-2.3.7/GSHHS_shp/i/GSHHS_i_L1.shp')
landNE <- terra::crop(land, bathy)

make_stock_polygons(
  key = stock_key,
  species_col = 'Name',
  stock_col = 'ASSESSMENT_STOCK_AREA',
  id_col = 'AREA',
  polygons = stat_areas,
  poly_id = 'Id',
  plot = T,
  bathymetry = bathy,
  coastline = landNE
)

##################################

##################################
### calculate variable exposure
##################################

#load in variables
var.names <- c(
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


#2014-23 v 2025 - 2035
plan(multisession, workers = 8)
combs <- future_map(
  1:nrow(spp.list),
  ~ variable_exposures_wrapper(
    spp = spp.list$Name[.x],
    forecast_release = 'r20250925',
    forecast_init = 'i202501',
    hindcast_release = 'r20250715',
    hindcast_yr_range = '20142023',
    spatial_temporal = FALSE,
    mask_bathy = T,
    rm_corr = T,
    dyn_vars = var.names,
    training_years = c(1993, 2019)
  ),
  .progress = T,
  .options = furrr_options(scheduling = FALSE)
)
plan(sequential)

##################################

##################################
### Make Total Exposure
##################################

#2009-2019 v 2025 - 2035
plan(multisession, workers = 8)
combs <- future_map(
  1:nrow(spp.list),
  ~ total_exposures_wrapper(
    spp = spp.list$Name[.x],
    forecast_release = 'r20250925',
    forecast_init = 'i202501',
    hindcast_release = 'r20250715',
    hindcast_yr_range = '20142023'
  ),
  .progress = T,
  .options = furrr_options(scheduling = FALSE)
)
plan(sequential)

##################################

##################################
### Plot Results
##################################

#get bathymetry for plotting
statics <- terra::rast('../SDMs/Data/staticVariables_cropped_terra_reproj.tif')
bathy <- statics$bathy

#get coastline for plotting
land <- terra::vect('../shpfiles/gshhg-shp-2.3.7/GSHHS_shp/i/GSHHS_i_L1.shp')
landNE <- terra::crop(land, bathy)

### set up variable dataframe to make pretty names - not exact MOM6 names to make sure they fit
varDF <- data.frame(
  Long.Name = c(
    'Bottom Temperature',
    'Bottom Oxygen',
    'Bottom Salinity',
    'Bottom Aragonite Solubility',
    'Sea Surface Temperature',
    'Sea Surface Salinity',
    'Surface pH',
    'Mixed layer depth\n(delta rho = 0.03)',
    'Diazotroph\nintegrated prim. prod.',
    'Small phyto.\nintegrated prim. prod.',
    'Medium phyto.\nintegrated prim. prod.',
    'Large phyto.\nintegrated prim. prod.',
    'Small zooplankton\nintegrated biomass',
    'Medium zooplankton\nintegrated biomass',
    'Large zooplankton\nintegrated biomass',
    'Net primary production',
    'Downward Flux of\nParticulate Organic Carbon'
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


make_exposure_plots(
  species = spp.list$Name,
  type = c('variable', 'total', 'important', 'radar'),
  forecast_release = 'r20250925',
  forecast_init = 'i202501',
  hindcast_release = 'r20250715',
  hindcast_yr_range = '20142023',
  variable_df = varDF,
  coastline = landNE,
  bathymetry = bathy
)

## make exposure summary tables
makereport_exposuretable(
  species = spp.list$Name,
  presentTime = '2009-2019',
  futureTime = '2025-2035',
  variableDF = varDF
)

##################################
