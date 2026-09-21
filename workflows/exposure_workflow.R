###calculate exposure w/ functions

##################################
#####SET UP - LOAD EVERY TIME ####
##################################

setwd('/home/kgallagher/ClimateVulnerabilityAssessment2.0/Exposure')
### source functions
library(spatialcva)

#load species list for loops
spp.list <- read.csv(
  '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/spp_list.csv'
)
#spp.list <- spp.list[,c(1:6)]
spp.list$Name <- gsub(' ', '', spp.list$Common.Name) #make clean names to make folders if necessary/match to folder names
#save yourself the headache and remove the one that fails
spp.list <- spp.list[-42, ]

#make directory for each species if it doesn't exist; if directory exists, it is not changed
for (x in 1:nrow(spp.list)) {
  dir.create(file.path(getwd(), spp.list$Name[x]), showWarnings = T) #main species folder
  dir.create(file.path(getwd(), spp.list$Name[x], 'Data'), showWarnings = T) #data folder
  dir.create(file.path(getwd(), spp.list$Name[x], 'Figures'), showWarnings = T) #figures folder
}
##################################

##################################
### calculate & rank exposure
##################################
###############step 1 - calculate exposure
#only needs to be done once for each time period

#1993-08 v 2009-2019
load(
  "/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/Data/MOM6/raw_MOM6_082025.RData"
) #load MOM6 raw data (object name = raw)
#data cleanup
pre <- fut <- vector(mode = 'list', length = length(raw))
for (x in 1:length(raw)) {
  pre[[x]] <- list(raster::subset(raw[[x]][[1]], 1:192))
}

for (x in 1:length(raw)) {
  fut[[x]] <- list(raster::subset(raw[[x]][[1]], 193:324))
}

exp9309 <- calcExposure(pre, fut)
names(exp9309) <- names(raw)
save(exp9309, file = './RawExposure/Data/1993_2008_v_2009_2019_exposure.RData')

#make and save nice plots of each variable
for (x in 1:length(exp9309)) {
  pdf(
    paste0(
      './RawExposure/Figures/1993-2008 vs 2009-19/',
      names(exp9309[x]),
      '_exposure.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(exp9309[[x]], main = month.abb, zlim = range(exp9309[[x]][], na.rm = T))
  dev.off()
  print(x)
}

##plot raw differences - may help with explanations?
#make climatologies (and present sd while we're here )
mPres <- mFut <- sdPres <- vector(mode = 'list', length = length(pre))
for (v in 1:length(pre)) {
  climP <- climF <- sdP <- vector(mode = 'list', length = 12)
  for (x in 1:12) {
    #take mean of 'present' and 'future'
    climP[[x]] <- raster::calc(
      raster::subset(
        pre[[v]][[1]],
        seq(x, raster::nlayers(pre[[v]][[1]]), by = 12)
      ),
      mean
    )
    climF[[x]] <- raster::calc(
      raster::subset(
        fut[[v]][[1]],
        seq(x, raster::nlayers(fut[[v]][[1]]), by = 12)
      ),
      mean
    )

    ##calculate SD
    sdP[[x]] <- raster::calc(
      raster::subset(
        pre[[v]][[1]],
        seq(x, raster::nlayers(pre[[v]][[1]]), by = 12)
      ),
      sd
    )
  }
  mPres[[v]] <- raster::stack(climP)
  mFut[[v]] <- raster::stack(climF)
  sdPres[[v]] <- raster::stack(sdP)
}
#### plot climatologies
#1993-2008
for (x in 1:length(mPres)) {
  pdf(
    paste0(
      './RawExposure/Figures/climatologies/',
      names(raw[x]),
      '_1993_2008_climatology.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(mPres[[x]], main = month.abb, zlim = range(mPres[[x]][], na.rm = T))
  dev.off()
  print(x)
}

#2009-2019
for (x in 1:length(mPres)) {
  pdf(
    paste0(
      './RawExposure/Figures/climatologies/',
      names(raw[x]),
      '_2009_2019_climatology.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(mFut[[x]], main = month.abb, zlim = range(mFut[[x]][], na.rm = T))
  dev.off()
  print(x)
}

###plot differences
for (x in 1:length(mPres)) {
  pdf(
    paste0(
      './RawExposure/Figures/1993-2008 vs 2009-19/differences/',
      names(raw[x]),
      '_difference.pdf'
    ),
    width = 11,
    height = 8
  )
  df <- mFut[[x]] - mPres[[x]]
  plot(df, main = month.abb, zlim = range(df[], na.rm = T))
  dev.off()
  print(x)
}

###plot present standard deviations
for (x in 1:length(sdPres)) {
  pdf(
    paste0(
      './RawExposure/Figures/1993-2008 vs 2009-19/present_sds/',
      names(raw[x]),
      '_sds.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(sdPres[[x]], main = month.abb, zlim = range(sdPres[[x]][], na.rm = T))
  dev.off()
  print(x)
}


################
#2009-2019 v 2020 - 2030
#get 09-19 data
load(
  "/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/Data/MOM6/raw_MOM6_082025.RData"
) #load MOM6 raw data (object name = raw)
#data cleanup
pre <- fut <- vector(mode = 'list', length = length(raw))
for (x in 1:length(raw)) {
  pre[[x]] <- list(raster::subset(raw[[x]][[1]], 193:324))
}
#2020-2030 data
load(
  "/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/Data/MOM6/raw_MOM6_decadalforecast_2020_2030_102025.RData"
) #load MOM6 raw data (object name = raw)
fut <- raw

exp0920 <- calcExposure(pre, fut)
names(exp0920) <- names(raw)
save(exp0920, file = './RawExposure/Data/2009_2019_v_2020_2030_exposure.RData')

#make and save nice plots of each variable
for (x in 1:length(exp0920)) {
  pdf(
    paste0(
      './RawExposure/Figures/2009-19 vs 2020-30/',
      names(exp0920[x]),
      '_exposure.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(exp0920[[x]], main = month.abb, zlim = range(exp0920[[x]][], na.rm = T))
  dev.off()
  print(x)
}

#make climatologies (and present sd while we're here )
mPres <- mFut <- sdPres <- vector(mode = 'list', length = length(pre))
for (v in 1:length(pre)) {
  climP <- climF <- sdP <- vector(mode = 'list', length = 12)
  for (x in 1:12) {
    #take mean of 'present' and 'future'
    climP[[x]] <- raster::calc(
      raster::subset(
        pre[[v]][[1]],
        seq(x, raster::nlayers(pre[[v]][[1]]), by = 12)
      ),
      mean
    )
    climF[[x]] <- raster::calc(
      raster::subset(
        fut[[v]][[1]],
        seq(x, raster::nlayers(fut[[v]][[1]]), by = 12)
      ),
      mean
    )

    ##calculate SD
    sdP[[x]] <- raster::calc(
      raster::subset(
        pre[[v]][[1]],
        seq(x, raster::nlayers(pre[[v]][[1]]), by = 12)
      ),
      sd
    )
  }
  mPres[[v]] <- raster::stack(climP)
  mFut[[v]] <- raster::stack(climF)
  sdPres[[v]] <- raster::stack(sdP)
}
#### plot climatologies (just fut since we already have 09-19)
#2020-2030
for (x in 1:length(mFut)) {
  pdf(
    paste0(
      './RawExposure/Figures/climatologies/',
      names(raw[x]),
      '_2020_2030_climatology.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(mFut[[x]], main = month.abb, zlim = range(mFut[[x]][], na.rm = T))
  dev.off()
  print(x)
}

###plot differences
for (x in 1:length(mPres)) {
  pdf(
    paste0(
      './RawExposure/Figures/2009-19 vs 2020-30/differences/',
      names(raw[x]),
      '_difference.pdf'
    ),
    width = 11,
    height = 8
  )
  df <- mFut[[x]] - mPres[[x]]
  plot(df, main = month.abb, zlim = range(df[], na.rm = T))
  dev.off()
  print(x)
}

###plot present standard deviations
for (x in 1:length(sdPres)) {
  pdf(
    paste0(
      './RawExposure/Figures/2009-19 vs 2020-30/present_sds/',
      names(raw[x]),
      '_sds.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(sdPres[[x]], main = month.abb, zlim = range(sdPres[[x]][], na.rm = T))
  dev.off()
  print(x)
}

############
#2009-2019 v 2025-2035
#get 09-19 data
load(
  "/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/Data/MOM6/raw_MOM6_082025.RData"
) #load MOM6 raw data (object name = raw)
#data cleanup
pre <- fut <- vector(mode = 'list', length = length(raw))
for (x in 1:length(raw)) {
  pre[[x]] <- list(raster::subset(raw[[x]][[1]], 193:324))
}
load(
  "/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/Data/MOM6/raw_MOM6_decadalforecast_2025_2035_102025.RData"
) #load MOM6 raw data (object name = raw)
fut <- raw

exp0925 <- calcExposure(pre, fut)
names(exp0925) <- names(raw)
save(exp0925, file = './RawExposure/Data/2009_2019_v_2025_2035_exposure.RData')

#make and save nice plots of each variable
for (x in 1:length(exp0925)) {
  pdf(
    paste0(
      './RawExposure/Figures/2009-19 vs 2025-35/',
      names(exp0925[x]),
      '_exposure.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(exp0925[[x]], main = month.abb, zlim = range(exp0925[[x]][], na.rm = T))
  dev.off()
  print(x)
}

#make climatologies
mPres <- mFut <- sdPres <- vector(mode = 'list', length = length(pre))
for (v in 1:length(pre)) {
  climP <- climF <- sdP <- vector(mode = 'list', length = 12)
  for (x in 1:12) {
    #take mean of 'present' and 'future'
    climP[[x]] <- raster::calc(
      raster::subset(
        pre[[v]][[1]],
        seq(x, raster::nlayers(pre[[v]][[1]]), by = 12)
      ),
      mean
    )
    climF[[x]] <- raster::calc(
      raster::subset(
        fut[[v]][[1]],
        seq(x, raster::nlayers(fut[[v]][[1]]), by = 12)
      ),
      mean
    )

    ##calculate SD
    # sdP[[x]] <- raster::calc(raster::subset(pre[[v]][[1]], seq(x, raster::nlayers(pre[[v]][[1]]), by = 12)), sd)
  }
  mPres[[v]] <- raster::stack(climP)
  mFut[[v]] <- raster::stack(climF)
  #sdPres[[v]] <- raster::stack(sdP)
}
#don't need SD again since we've already done it for 09-19 - copied from other folder

#### plot climatologies (just fut since we already have 09-19)
#2025-2035
for (x in 1:length(mFut)) {
  pdf(
    paste0(
      './RawExposure/Figures/climatologies/',
      names(raw[x]),
      '_2025_2035_climatology.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(mFut[[x]], main = month.abb, zlim = range(mFut[[x]][], na.rm = T))
  dev.off()
  print(x)
}

###plot differences
for (x in 1:length(mPres)) {
  pdf(
    paste0(
      './RawExposure/Figures/2009-19 vs 2025-35/differences/',
      names(raw[x]),
      '_difference.pdf'
    ),
    width = 11,
    height = 8
  )
  df <- mFut[[x]] - mPres[[x]]
  plot(df, main = month.abb, zlim = range(df[], na.rm = T))
  dev.off()
  print(x)
}

##don't need to plot SD again since we did it already - figures copied to this folder for consistency

########################step 2 - rank exposure
#1993-08 v 2009-2019
load('./RawExposure/Data/1993_2008_v_2009_2019_exposure.RData') #load raw exposure (exp9309)
expRanked <- rankExposure(
  exp9309,
  flip = T,
  noflipList = c('bottomT', 'surfaceT', 'bottomArg', 'MLD')
)
save(
  expRanked,
  file = './RawExposure/Data/1993_2008_v_2009_2019_exposure_ranked.RData'
)

#make and save nice plots of each variable
for (x in 1:length(expRanked)) {
  pdf(
    paste0(
      './RawExposure/Figures/1993-2008 vs 2009-19/',
      names(expRanked[x]),
      '_exposure_ranked.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(expRanked[[x]], main = month.abb)
  dev.off()
  print(x)
}

#2009-2019 v 2020-30
load('./RawExposure/Data/2009_2019_v_2020_2030_exposure.RData') #load raw exposure (exp0920)
expRanked <- rankExposure(
  exp0920,
  flip = T,
  noflipList = c('bottomT', 'surfaceT', 'bottomArg', 'MLD')
)
save(
  expRanked,
  file = './RawExposure/Data/2009_2019_v_2020_2030_exposure_ranked.RData'
)

#make and save nice plots of each variable
for (x in 1:length(expRanked)) {
  pdf(
    paste0(
      './RawExposure/Figures/2009-19 vs 2020-30/',
      names(expRanked[x]),
      '_exposure_ranked.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(expRanked[[x]], main = month.abb)
  dev.off()
  print(x)
}

#2009-2019 v 2025-35
load('./RawExposure/Data/2009_2019_v_2025_2035_exposure.RData') #load raw exposure (exp0925)
expRanked <- rankExposure(
  exp0925,
  flip = T,
  noflipList = c('bottomT', 'surfaceT', 'bottomArg', 'MLD')
)
save(
  expRanked,
  file = './RawExposure/Data/2009_2019_v_2025_2035_exposure_ranked.RData'
)

#make and save nice plots of each variable
for (x in 1:length(expRanked)) {
  pdf(
    paste0(
      './RawExposure/Figures/2009-19 vs 2025-35/',
      names(expRanked[x]),
      '_exposure_ranked.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(expRanked[[x]], main = month.abb)
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
