###calculate exposure w/ functions

##################################
#####SET UP - LOAD EVERY TIME ####
##################################

setwd('/home/kgallagher/ClimateVulnerabilityAssessment2.0/Exposure')
### source functions
targets::tar_source(
  "/home/kgallagher/ClimateVulnerabilityAssessment2.0/functions"
) #this will eventually be replaced with loading the package

#load species list for loops
spp.list <- read.csv(
  '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/spp_list.csv'
)
#spp.list <- spp.list[,c(1:6)]
spp.list$Name <- gsub(' ', '', spp.list$Common.Name) #make clean names to make folders if necessary/match to folder names

#make directory for each species if it doesn't exist; if directory exists, it is not changed
for (x in 1:nrow(spp.list)) {
  dir.create(file.path(getwd(), spp.list$Name[x]), showWarnings = T) #main species folder
  dir.create(file.path(getwd(), spp.list$Name[x], 'Data'), showWarnings = T) #data folder
  #data subfolders for all combinations of present/future timeseries
  dir.create(
    file.path(getwd(), spp.list$Name[x], 'Data', '1993-2008 vs 2009-2019'),
    showWarnings = T
  ) #present: 1993-2008, future: 2009-2019
  dir.create(
    file.path(getwd(), spp.list$Name[x], 'Data', '2009-2019 vs 2020-2030'),
    showWarnings = T
  ) #present: 2009-2019, future: 2020-2030
  dir.create(
    file.path(getwd(), spp.list$Name[x], 'Data', '2009-2019 vs 2025-2035'),
    showWarnings = T
  ) #present: 2009-2019, future: 2025-2035
  dir.create(file.path(getwd(), spp.list$Name[x], 'Figures'), showWarnings = T) #figures folder
  #data subfolders for all combinations of present/future timeseries
  dir.create(
    file.path(getwd(), spp.list$Name[x], 'Figures', '1993-2008 vs 2009-2019'),
    showWarnings = T
  ) #present: 1993-2008, future: 2009-2019
  dir.create(
    file.path(getwd(), spp.list$Name[x], 'Figures', '2009-2019 vs 2020-2030'),
    showWarnings = T
  ) #present: 2009-2019, future: 2020-2030
  dir.create(
    file.path(getwd(), spp.list$Name[x], 'Figures', '2009-2019 vs 2025-2035'),
    showWarnings = T
  ) #present: 2009-2019, future: 2025-2035
}
##################################

##################################
### calculate & rank exposure
##################################
###############step 1 - calculate exposure

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
staticR <- terra::rast('../SDMs/Data/staticVariables_cropped_terra_reproj.tif')#staticR
#bathy object = staticR$bathy
bathy <- terra::wrap(staticR$bathy) #this is required because of the way terra holds rasters in memory and how things are distributed in parallel with future_map; the bathy raster gets unwrapped within the wrapper function


#only needs to be done once for each time period
#2014-2023 v 2025-2035

for (x in var.names) {
  hindcast_path <- paste0('../SDMs/Data/MOM6/raw_MOM6_', x, '_hindcast_r20250715_global.tif')
  hindcast <- terra::rast(hindcast_path)
  hindcast <- hindcast[[253:372]] #last ten years of hindcast (2014-2023)

  #get years from names to help with naming output
  yrs <- as.numeric(sub(".*\\.", "", names(hindcast)))

  forecast_path <- paste0('../SDMs/Data/MOM6/raw_MOM6_', x, '_forecast_r20250925_i202501_global.tif')
  forecast <- terra::rast(forecast_path)

  #raw exposure
  raw_exp <- calculate_raw_exposure(present = hindcast,
                                    future = forecast,
                                    spatial_temporal = F,
                                    mask_bathy = T,
                                    bathy = bathy,
                                    bathy_range = c(-1000, 0))
  terra::writeRaster(raw_exp, filename = paste0('./RawExposure/Data/',
                                                x,
                                                '_rawexposure_r20250925_i202501_r20250715_',
                                                min(yrs), max(yrs),'_global.tif'))

  #rank exposure
  ranked_exp <- rank_exposure(exposure = raw_exp,
                              flip = !(x %in% c('bottomT', 'surfaceT', 'bottomArg', 'MLD'))) #if x is one of these names, set flip to F; if not, flip will be T
  terra::writeRaster(raw_exp, filename = paste0('./RawExposure/Data/',
                                                x,
                                                '_rankedexposure_r20250925_i202501_r20250715_',
                                                min(yrs), max(yrs),'_global.tif'))

}

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
### create species-specific averages for all variables
##################################

#contemporary - 1993-2008 v 2009-2019
args <- tidyr::expand_grid(
  spp = spp.list$Name,
  ensName = 'ENSEMBLE_1993_2019',
  pStart = 1993,
  pEnd = 2008,
  fStart = 2009,
  fEnd = 2019
)
plan(multisession, workers = 6)
checks <- future_pmap(
  list(
    ..1 = args$spp,
    ..2 = args$ensName,
    ..3 = args$pStart,
    ..4 = args$pEnd,
    ..5 = args$fStart,
    ..6 = args$fEnd
  ),
  ~ makeVariableAverages(
    spp = ..1,
    ensName = ..2,
    pStart = ..3,
    pEnd = ..4,
    fStart = ..5,
    fEnd = ..6
  ),
  .progress = T,
  .options = furrr_options(seed = 2025)
)
plan(sequential)

#decade 1 - 2009-2019 v 2020 - 2030
args <- tidyr::expand_grid(
  spp = spp.list$Name,
  ensName = 'ENSEMBLE_1993_2019',
  pStart = 2009,
  pEnd = 2019,
  fStart = 2020,
  fEnd = 2030
)
plan(multisession, workers = 6)
checks <- future_pmap(
  list(
    ..1 = args$spp,
    ..2 = args$ensName,
    ..3 = args$pStart,
    ..4 = args$pEnd,
    ..5 = args$fStart,
    ..6 = args$fEnd
  ),
  ~ makeVariableAverages(
    spp = ..1,
    ensName = ..2,
    pStart = ..3,
    pEnd = ..4,
    fStart = ..5,
    fEnd = ..6
  ),
  .progress = T,
  .options = furrr_options(seed = 2025)
)
plan(sequential)

#decade 2 - 2009-2019 v 2025 - 2035
args <- tidyr::expand_grid(
  spp = spp.list$Name,
  ensName = 'ENSEMBLE_1993_2019',
  pStart = 2009,
  pEnd = 2019,
  fStart = 2025,
  fEnd = 2035
)
plan(multisession, workers = 6)
checks <- future_pmap(
  list(
    ..1 = args$spp,
    ..2 = args$ensName,
    ..3 = args$pStart,
    ..4 = args$pEnd,
    ..5 = args$fStart,
    ..6 = args$fEnd
  ),
  ~ makeVariableAverages(
    spp = ..1,
    ensName = ..2,
    pStart = ..3,
    pEnd = ..4,
    fStart = ..5,
    fEnd = ..6
  ),
  .progress = T,
  .options = furrr_options(seed = 2025)
)
plan(sequential)

##################################

##################################
### Make Total Exposure
##################################
#create weights & save
staticVars <- c('year', 'month', 'bathy', 'rugosity', 'dist2coast') #the variables to exclude from dynamic variable weights, since these will not chance
for (x in 1:nrow(spp.list)) {
  #load in necessary things
  #make list of importance files
  iFlist <- dir(
    path = paste0(
      '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
      spp.list$Name[x],
      '/model_output/importance'
    ),
    full.names = T
  )

  #load in weights
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/model_output/ensemble_weights.RData'
  )) #weights

  #load in data frame to get names of variables
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/pa_clean.RData'
  )) #dfC
  iv <- which(names(dfC) == 'x' | names(dfC) == 'y' | names(dfC) == 'value')
  vars <- names(dfC)[-iv] #exclude space and value (presence/absence)

  cW <- combineWeights(
    vars = vars,
    ensWeights = weights,
    impFlist = iFlist,
    staticNames = staticVars
  )
  save(
    cW,
    file = paste0(
      file.path(getwd(), spp.list$Name[x], 'Data'),
      '/combined_variable_weights.RData'
    )
  )
  print(x)
}

##TIME SERIES
#make total exposure with only important variables in models
#contemporary exposure (1993-2008 vs 2009-2019)
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/1993-2008 vs 2009-2019/variable_exposure_timeseries.RData'
  )) #vecExp

  #subset timeseries matrix by rownames
  i <- rownames(vecExp) %in% names(cW)
  vecSub <- vecExp[i, ]

  totalT <- combineTimeseries(
    matExp = vecSub,
    weights = cW,
    wThreshold = 0.1,
    countAll = F
  )
  save(
    totalT,
    file = paste0(
      file.path(getwd(), spp.list$Name[x], 'Data'),
      '/1993-2008 vs 2009-2019/total_exposure_timeseries_subset.RData'
    )
  )
  print(x)
}

#decade 1 exposure (2009-2019 vs 2020-2030)
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2020-2030/variable_exposure_timeseries.RData'
  )) #vecExp

  #subset timeseries matrix by rownames
  i <- rownames(vecExp) %in% names(cW)
  vecSub <- vecExp[i, ]

  totalT <- combineTimeseries(
    matExp = vecSub,
    weights = cW,
    wThreshold = 0.1,
    countAll = F
  )
  save(
    totalT,
    file = paste0(
      file.path(getwd(), spp.list$Name[x], 'Data'),
      '/2009-2019 vs 2020-2030/total_exposure_timeseries_subset.RData'
    )
  )
  print(x)
}

#decade 2 exposure (2009-2019 vs 2025-2035)
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2025-2035/variable_exposure_timeseries.RData'
  )) #vecExp

  #subset timeseries matrix by rownames
  i <- rownames(vecExp) %in% names(cW)
  vecSub <- vecExp[i, ]

  totalT <- combineTimeseries(
    matExp = vecSub,
    weights = cW,
    wThreshold = 0.1,
    countAll = F
  )
  save(
    totalT,
    file = paste0(
      file.path(getwd(), spp.list$Name[x], 'Data'),
      '/2009-2019 vs 2025-2035/total_exposure_timeseries_subset.RData'
    )
  )
  print(x)
}

#make total exposure with ALL variables
#contemporary exposure
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/1993-2008 vs 2009-2019/variable_exposure_timeseries.RData'
  )) #vecExp

  #subset timeseries matrix by rownames
  i <- rownames(vecExp) %in% names(cW)
  vecSub <- vecExp[i, ]

  totalT <- combineTimeseries(
    matExp = vecSub,
    weights = cW,
    wThreshold = 0,
    countAll = T
  )
  save(
    totalT,
    file = paste0(
      file.path(getwd(), spp.list$Name[x], 'Data'),
      '/1993-2008 vs 2009-2019/total_exposure_timeseries_all.RData'
    )
  )
  print(x)
}

#decade 1 exposure
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2020-2030/variable_exposure_timeseries.RData'
  )) #vecExp

  #subset timeseries matrix by rownames
  i <- rownames(vecExp) %in% names(cW)
  vecSub <- vecExp[i, ]

  totalT <- combineTimeseries(
    matExp = vecSub,
    weights = cW,
    wThreshold = 0,
    countAll = T
  )
  save(
    totalT,
    file = paste0(
      file.path(getwd(), spp.list$Name[x], 'Data'),
      '/2009-2019 vs 2020-2030/total_exposure_timeseries_all.RData'
    )
  )
  print(x)
}

#decade 2 exposure
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2025-2035/variable_exposure_timeseries.RData'
  )) #vecExp

  #subset timeseries matrix by rownames
  i <- rownames(vecExp) %in% names(cW)
  vecSub <- vecExp[i, ]

  totalT <- combineTimeseries(
    matExp = vecSub,
    weights = cW,
    wThreshold = 0,
    countAll = T
  )
  save(
    totalT,
    file = paste0(
      file.path(getwd(), spp.list$Name[x], 'Data'),
      '/2009-2019 vs 2025-2035/total_exposure_timeseries_all.RData'
    )
  )
  print(x)
}

#MAPS
#with important variables
#contemporary exposure
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/1993-2008 vs 2009-2019/variable_exposure_maps.RData'
  )) #mapExp

  #subset mapExp by rownames
  i <- names(mapExp) %in% names(cW)
  mapSub <- raster::subset(mapExp, which(i == T))

  totalM <- combineMaps(
    mapExp = mapSub,
    weights = cW,
    wThreshold = 0.1,
    countAll = F
  )
  save(
    totalM,
    file = paste0(
      file.path(getwd(), spp.list$Name[x], 'Data'),
      '/1993-2008 vs 2009-2019/total_exposure_maps_subset.RData'
    )
  )
  print(x)
}

#decade 1 exposure
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2020-2030/variable_exposure_maps.RData'
  )) #vecExp

  #subset mapExp by rownames
  i <- names(mapExp) %in% names(cW)
  mapSub <- raster::subset(mapExp, which(i == T))

  totalM <- combineMaps(
    mapExp = mapSub,
    weights = cW,
    wThreshold = 0.1,
    countAll = F
  )
  save(
    totalM,
    file = paste0(
      file.path(getwd(), spp.list$Name[x], 'Data'),
      '/2009-2019 vs 2020-2030/total_exposure_maps_subset.RData'
    )
  )
  print(x)
}

#decade 2 exposure
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2025-2035/variable_exposure_maps.RData'
  )) #vecExp

  #subset mapExp by rownames
  i <- names(mapExp) %in% names(cW)
  mapSub <- raster::subset(mapExp, which(i == T))

  totalM <- combineMaps(
    mapExp = mapSub,
    weights = cW,
    wThreshold = 0.1,
    countAll = F
  )
  save(
    totalM,
    file = paste0(
      file.path(getwd(), spp.list$Name[x], 'Data'),
      '/2009-2019 vs 2025-2035/total_exposure_maps_subset.RData'
    )
  )
  print(x)
}

#with ALL variables
#contemporary exposure
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/1993-2008 vs 2009-2019/variable_exposure_maps.RData'
  )) #mapExp

  #subset mapExp by rownames
  i <- names(mapExp) %in% names(cW)
  mapSub <- raster::subset(mapExp, which(i == T))

  totalM <- combineMaps(
    mapExp = mapSub,
    weights = cW,
    wThreshold = 0,
    countAll = T
  )
  save(
    totalM,
    file = paste0(
      file.path(getwd(), spp.list$Name[x], 'Data'),
      '/1993-2008 vs 2009-2019/total_exposure_maps_all.RData'
    )
  )
  print(x)
}

#decade 1 exposure
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2020-2030/variable_exposure_maps.RData'
  )) #vecExp

  #subset mapExp by rownames
  i <- names(mapExp) %in% names(cW)
  mapSub <- raster::subset(mapExp, which(i == T))

  totalM <- combineMaps(
    mapExp = mapSub,
    weights = cW,
    wThreshold = 0,
    countAll = T
  )
  save(
    totalM,
    file = paste0(
      file.path(getwd(), spp.list$Name[x], 'Data'),
      '/2009-2019 vs 2020-2030/total_exposure_maps_all.RData'
    )
  )
  print(x)
}

#decade 2 exposure
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2025-2035/variable_exposure_maps.RData'
  )) #vecExp

  #subset mapExp by rownames
  i <- names(mapExp) %in% names(cW)
  mapSub <- raster::subset(mapExp, which(i == T))

  totalM <- combineMaps(
    mapExp = mapSub,
    weights = cW,
    wThreshold = 0,
    countAll = T
  )
  save(
    totalM,
    file = paste0(
      file.path(getwd(), spp.list$Name[x], 'Data'),
      '/2009-2019 vs 2025-2035/total_exposure_maps_all.RData'
    )
  )
  print(x)
}

##################################

##################################
### Plot Results - OLD FOR LOOPS
##################################
##timeseries
#contemporary
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/1993-2008 vs 2009-2019/variable_exposure_timeseries.RData'
  )) #vecExp

  #subset timeseries matrix by rownames
  i <- rownames(vecExp) %in% names(cW)
  vecSub <- vecExp[i, ]

  #plot variables
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/1993-2008 vs 2009-2019/exposure_variable_timeseries.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(
    vecSub[1, ],
    t = 'b',
    lty = 1,
    pch = 1,
    ylim = c(1, 4),
    ylab = "Exposure",
    xlab = "Month"
  )
  for (y in 2:nrow(vecSub)) {
    lines(vecSub[y, ], t = 'b', lty = y, pch = y)
  }
  legend(
    'top',
    legend = c(rownames(vecSub)),
    lty = c(1:nrow(vecSub)),
    pch = c(1:nrow(vecSub)),
    bty = 'n',
    ncol = 2
  )
  dev.off()

  #load total
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/1993-2008 vs 2009-2019/total_exposure_timeseries_all.RData'
  )) #totalT
  totalTA <- totalT
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/1993-2008 vs 2009-2019/total_exposure_timeseries_subset.RData'
  )) #totalT
  totalTS <- totalT

  #plot - total
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/1993-2008 vs 2009-2019/exposure_timeseries_all.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(
    totalTA,
    t = 'b',
    lty = 8,
    lwd = 3,
    pch = 19,
    ylim = c(1, 4),
    ylab = "Exposure",
    xlab = "Month"
  )
  for (y in 1:nrow(vecSub)) {
    lines(vecSub[y, ], t = 'b', lty = y, pch = y)
  }
  legend(
    'top',
    legend = c('Total - All Variables', rownames(vecSub)),
    lty = c(8, 1:nrow(vecSub)),
    pch = c(19, 1:nrow(vecSub)),
    bty = 'n',
    ncol = 2
  )
  dev.off()

  #plot - subset
  wi <- which(cW > 0.1)
  matSub <- vecSub[wi, ]

  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/1993-2008 vs 2009-2019/exposure_timeseries_subset.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(
    totalTS,
    t = 'b',
    lty = 9,
    lwd = 3,
    pch = 20,
    ylim = c(1, 4),
    ylab = "Exposure",
    xlab = "Month"
  )
  for (y in 1:nrow(matSub)) {
    lines(matSub[y, ], t = 'b', lty = y, pch = y)
  }
  legend(
    'top',
    legend = c('Total - Important Variables', rownames(matSub)),
    lty = c(9, 1:nrow(matSub)),
    pch = c(20, 1:nrow(matSub)),
    bty = 'n',
    ncol = 2
  )
  dev.off()

  print(x)
}

#decade 1
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2020-2030/variable_exposure_timeseries.RData'
  )) #vecExp

  #subset timeseries matrix by rownames
  i <- rownames(vecExp) %in% names(cW)
  vecSub <- vecExp[i, ]

  #plot variables
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/2009-2019 vs 2020-2030/exposure_variable_timeseries.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(
    vecSub[1, ],
    t = 'b',
    lty = 1,
    pch = 1,
    ylim = c(1, 4),
    ylab = "Exposure",
    xlab = "Month"
  )
  for (y in 2:nrow(vecSub)) {
    lines(vecSub[y, ], t = 'b', lty = y, pch = y)
  }
  legend(
    'top',
    legend = c(rownames(vecSub)),
    lty = c(1:nrow(vecSub)),
    pch = c(1:nrow(vecSub)),
    bty = 'n',
    ncol = 2
  )
  dev.off()

  #load total
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2020-2030/total_exposure_timeseries_all.RData'
  )) #totalT
  totalTA <- totalT
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2020-2030/total_exposure_timeseries_subset.RData'
  )) #totalT
  totalTS <- totalT

  #plot - total
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/2009-2019 vs 2020-2030/exposure_timeseries_all.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(
    totalTA,
    t = 'b',
    lty = 8,
    lwd = 3,
    pch = 19,
    ylim = c(1, 4),
    ylab = "Exposure",
    xlab = "Month"
  )
  for (y in 1:nrow(vecSub)) {
    lines(vecSub[y, ], t = 'b', lty = y, pch = y)
  }
  legend(
    'top',
    legend = c('Total - All Variables', rownames(vecSub)),
    lty = c(8, 1:nrow(vecSub)),
    pch = c(19, 1:nrow(vecSub)),
    bty = 'n',
    ncol = 2
  )
  dev.off()

  #plot - subset
  wi <- which(cW > 0.1)
  matSub <- vecSub[wi, ]

  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/2009-2019 vs 2020-2030/exposure_timeseries_subset.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(
    totalTS,
    t = 'b',
    lty = 9,
    lwd = 3,
    pch = 20,
    ylim = c(1, 4),
    ylab = "Exposure",
    xlab = "Month"
  )
  for (y in 1:nrow(matSub)) {
    lines(matSub[y, ], t = 'b', lty = y, pch = y)
  }
  legend(
    'top',
    legend = c('Total - Important Variables', rownames(matSub)),
    lty = c(9, 1:nrow(matSub)),
    pch = c(20, 1:nrow(matSub)),
    bty = 'n',
    ncol = 2
  )
  dev.off()

  print(x)
}

#decade 2
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2025-2035/variable_exposure_timeseries.RData'
  )) #vecExp

  #subset timeseries matrix by rownames
  i <- rownames(vecExp) %in% names(cW)
  vecSub <- vecExp[i, ]

  #plot variables
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/2009-2019 vs 2025-2035/exposure_variable_timeseries.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(
    vecSub[1, ],
    t = 'b',
    lty = 1,
    pch = 1,
    ylim = c(1, 4),
    ylab = "Exposure",
    xlab = "Month"
  )
  for (y in 2:nrow(vecSub)) {
    lines(vecSub[y, ], t = 'b', lty = y, pch = y)
  }
  legend(
    'top',
    legend = c(rownames(vecSub)),
    lty = c(1:nrow(vecSub)),
    pch = c(1:nrow(vecSub)),
    bty = 'n',
    ncol = 2
  )
  dev.off()

  #load total
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2025-2035/total_exposure_timeseries_all.RData'
  )) #totalT
  totalTA <- totalT
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2025-2035/total_exposure_timeseries_subset.RData'
  )) #totalT
  totalTS <- totalT

  #plot - total
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/2009-2019 vs 2025-2035/exposure_timeseries_all.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(
    totalTA,
    t = 'b',
    lty = 8,
    lwd = 3,
    pch = 19,
    ylim = c(1, 4),
    ylab = "Exposure",
    xlab = "Month"
  )
  for (y in 1:nrow(vecSub)) {
    lines(vecSub[y, ], t = 'b', lty = y, pch = y)
  }
  legend(
    'top',
    legend = c('Total - All Variables', rownames(vecSub)),
    lty = c(8, 1:nrow(vecSub)),
    pch = c(19, 1:nrow(vecSub)),
    bty = 'n',
    ncol = 2
  )
  dev.off()

  #plot - subset
  wi <- which(cW > 0.1)
  matSub <- vecSub[wi, ]

  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/2009-2019 vs 2025-2035/exposure_timeseries_subset.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(
    totalTS,
    t = 'b',
    lty = 9,
    lwd = 3,
    pch = 20,
    ylim = c(1, 4),
    ylab = "Exposure",
    xlab = "Month"
  )
  for (y in 1:nrow(matSub)) {
    lines(matSub[y, ], t = 'b', lty = y, pch = y)
  }
  legend(
    'top',
    legend = c('Total - Important Variables', rownames(matSub)),
    lty = c(9, 1:nrow(matSub)),
    pch = c(20, 1:nrow(matSub)),
    bty = 'n',
    ncol = 2
  )
  dev.off()

  print(x)
}

#maps
load('./RawExposure/Data/coastline.RData')
#contemporary
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load maps
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/1993-2008 vs 2009-2019/variable_exposure_maps.RData'
  )) #mapExp

  #subset timeseries matrix by rownames
  i <- names(mapExp) %in% names(cW)
  mapSub <- raster::subset(mapExp, which(i == T))

  #plot
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/1993-2008 vs 2009-2019/exposure_variable_maps.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(mapSub, zlim = c(1, 4), col = cmocean('matter')(4))
  #plot(coastCropped['id'], col = 'grey', add = T)
  dev.off()

  #load total
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/1993-2008 vs 2009-2019/total_exposure_maps_all.RData'
  )) #totalM

  #plot
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/1993-2008 vs 2009-2019/exposure_total_maps_all.pdf'
    ),
    width = 8,
    height = 11
  )
  plot(totalM, zlim = c(1, 4), col = cmocean('matter')(4))
  plot(coastCropped['id'], col = 'grey', add = T)
  dev.off()

  #load total subset
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/1993-2008 vs 2009-2019/total_exposure_maps_subset.RData'
  )) #totalM

  #plot
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/1993-2008 vs 2009-2019/exposure_total_maps_subset.pdf'
    ),
    width = 8,
    height = 11
  )
  plot(totalM, zlim = c(1, 4), col = cmocean('matter')(4))
  plot(coastCropped['id'], col = 'grey', add = T)
  dev.off()

  print(x)
}

#decade 1
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load maps
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2020-2030/variable_exposure_maps.RData'
  )) #mapExp

  #subset timeseries matrix by rownames
  i <- names(mapExp) %in% names(cW)
  mapSub <- raster::subset(mapExp, which(i == T))

  #plot
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/2009-2019 vs 2020-2030/exposure_variable_maps.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(mapSub, zlim = c(1, 4), col = cmocean('matter')(4))
  #plot(coastCropped['id'], col = 'grey', add = T)
  dev.off()

  #load total
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2020-2030/total_exposure_maps_all.RData'
  )) #totalM

  #plot
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/2009-2019 vs 2020-2030/exposure_total_maps_all.pdf'
    ),
    width = 8,
    height = 11
  )
  plot(totalM, zlim = c(1, 4), col = cmocean('matter')(4))
  plot(coastCropped['id'], col = 'grey', add = T)
  dev.off()

  #load total subset
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2020-2030/total_exposure_maps_subset.RData'
  )) #totalM

  #plot
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/2009-2019 vs 2020-2030/exposure_total_maps_subset.pdf'
    ),
    width = 8,
    height = 11
  )
  plot(totalM, zlim = c(1, 4), col = cmocean('matter')(4))
  plot(coastCropped['id'], col = 'grey', add = T)
  dev.off()

  print(x)
}

#decade 2
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load maps
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2025-2035/variable_exposure_maps.RData'
  )) #mapExp

  #subset timeseries matrix by rownames
  i <- names(mapExp) %in% names(cW)
  mapSub <- raster::subset(mapExp, which(i == T))

  #plot
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/2009-2019 vs 2025-2035/exposure_variable_maps.pdf'
    ),
    width = 11,
    height = 8
  )
  plot(mapSub, zlim = c(1, 4), col = cmocean('matter')(4))
  #plot(coastCropped['id'], col = 'grey', add = T)
  dev.off()

  #load total
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2025-2035/total_exposure_maps_all.RData'
  )) #totalM

  #plot
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/2009-2019 vs 2025-2035/exposure_total_maps_all.pdf'
    ),
    width = 8,
    height = 11
  )
  plot(totalM, zlim = c(1, 4), col = cmocean('matter')(4))
  plot(coastCropped['id'], col = 'grey', add = T)
  dev.off()

  #load total subset
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2025-2035/total_exposure_maps_subset.RData'
  )) #totalM

  #plot
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/2009-2019 vs 2025-2035/exposure_total_maps_subset.pdf'
    ),
    width = 8,
    height = 11
  )
  plot(totalM, zlim = c(1, 4), col = cmocean('matter')(4))
  plot(coastCropped['id'], col = 'grey', add = T)
  dev.off()

  print(x)
}

#dynamic radar plots
library(fmsb)
for (x in 1:nrow(spp.list)) {
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  cW <- rbind(rep(1, length(cW)), rep(0, length(cW)), cW)

  #plot
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/dynamic_variable_weights.pdf'
    ),
    width = 8,
    height = 8
  )
  radarchart(as.data.frame(cW), pfcol = alpha('grey', 0.5))
  dev.off()

  print(x)
}

##inset timeseries on maps
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
    'Mixed layer depth (delta rho = 0.03)',
    'Diazotroph\nintegrated prim. prod.',
    'Small phyto.\nintegrated prim. prod.',
    'Medium phyto.\nintegrated prim. prod.',
    'Large phyto.\nintegrated prim. prod.',
    'Small zooplankton\nintegrated biomass',
    'Medium zooplankton\nintegrated biomass',
    'Large zooplankton\nintegrated biomass',
    'Net primary production',
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
#decade 2
for (x in 1:nrow(spp.list)) {
  ###VARIABLE-LEVEL EXPOSURE
  #load variable weights
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/combined_variable_weights.RData'
  )) #cW

  #load maps
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2025-2035/variable_exposure_maps.RData'
  )) #mapExp
  #subset timeseries matrix by rownames
  i <- names(mapExp) %in% names(cW)
  mapSub <- raster::subset(mapExp, which(i == T))

  #load timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2025-2035/variable_exposure_timeseries.RData'
  )) #vecExp
  #subset timeseries matrix by rownames
  i <- rownames(vecExp) %in% names(cW)
  vecSub <- vecExp[i, ]

  #plot
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/2009-2019 vs 2025-2035/variable_exposure_maps_inset_timeseries.pdf'
    ),
    width = 8,
    height = 11
  )
  #set up panels according to the number of variables
  if (raster::nlayers(mapSub) < 6) {
    par(mfrow = c(2, 3))
  } else {
    par(mfrow = c(3, 3))
  }

  for (y in 1:raster::nlayers(mapSub)) {
    #get full name of variable
    i <- varDF$Short.Name %in% names(mapSub)[y]

    #map
    par(plt = c(0.2, 0.9, 0.15, 0.875))
    plot(
      raster::subset(mapSub, y),
      zlim = c(1, 4),
      col = cmocean('matter')(4),
      legend = F,
      legend.mar = 0,
      xlab = expression('Longitude (' * degree * ')'),
      ylab = expression('Latitude (' * degree * ')'),
      xaxt = 'n',
      yaxt = 'n',
      main = varDF$Long.Name[i]
    )
    axis(2, at = seq(30, 50, by = 1), labels = seq(30, 50, by = 1), las = 2)
    axis(1, at = seq(-85, -65, by = 1), labels = seq(-85, -65, by = 1))
    plot(coastCropped['id'], col = 'grey', add = T)

    #inset timeseries
    par(plt = c(0.55, 0.9, 0.25, 0.45), new = TRUE)
    plot(
      vecSub[y, ],
      t = 'b',
      lty = 8,
      lwd = 0.8,
      cex = 0.8,
      pch = y,
      ylim = c(1, 4),
      ylab = "",
      xlab = "",
      yaxt = 'n',
      xaxt = 'n'
    )
    axis(1, at = 1:12, labels = month.abb, las = 2)
    axis(2, at = 1:4, labels = c('L', "M", "H", "VH"), las = 2, cex.lab = 0.75)
  }

  if (raster::nlayers(mapSub) != 6) {
    #add legend on the last one if the number of variables is not 6
    plot(1:10, t = 'n', axes = F, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    fields::image.plot(
      matrix(seq(1, 4, length.out = 16), 4, 4),
      legend.only = T,
      horizontal = F,
      legend.shrink = 0.7,
      smallplot = c(0.4, 0.6, 0.2, 0.8),
      legend.args = list(text = 'Exposure', cex = 1.25, side = 3, line = 0.1),
      axis.args = list(
        cex.axis = 1,
        at = 1:4,
        labels = c('Low (L)', "Moderate (M)", "High (H)", "Very High (VH)"),
        mgp = c(3, 0.5, 0)
      ),
      col = cmocean::cmocean('matter')(4)
    )
  } else {
    #if the number of variables is 6, it will still be a 3x3 grid, so put legend in the middle by adding an extra plot
    plot(1:10, t = 'n', axes = F, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    plot(1:10, t = 'n', axes = F, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    fields::image.plot(
      matrix(seq(1, 4, length.out = 16), 4, 4),
      legend.only = T,
      horizontal = F,
      legend.shrink = 0.7,
      smallplot = c(0.4, 0.6, 0.2, 0.8),
      legend.args = list(text = 'Exposure', cex = 1.25, side = 3, line = 0.1),
      axis.args = list(
        cex.axis = 1,
        at = 1:4,
        labels = c('Low (L)', "Moderate (M)", "High (H)", "Very High (VH)"),
        mgp = c(3, 0.5, 0)
      ),
      col = cmocean::cmocean('matter')(4)
    )
  }

  dev.off()

  ##TOTAL EXPOSURE - ALL VARIABLES
  #load total map
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2025-2035/total_exposure_maps_all.RData'
  )) #totalM

  #load total timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2025-2035/total_exposure_timeseries_all.RData'
  )) #totalT

  #plot
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/2009-2019 vs 2025-2035/total_exposure_maps_inset_timeseries_allvars.pdf'
    ),
    width = 8,
    height = 11
  )
  #map
  par(fig = c(0, 1, 0, 1))
  plot(
    totalM,
    zlim = c(1, 4),
    col = cmocean('matter')(4),
    ylim = c(35, 45),
    legend = F,
    xlab = expression('Longitude (' * degree * ')'),
    ylab = expression('Latitude (' * degree * ')'),
    xaxt = 'n',
    yaxt = 'n',
    legend.mar = 0
  )
  axis(2, at = seq(30, 50, by = 1), labels = seq(30, 50, by = 1), las = 2)
  axis(1, at = seq(-85, -65, by = 1), labels = seq(-85, -65, by = 1))
  plot(coastCropped['id'], col = 'grey', add = T)
  fields::image.plot(
    matrix(seq(1, 4, length.out = 16), 4, 4),
    legend.only = T,
    horizontal = T,
    legend.shrink = 0.7,
    smallplot = c(0.5, 0.9, 0.15, 0.2),
    legend.args = list(text = 'Exposure', cex = 1.5, side = 3, line = 0.1),
    axis.args = list(
      cex.axis = 1,
      at = 1:4,
      labels = c('Low', "Moderate", "High", "Very High"),
      mgp = c(3, 0.5, 0)
    ),
    col = cmocean::cmocean('matter')(4)
  )

  par(fig = c(0.125, 0.6, 0.65, 0.95), new = TRUE)
  plot(
    totalT,
    t = 'b',
    lty = 8,
    lwd = 1.5,
    pch = 19,
    ylim = c(1, 4),
    ylab = "",
    xlab = "Month",
    yaxt = 'n',
    xaxt = 'n'
  )
  axis(1, at = 1:12, labels = month.abb, las = 2)
  axis(
    2,
    at = 1:4,
    labels = c('Low', "Moderate", "High", "Very High"),
    las = 2,
    cex.lab = 0.75
  )
  dev.off()

  ### ONLY IMPORTANT VARS
  #load total map
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2025-2035/total_exposure_maps_subset.RData'
  )) #totalM

  #load total timeseries
  load(paste0(
    file.path(getwd(), spp.list$Name[x], 'Data'),
    '/2009-2019 vs 2025-2035/total_exposure_timeseries_subset.RData'
  )) #totalT

  #plot
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x], 'Figures'),
      '/2009-2019 vs 2025-2035/total_exposure_maps_inset_timeseries_impvars.pdf'
    ),
    width = 8,
    height = 11
  )
  #map
  par(fig = c(0, 1, 0, 1))
  plot(
    totalM,
    zlim = c(1, 4),
    col = cmocean('matter')(4),
    ylim = c(35, 45),
    legend = F,
    xlab = expression('Longitude (' * degree * ')'),
    ylab = expression('Latitude (' * degree * ')'),
    xaxt = 'n',
    yaxt = 'n',
    legend.mar = 0
  )
  axis(2, at = seq(30, 50, by = 1), labels = seq(30, 50, by = 1), las = 2)
  axis(1, at = seq(-85, -65, by = 1), labels = seq(-85, -65, by = 1))
  plot(coastCropped['id'], col = 'grey', add = T)
  fields::image.plot(
    matrix(seq(1, 4, length.out = 16), 4, 4),
    legend.only = T,
    horizontal = T,
    legend.shrink = 0.7,
    smallplot = c(0.5, 0.9, 0.15, 0.2),
    legend.args = list(text = 'Exposure', cex = 1.5, side = 3, line = 0.1),
    axis.args = list(
      cex.axis = 1,
      at = 1:4,
      labels = c('Low', "Moderate", "High", "Very High"),
      mgp = c(3, 0.5, 0)
    ),
    col = cmocean::cmocean('matter')(4)
  )

  par(fig = c(0.125, 0.6, 0.65, 0.95), new = TRUE)
  plot(
    totalT,
    t = 'b',
    lty = 8,
    lwd = 1.5,
    pch = 19,
    ylim = c(1, 4),
    ylab = "",
    xlab = "Month",
    yaxt = 'n',
    xaxt = 'n'
  )
  axis(1, at = 1:12, labels = month.abb, las = 2)
  axis(
    2,
    at = 1:4,
    labels = c('Low', "Moderate", "High", "Very High"),
    las = 2,
    cex.lab = 0.75
  )
  dev.off()

  print(x)
}

##################################

##################################
### Plot Results - WITH FUNCTIONS/NESTED FIGURES
##################################

load('./RawExposure/Data/coastline.RData') #coastCropped

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

#contemporary timeframe
plot_Exposure(
  species = spp.list$Name,
  type = c('variable', 'total', 'important', 'radar'),
  presentTime = '1993-2008',
  futureTime = '2009-2019',
  variableDF = varDF,
  coastline = coastCropped
)

#decade 1
plot_Exposure(
  species = spp.list$Name,
  type = c('variable', 'total', 'important'),
  presentTime = '2009-2019',
  futureTime = '2020-2030',
  variableDF = varDF,
  coastline = coastCropped
)

#decade 2
plot_Exposure(
  species = spp.list$Name,
  type = c('variable', 'total', 'important'),
  presentTime = '2009-2019',
  futureTime = '2025-2035',
  variableDF = varDF,
  coastline = coastCropped
)

## make exposure summary tables
makereport_exposuretable(
  species = spp.list$Name,
  presentTime = '2009-2019',
  futureTime = '2025-2035',
  variableDF = varDF
)

##################################
