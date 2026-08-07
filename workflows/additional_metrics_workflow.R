###additional metrics workflow
#includes directionality & changes in distributions
#data quality is included in sensitivity_workflow
#model confidence is included in sdm_workflow

library(spatialcva)

##############################
####### DIRECTIONALITY #######
##############################

setwd(
  "/home/kgallagher/ClimateVulnerabilityAssessment2.0/AdditionalMetrics/Directionality"
)

#create combined data.frame
flist <- dir('./raw_csvs', pattern = '.csv')
scorers <- sub(".*/NECVA2.0_Directional_Scores_(.*)\\.csv$", "\\1", flist)
direct <- NULL
#this presumes a similar set up and naming scheme to the model confidence spreadsheets
for(x in 1:length(flist)){
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
species.direct <- lapply(species.data.list, directionality, bootstrap = F) #calculate sensitivity w/o bootstrap
direct.bootstrap <- lapply(species.data.list, directionality, bootstrap = T) #this only takes ~5 minutes for 42 species

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
  "/home/kgallagher/ClimateVulnerabilityAssessment2.0/AdditionalMetrics/DistributionChange"
)


spp.list <- read.csv(
  '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/spp_list.csv'
)
spp.list$Name <- gsub(' ', '', spp.list$Common.Name)

#create species specific folders
for (x in 1:nrow(spp.list)) {
  dir.create(file.path(getwd(), spp.list$Name[x]), showWarnings = T) #main folder
}

##calculate for each time frame of interest
#1993-2019
for (x in 1:nrow(spp.list)) {
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/output_rasters/ENSEMBLE_1993_2019.RData'
  )) #abund
  abund <- raster::stack(abund)

  distMetrics <- change_in_distribution(
    abund = abund,
    area.threshold = 0.75,
    cell.area = 8 * 8
  )
  save(
    distMetrics,
    file = paste0(
      file.path(getwd(), spp.list$Name[x]),
      '/distribution_metrics_1993_2019.RData'
    )
  )
  print(spp.list$Name[x])
}

#2025-2035
for (x in 1:nrow(spp.list)) {
  load(paste0(
    '/home/kgallagher/ClimateVulnerabilityAssessment2.0/SDMs/',
    spp.list$Name[x],
    '/output_rasters/ENSEMBLE_2025_2034.RData'
  )) #abund
  abund <- raster::stack(abund)

  distMetrics <- change_in_distribution(
    abund = abund,
    area.threshold = 0.75,
    cell.area = 8 * 8
  )
  save(
    distMetrics,
    file = paste0(
      file.path(getwd(), spp.list$Name[x]),
      '/distribution_metrics_2025_2034.RData'
    )
  )
  print(spp.list$Name[x])
}

## plot on single time series & save
#should this get a plotting function?
for (x in 1:nrow(spp.list)) {
  #1993-2019
  load(paste0(
    file.path(getwd(), spp.list$Name[x]),
    '/distribution_metrics_1993_2019.RData'
  )) #distMetrics
  distMetrics$month <- sapply(str_split(distMetrics$timestamp, '[.]'), "[[", 1)
  distMetrics$year <- as.numeric(sapply(
    str_split(distMetrics$timestamp, '[.]'),
    "[[",
    2
  ))
  dist93 <- distMetrics

  #2025-2034
  load(paste0(
    file.path(getwd(), spp.list$Name[x]),
    '/distribution_metrics_2025_2034.RData'
  )) #distMetrics
  distMetrics$month <- sapply(str_split(distMetrics$timestamp, '[.]'), "[[", 1)
  distMetrics$year <- as.numeric(sapply(
    str_split(distMetrics$timestamp, '[.]'),
    "[[",
    2
  ))
  dist25 <- distMetrics

  #combine
  distMetrics <- rbind(dist93, dist25)

  #aggregate annually
  annualMets <- aggregate(distMetrics, by = list(distMetrics$year), FUN = mean)

  #plot center of gravity
  pdf(
    paste0(file.path(getwd(), spp.list$Name[x]), '/center_of_gravity.pdf'),
    width = 8,
    height = 11
  )
  layout(
    matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 2:13), nrow = 5, ncol = 4, byrow = T),
    height = rep(1, 4),
    width = rep(1, 4)
  )
  par(mar = c(2, 2, 1, 1))
  #annual time series
  plot(
    cog ~ year,
    data = annualMets[annualMets$year < 2020, ],
    t = 'b',
    pch = 19,
    ylim = range(distMetrics$cog),
    xlim = c(1993, 2035)
  ) #plot 1993-2019 in black
  lines(
    cog ~ year,
    data = annualMets[annualMets$year > 2020, ],
    t = 'b',
    pch = 19,
    col = 'red4'
  )

  #plot monthly
  for (m in month.abb) {
    plot(
      cog ~ year,
      data = distMetrics[distMetrics$month == m & distMetrics$year < 2020, ],
      t = 'b',
      pch = 1,
      main = m,
      ylim = range(distMetrics$cog),
      xlim = c(1993, 2035)
    )
    lines(
      cog ~ year,
      data = distMetrics[distMetrics$month == m & distMetrics$year > 2020, ],
      t = 'b',
      pch = 1,
      col = 'red4'
    )
  }

  dev.off()

  #plot center of gravity
  pdf(
    paste0(
      file.path(getwd(), spp.list$Name[x]),
      '/area_of_high_probability.pdf'
    ),
    width = 8,
    height = 11
  )
  layout(
    matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 2:13), nrow = 5, ncol = 4, byrow = T),
    height = rep(1, 4),
    width = rep(1, 4)
  )
  par(mar = c(2, 2, 1, 1))
  #annual time series
  plot(
    area ~ year,
    data = annualMets[annualMets$year < 2020, ],
    t = 'b',
    pch = 19,
    ylim = range(distMetrics$area),
    xlim = c(1993, 2035)
  ) #plot 1993-2019 in black
  lines(
    area ~ year,
    data = annualMets[annualMets$year > 2020, ],
    t = 'b',
    pch = 19,
    col = 'red4'
  )

  #plot monthly
  for (m in month.abb) {
    plot(
      area ~ year,
      data = distMetrics[distMetrics$month == m & distMetrics$year < 2020, ],
      t = 'b',
      pch = 1,
      main = m,
      ylim = range(distMetrics$area),
      xlim = c(1993, 2035)
    )
    lines(
      area ~ year,
      data = distMetrics[distMetrics$month == m & distMetrics$year > 2020, ],
      t = 'b',
      pch = 1,
      col = 'red4'
    )
  }

  dev.off()

  print(spp.list$Name[x])
}

##############################
