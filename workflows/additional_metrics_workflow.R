###additional metrics workflow
#includes directionality & changes in distributions
#data quality is included in sensitivity_workflow
#model confidence is included in sdm_workflow

##############################
####### DIRECTIONALITY #######
##############################
### STILL A DRAFT ##
## THIS CODE HASN'T BEEN TESTED BUT SHOULD WORK

setwd(
  "/home/kgallagher/ClimateVulnerabilityAssessment2.0/AdditionalMetrics/Directionality"
)
#knowing that directionality isn't input into the portal, it is more likely going to be set up like model confidence, so we will need to combine csvs

#create combined data.frame
flist <- dir('./raw_csvs', pattern = '.csv')
direct <- NULL
#this presumes a similar set up and naming scheme to the model confidence spreadsheets
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
  direct <- direct(modConf, f)
}
write.csv(direct, file = 'combined_directionality.csv') #save for prosperity (the above should be quick but just in case)

#now we calculate metric similar to sensitivity
species.data.list <- split(direct, direct$Species)
species.direct <- lapply(species.data.list, directionality, bootstrap = F) #calculate sensitivity w/o bootstrap
direct.bootstrap <- lapply(species.data.list, directionality, bootstrap = T) #we have ~1/2 of the species as 1.0 so in theory this should take ~30 minutes in sequence or ~10 in parallel based on above tests

#here's the parallel option if you want it
#parallel option - takes ~20 minutes on container
# Create a PSOCK cluster with 4 cores
cl <- makeCluster(4, type = "PSOCK")
# Export data and functions to the workers (essential step!)
clusterExport(cl, c("species.data.list", "directionality"))
# Run the parallel lapply
direct.bootstrap <- parLapply(
  cl,
  species.data.list,
  directionality,
  bootstrap = T
)
# Stop the cluster when done
stopCluster(cl)

#get certainty
direct.certainty <- mapply(
  directionality.certainty,
  direct.bootstrap,
  species.direct,
  SIMPLIFY = F
)
directDF <- do.call(rbind, direct.certainty)
directDF$Species <- rownames(directDF)
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
  grDevices::pdf(
    paste0(file.path(getwd(), spp.list$Name[x]), '/center_of_gravity.pdf'),
    width = 8,
    height = 11
  )
  layout(
    matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 2:13), nrow = 5, ncol = 4, byrow = T),
    height = rep(1, 4),
    width = rep(1, 4)
  )
  graphics::par(mar = c(2, 2, 1, 1))
  #annual time series
  terra::plot(
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
    terra::plot(
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
  grDevices::pdf(
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
  graphics::par(mar = c(2, 2, 1, 1))
  #annual time series
  terra::plot(
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
    terra::plot(
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
