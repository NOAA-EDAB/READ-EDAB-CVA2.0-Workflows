##################################
#####SET UP - LOAD EVERY TIME ####
##################################
##this will eventually be replaced with loading the package in
### source functions
targets::tar_source(
  "/home/kgallagher/ClimateVulnerabilityAssessment2.0/functions"
)

library(parallel)

setwd("~/ClimateVulnerabilityAssessment2.0/Sensitivity")


##################################

#####################################
#####EXAMPLE WITH CVA1.0 RESULTS ####
#####################################
#####analysis
sens <- read.csv('./Example/NEVA_Preliminary_1_0_example.csv')
sens <- sens[sens$Attributes != 'Exposure Factor', ] #only sensitivity
colnames(sens)[5] <- 'Attribute.Name'

#create list of data.frames for each species/stock
species.data.list <- split(sens, sens$Stock.Name)
species.sensitivity <- lapply(
  species.data.list,
  calculate.sensitivity,
  bootstrap = F
) #calculate sensitivity w/o bootstrap
sensitivity.bootstrap <- lapply(
  species.data.list,
  calculate.sensitivity,
  bootstrap = T
) #takes ~55 min for 82 species with 10,000 iterations in local environment in sequence; run time is similar on container

#parallel option - takes ~20 minutes on container
# Create a PSOCK cluster with 4 cores
cl <- makeCluster(4, type = "PSOCK")
# Export data and functions to the workers (essential step!)
clusterExport(
  cl,
  c(
    "species.data.list",
    "calculate.sensitivity",
    "attribute.score",
    "logic.rule",
    'bootstrap.certainty'
  )
)
# Run the parallel lapply
sensitivity.bootstrap <- parLapply(
  cl,
  species.data.list,
  calculate.sensitivity,
  bootstrap = T
)
# Stop the cluster when done
stopCluster(cl)

#get certainty
sensitivity.certainty <- mapply(
  bootstrap.certainty,
  sensitivity.bootstrap,
  species.sensitivity,
  SIMPLIFY = F
)
sensitivityDF <- do.call(rbind, sensitivity.certainty)
write.csv(sensitivityDF, './Example/sensitivity_1_0_example.csv') #save results

## make data.quality spreadsheet
dq <- lapply(species.data.list, data.quality.score)
attributeDQ <- do.call(rbind, dq)
write.csv(attributeDQ, './Example/data_quality_1_0_example.csv') #save results

#####reports
#individual scorer reports
makereport_scorerbarplots(
  data = sens,
  species = unique(sens$Stock.Name),
  plots.folder.name = './Example/Scorer_Barplots',
  sensitivity = T,
  plotDataQuality = T,
  preliminary = T
)
makereport_sensitivitybarplots(
  data = sens,
  species = unique(sens$Stock.Name),
  plots.file.name = './Example/Scorer_Barplots',
  sensitivity = T,
  plotDataQuality = T,
  preliminary = T,
  plotLegend = F
)

#make sensitivity tables
sensitivityDF <- read.csv('./Example/sensitivity_1_0_example.csv')
colnames(sensitivityDF)[1] <- 'Stock.Name'
attributeDQ <- read.csv('./Example/data_quality_1_0_example.csv')
colnames(attributeDQ)[1] <- 'Stock.Name'
makereport_sensitivitytable(
  species = unique(sens$Stock.Name),
  species_col = 'Stock.Name',
  total_sens_col = 'Total.Sensitivity',
  certainty_col = 'Certainty',
  attribute_names_raw = colnames(sensitivityDF)[2:13],
  attribute_names_clean = unique(sens$Attribute.Name),
  raw.data = sens,
  sensitivity = sensitivityDF,
  data.quality = attributeDQ,
  table.folder = './Example/Summary_Tables'
)

###################################

#####################################
#####CVA2.0 RESULTS #################
#####################################
#####analysis
sens <- read.csv('sensitivity_2_0_preliminary_from_portal.csv') #change to name of preliminary/final csv file
sens <- sens[sens$Attributes != 'Exposure Factor', ] #only sensitivity - we are only scoring sensitivity in 2.0 so you likely won't need this
colnames(sens)[5] <- 'Attribute.Name' #double check to make sure you need this too

#create list of data.frames for each species/stock
species.data.list <- split(sens, sens$Stock.Name)
species.sensitivity <- lapply(
  species.data.list,
  calculate.sensitivity,
  bootstrap = F
) #calculate sensitivity w/o bootstrap
sensitivity.bootstrap <- lapply(
  species.data.list,
  calculate.sensitivity,
  bootstrap = T
) #we have ~1/2 of the species as 1.0 so in theory this should take ~30 minutes in sequence or ~10 in parallel based on above tests

#here's the parallel option if you want it
#parallel option - takes ~20 minutes on container
# Create a PSOCK cluster with 4 cores
cl <- makeCluster(4, type = "PSOCK")
# Export data and functions to the workers (essential step!)
clusterExport(
  cl,
  c(
    "species.data.list",
    "calculate.sensitivity",
    "attribute.score",
    "logic.rule",
    'bootstrap.certainty'
  )
)
# Run the parallel lapply
sensitivity.bootstrap <- parLapply(
  cl,
  species.data.list,
  calculate.sensitivity,
  bootstrap = T
)
# Stop the cluster when done
stopCluster(cl)

#get certainty
sensitivity.certainty <- mapply(
  bootstrap.certainty,
  sensitivity.bootstrap,
  species.sensitivity,
  SIMPLIFY = F
)
sensitivityDF <- do.call(rbind, sensitivity.certainty)
write.csv(sensitivityDF, './Preliminary/sensitivity_2_0_preliminary.csv') #save results - UPDATE NAME WITH DESIRED NAME

## make data.quality spreadsheet
dq <- lapply(species.data.list, data.quality.score)
attributeDQ <- do.call(rbind, dq)
write.csv(sensitivityDF, './Preliminary/data_quality_2_0_preliminary.csv') #save results - UPDATE NAME WITH DESIRED NAME

#####reports
#individual scorer reports
makereport_scorerbarplots(
  data = sens,
  species = unique(sens$Stock.Name),
  plots.folder.name = './Preliminary/Scorer_Barplots',
  sensitivity = T,
  plotDataQuality = T,
  preliminary = T
)
makereport_sensitivitybarplots(
  data = sens,
  species = unique(sens$Stock.Name),
  plots.file.name = './Preliminary/Scorer_Barplots_2_0.pdf',
  sensitivity = T,
  plotDataQuality = T,
  preliminary = T,
  plotLegend = F
)

#make sensitivity tables
sensitivityDF <- read.csv('./Preliminary/sensitivity_2_0_preliminary.csv')
colnames(sensitivityDF)[1] <- 'Stock.Name' #make sure you  need to rename the column
attributeDQ <- read.csv('./Preliminary/data_quality_2_0_preliminary.csv')
colnames(attributeDQ)[1] <- 'Stock.Name' #make sure you need to rename the column
makereport_sensitivitytable(
  species = unique(sens$Stock.Name),
  species_col = 'Stock.Name',
  total_sens_col = 'Total.Sensitivity',
  certainty_col = 'Certainty',
  attribute_names_raw = colnames(sensitivityDF)[2:13],
  attribute_names_clean = unique(sens$Attribute.Name),
  raw.data = sens,
  sensitivity = sensitivityDF,
  data.quality = attributeDQ,
  table.folder = './Preliminary/Summary_Tables/'
)
