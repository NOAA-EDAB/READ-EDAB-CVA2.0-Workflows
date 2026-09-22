##################################
#####SET UP - LOAD EVERY TIME ####
##################################
### load packages
library(spatialcva)
library(parallel)

setwd(here::here("Sensitivity"))

##################################
##### PRELIMINARY ANALYSIS #######
##################################
### PRELIMINARY SCORES
sens <- read.csv('./Preliminary/cva2.0_sensitivity_prelim_raw_scores.csv') #load in final csv file
sens <- sens[sens$Attributes != 'Exposure Factor', ] #make sure that only sensitivity is included - expert scores were only used in sensitivity for CVA2.0 but this just makes sure there aren't any empty columns

#create list of data.frames for each species/stock
species.data.list <- split(sens, sens$Stock.Name)
species.sensitivity <- lapply(
  species.data.list,
  calculate_sensitivity,
  bootstrap = F
) #calculate sensitivity w/o bootstrap
sensitivity.bootstrap <- lapply(
  species.data.list,
  calculate_sensitivity,
  bootstrap = T
) #we have ~1/2 of the species as 1.0 so in theory this should take ~30 minutes in sequence or ~10 in parallel based on above tests

#get certainty
sensitivity.certainty <- mapply(
  calculate_sensitivity_certainty,
  sensitivity.bootstrap,
  species.sensitivity,
  SIMPLIFY = F
)
sensitivityDF <- do.call(rbind, sensitivity.certainty)
write.csv(sensitivityDF, './Preliminary/sensitivity_2_0_prelim_scores.csv') #save results

## make data.quality spreadsheet
dq <- lapply(species.data.list, calculate_data_quality)
attributeDQ <- do.call(rbind, dq)
write.csv(attributeDQ, './Preliminary/data_quality_2_0_prelim_scores.csv') #save results

#####make materials for discussions

#individual scorer reports - one pdf per scorer
makereport_scorerbarplots(
  data = sens,
  species = unique(sens$Stock.Name),
  plots.folder.name = './Preliminary/Individual_Scorer_Barplots',
  sensitivity = T,
  plotDataQuality = T,
  preliminary = T
)

#all barplots report - a single pdf with all barplots, one page per species
makereport_sensitivitybarplots(
  data = sens,
  species = unique(sens$Stock.Name),
  plots.file.name = './Preliminary/Scorer_Barplots_2_0.pdf',
  sensitivity = T,
  plotDataQuality = T,
  preliminary = T,
  plotLegend = F
)

##################################

##################################
##### FINAL ANALYSIS #############
##################################

sens <- read.csv('./Final/cva2.0_sensitivity_final_raw_scores.csv') #load in final csv file
sens <- sens[sens$Attributes != 'Exposure Factor', ] #make sure that only sensitivity is included - expert scores were only used in sensitivity for CVA2.0 but this just makes sure there aren't any empty columns

#create list of data.frames for each species/stock
species.data.list <- split(sens, sens$Stock.Name)
species.sensitivity <- lapply(
  species.data.list,
  calculate_sensitivity,
  bootstrap = F
) #calculate sensitivity w/o bootstrap
sensitivity.bootstrap <- lapply(
  species.data.list,
  calculate_sensitivity,
  bootstrap = T
) #takes about an hour for 42 species

#get certainty
sensitivity.certainty <- mapply(
  calculate_sensitivity_certainty,
  sensitivity.bootstrap,
  species.sensitivity,
  SIMPLIFY = F
)
sensitivityDF <- do.call(rbind, sensitivity.certainty)
write.csv(sensitivityDF, './Final/sensitivity_2_0_final_scores.csv') #save results

## make data.quality spreadsheet
dq <- lapply(species.data.list, calculate_data_quality)
attributeDQ <- do.call(rbind, dq)

##calculate percent of scores greater than 2 for species narratives
dq.per <- lapply(species.data.list, function(x) {
  length(which(x$Data.Quality >= 2)) / nrow(x)
})
dq.per <- do.call(rbind, dq.per)
attributeDQ$Percent.2 <- dq.per[, 1]

write.csv(attributeDQ, './Final/data_quality_2_0_final_scores.csv') #save results

#####reports
#barplot reports aren't needed for final results, so just make the final tables for species narratives

#make sensitivity tables
#reload things in if they aren't in already 
sens <- read.csv('./Final/cva2.0_sensitivity_final_raw_scores.csv') #load in final csv file
sens <- sens[sens$Attributes != 'Exposure Factor', ] #make sure that only sensitivity is included - expert scores were only used in sensitivity for CVA2.0 but this just makes sure there aren't any empty columns
sens$Stock.Name <- replace(sens$Stock.Name, sens$Stock.Name == 'Short-finned squid', 'Short finned squid')
sens$Stock.Name <- replace(sens$Stock.Name, sens$Stock.Name == 'Long-finned squid', 'Long finned squid')
sens$Stock.Name <- replace(sens$Stock.Name, sens$Stock.Name == "Smooth dogfish shark-Atlantic", 'Smooth dogfish')
sens <- split_species_stocks(sens, 'Stock.Name')
sens$Species <- gsub('/', ' ', sens$Species)

sensitivityDF <- read.csv('./Final/sensitivity_2_0_final_scores.csv')
colnames(sensitivityDF)[1] <- 'Stock.Name'
sensitivityDF$Stock.Name <- replace(sensitivityDF$Stock.Name, sensitivityDF$Stock.Name == 'Short-finned squid', 'Short finned squid')
sensitivityDF$Stock.Name <- replace(sensitivityDF$Stock.Name, sensitivityDF$Stock.Name == 'Long-finned squid', 'Long finned squid')
sensitivityDF$Stock.Name <- replace(sensitivityDF$Stock.Name, sensitivityDF$Stock.Name == "Smooth dogfish shark-Atlantic", 'Smooth dogfish')
sensitivityDF <- split_species_stocks(sensitivityDF, 'Stock.Name')
sensitivityDF$Species <- gsub('/', ' ', sensitivityDF$Species)

attributeDQ <- read.csv('./Final/data_quality_2_0_final_scores.csv')
colnames(attributeDQ)[1] <- 'Stock.Name'
attributeDQ$Stock.Name <- replace(attributeDQ$Stock.Name, attributeDQ$Stock.Name == 'Short-finned squid', 'Short finned squid')
attributeDQ$Stock.Name <- replace(attributeDQ$Stock.Name, attributeDQ$Stock.Name == 'Long-finned squid', 'Long finned squid')
attributeDQ$Stock.Name <- replace(attributeDQ$Stock.Name, attributeDQ$Stock.Name == "Smooth dogfish shark-Atlantic", 'Smooth dogfish')
attributeDQ <- split_species_stocks(attributeDQ, 'Stock.Name')
attributeDQ$Species <- gsub('/', ' ', attributeDQ$Species)

make_sensitivity_table(
  species = unique(sens$Species),
  species_col = 'Species',
  stock_col = 'Stock',
  total_sens_col = 'Total.Sensitivity',
  certainty_col = 'Certainty',
  attribute_names_raw = colnames(sensitivityDF)[4:15],
  attribute_names_clean = unique(sens$Attribute.Name),
  raw_data = sens,
  sensitivity = sensitivityDF,
  data_quality = attributeDQ,
  table_dir = './Final/Summary_Tables/'
)
