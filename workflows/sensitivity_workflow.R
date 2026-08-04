##################################
#####SET UP - LOAD EVERY TIME ####
##################################
### load packages
library(spatialcva)
library(parallel)

setwd("~/Sensitivity")

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
write.csv(attributeDQ, './Final/data_quality_2_0_final_scores.csv') #save results

#####reports
#barplot reports aren't needed for final results, so just make the final tables for species narratives

#make sensitivity tables

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
  table.folder = './Final/Summary_Tables/'
)
