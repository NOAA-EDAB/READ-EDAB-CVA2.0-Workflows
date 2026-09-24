##################################
#####SET UP - LOAD EVERY TIME ####
##################################
here::i_am('workflows/READ-EDAB-CVA2.0-Workflows/workflows/sensitivity_workflow.R')

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

#data cleaning
sens$Stock.Name <- replace(sens$Stock.Name, sens$Stock.Name == 'Short-finned squid', 'Short finned squid')
sens$Stock.Name <- replace(sens$Stock.Name, sens$Stock.Name == 'Long-finned squid', 'Long finned squid')
sens$Stock.Name <- replace(sens$Stock.Name, sens$Stock.Name == "Smooth dogfish shark-Atlantic", 'Smooth dogfish')
sens <- split_species_stocks(sens, 'Stock.Name')
sens$Species <- gsub('/', ' ', sens$Species)

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

##calculate "global" sensitivities for species with multiple stocks 
sensG <- sens[!is.na(sens$Stock),] #remove species with Stock == NA to subset to only species that had stocks

#create list of data.frames for each species/stock
species.data.list <- split(sensG, sensG$Species)
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
sensitivityG <- do.call(rbind, sensitivity.certainty)

#add 'global' tag to sensitivityG to merge 
rownames(sensitivityG) <- paste(rownames(sensitivityG), 'global', sep = '-')

#combine
sensitivity <- rbind(sensitivityDF, sensitivityG)
sensitivity <- sensitivity[order(rownames(sensitivity)),]

#save
write.csv(sensitivity, './Final/sensitivity_2_0_final_scores_v2.csv') #save results

##### make data.quality spreadsheet
#by stocks 
species.data.list <- split(sens, sens$Stock.Name)
dq <- lapply(species.data.list, calculate_data_quality)
attributeDQ <- do.call(rbind, dq)

##calculate percent of scores greater than 2 for species narratives
dq.per <- lapply(species.data.list, function(x) {
  length(which(x$Data.Quality >= 2)) / nrow(x)
})
dq.per <- do.call(rbind, dq.per)
attributeDQ$Percent.2 <- dq.per[, 1]

#globally 
species.data.list <- split(sensG, sensG$Species)
dqG <- lapply(species.data.list, calculate_data_quality, id_col = 'Species')
attributeG <- do.call(rbind, dqG)

##calculate percent of scores greater than 2 for species narratives
dq.per <- lapply(species.data.list, function(x) {
  length(which(x$Data.Quality >= 2)) / nrow(x)
})
dq.per <- do.call(rbind, dq.per)
attributeG$Percent.2 <- dq.per[, 1]

#add 'global' tag to attributeG to merge 
rownames(attributeG) <- paste(rownames(attributeG), 'global', sep = '-')

#combine
attribute <- rbind(attributeDQ, attributeG)
attribute <- attribute[order(rownames(attribute)),]

write.csv(attribute, './Final/data_quality_2_0_final_scores_v2.csv') #save results

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

##calculate "global" sensitivities for species with multiple stocks 
sensG <- sens[!is.na(sens$Stock),] #remove species with Stock == NA to subset to only species that had stocks
sensG$Stock.Name <- paste(sensG$Species, 'global', sep = '-')
sensG$Stock <- 'global'

sensAll <- rbind(sens, sensG)
sensAll$Species <- gsub('/', ' ', sensAll$Species)

sensitivityDF <- read.csv('./Final/sensitivity_2_0_final_scores_v2.csv')
colnames(sensitivityDF)[1] <- 'Stock.Name'
sensitivityDF <- split_species_stocks(sensitivityDF, 'Stock.Name')
sensitivityDF$Species <- gsub('/', ' ', sensitivityDF$Species)

attributeDQ <- read.csv('./Final/data_quality_2_0_final_scores_v2.csv')
colnames(attributeDQ)[1] <- 'Stock.Name'
attributeDQ <- split_species_stocks(attributeDQ, 'Stock.Name')
attributeDQ$Species <- gsub('/', ' ', attributeDQ$Species)

clean_attributes <- unique(sens$Attribute.Name)
clean_attributes[5] <- 'Historical Variability in Temperature'

ns_stocks <- c('global', 'Northern', 'Eastern Gulf of Maine', 'Western Gulf of Maine', 'Gulf of Maine', 'Gulf of Maine/Georges Bank', 'Georges Bank', 'Gulf of Maine/Cape Cod', 'Southern New England', 'MA/RI', 'NJ/NY', 'Southern New England/Mid-Atlantic', 'Long Island Sound', 'Mid Atlantic Bight', 'DE/MA/VA', 'Southern')

make_sensitivity_table(
  species = unique(sensAll$Species),
  species_col = 'Species',
  stock_col = 'Stock',
  total_sens_col = 'Total.Sensitivity',
  certainty_col = 'Certainty',
  attribute_names_raw = colnames(sensitivityDF)[4:15],
  attribute_names_clean = clean_attributes,
  stock_order = ns_stocks,
  raw_data = sensAll,
  sensitivity = sensitivityDF,
  data_quality = attributeDQ,
  table_dir = './Final/Summary_Tables/'
)

#### make rasters for vulnerability calculation 
#get bathymetry for masking
statics <- terra::rast('../SDMs/Data/staticVariables_cropped_terra_reproj.tif')
bathy <- statics$bathy

#load a exposure raster to use as a template 
template <- terra::rast(paste0(
  file.path(here::here('Exposure'), 'Atlanticcod', 'Data',
    'total_exposure_map_all_var_r20250925_i202501_r20250715_20142023.tif')
))

for(x in 1:length(unique(sensitivityDF$Species))){
  srast <- make_sensitivity_raster(
      species = unique(sensitivityDF$Species)[x],
      sensitivity = sensitivityDF,
      species_col = 'Species',
      stock_col = 'Stock',
      total_sens_col = 'Total.Sensitivity',
      bathymetry = bathy,
      bathymetry_range = c(-1000, 0),
      r_template = template
    )
  
  terra::writeRaster(
    x = srast,
    filename = paste0(
      here::here('Sensitivity/Final/rasters'),
      paste0(
        '/',
        gsub(" ", '', unique(sensitivityDF$Species)[x]),
        '_',
        'sensitivity_v1.tif'
      )
    ),
    overwrite = TRUE
  )
  
  print(unique(sensitivityDF$Species)[x])
  plot(srast, main = unique(sensitivityDF$Species)[x])
}
