##################################################################
###     Sydne Record
###     Script to calculate mean distance between organismal plots per site
##################################################################

library(sf)
library(sfdep)
library(leaflet)
library(terra)
library(geosphere)
library(tidyverse)
library(neonDivData)

# Export file path
export_path <- 'G:/Shared Drives/MacrosystemsBiodiversity/data/neon_spatial/L1'

siteMeanDist <- function(x, taxon_name){ 
  site <- unique(x$siteID)
  taxon <- rep(taxon_name, length(site))
  aveDist <- rep(NA, length(site))
  for(i in 1:length(site)){
    temp <- filter(x, siteID==site[i])
    temp2 <- temp %>%
      select(plotID, longitude, latitude)
    temp3 <- distinct(temp2)
    pairwiseDist <- geosphere::distGeo(cbind(temp3$longitude, temp3$latitude))
    aveDist[i] <- mean(pairwiseDist, na.rm=TRUE)
    #rm(list=c('temp','temp2','temp3','pairwiseDist'))
  }
  out <- cbind(site, taxon, aveDist)
  return(out)
}

# Calculate mean distance between plots for different NEON sentinel organisms
plants <- siteMeanDist(data_plant, 'plant')
mammals <- siteMeanDist(data_small_mammal, 'mammal')
beetles <- siteMeanDist(data_beetle, 'beetle')
birds <- siteMeanDist(data_bird, 'bird')

# Create one dataframe of outputs to write to L1 neon_spatial directory
allDist <- rbind(plants, mammals, beetles, birds)

write.csv(allDist, file=file.path(export_path, '/organismalPlotMeanDist.csv'))
          
          