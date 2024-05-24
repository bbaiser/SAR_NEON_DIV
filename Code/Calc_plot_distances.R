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


####data####
#beetles
good_beetle<-read.csv("Data/beetle_plot_rar.csv")%>% # plots that have been filtered for completeness
             filter(fit=="TRUE")#filter for plots where obs fell with is 95% CIs of asymptotes
 

beetle_df <- data_beetle %>% 
              select(siteID, plotID, latitude, longitude) %>% 
              filter(plotID%in%good_beetle$plotID) %>% 
              distinct(.)
#bird
good_bird<-read.csv("Data/bird_plot_rar.csv")%>% # plots that have been filtered for completeness
           filter(fit=="TRUE")#filter for plots where obs fell with is 95% CIs of asymptotic
           
  
bird_df <- data_bird %>% 
           select(siteID, plotID, latitude, longitude) %>% 
           filter(plotID%in%good_bird$plotID) %>% 
           distinct(.)


#plants
good_plant<-read.csv("Data/plant_plot_rar.csv")%>% # plots that have been filtered for completeness
            filter(fit=="TRUE")#filter for plots where obs fell with is 95% CIs of asymptotes
           

plant_df <- data_plant %>% 
            select(siteID, plotID, latitude, longitude) %>% 
            filter(plotID%in%good_plant$plotID) %>% 
            distinct(.)
     
#small mammal
good_mammal<-read.csv("data/mammal_plot_rar.csv",row=1)%>%#beetle plots that have been filtered for completeness
             filter(fit=="TRUE")

mammal_df <- data_small_mammal %>% 
              select(siteID, plotID, latitude, longitude) %>% 
              filter(plotID%in%good_mammal$plotID) %>% 
              distinct(.)



####function####

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

plants <- siteMeanDist(plant_df, 'plant')
mammals <- siteMeanDist(data_small_mammal, 'mammal')
beetles <- siteMeanDist(data_beetle, 'beetle')
birds <- siteMeanDist(data_bird, 'bird')

# Create one dataframe of outputs to write to L1 neon_spatial directory
allDist <- rbind(plants, mammals, beetles, birds)

write.csv(allDist, "Data/all_dist.csv")
          
          