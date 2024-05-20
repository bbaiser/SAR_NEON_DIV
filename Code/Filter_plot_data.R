if(!require("neonDivData"))
  install.packages('neonDivData', repos = c(
    daijiang = 'https://daijiang.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
library(neonDivData)
library(tidyverse)




#### beetles####
View(data_beetle)
table(data_beetle$variable_name)
n_distinct(data_beetle$plotID) # 511 plots
summary(data_beetle$value) # all abundance > 0


#Get Richness per plot
sp_rich_beetle = data_beetle |> 
            group_by(siteID, plotID) |> 
            summarise(n_trap = n_distinct(trapID), # how many traps in this plot?
            n_observation = n_distinct(observation_datetime), # how many samples in total?
            start_year = min(lubridate::year(observation_datetime)), # when did it started?
            n_sp = n_distinct(taxon_id), # number of unique species
            latitude = mean(latitude, na.rm = T),
            longitude = mean(longitude, na.rm = T),
            elevation = mean(elevation, na.rm = T),
            land_class = unique(nlcdClass)[1],
            .groups = "drop")|> 
            filter(!((n_observation<4)))#remove plots with less than 5 observations
            
good_plots<-as.data.frame(sp_rich_beetle$plotID)#make a list of plot with over 5 sampling events
colnames(good_plots)<-"plotID"

write.csv(good_plots,"Data/good_beelte_plots5.csv")

#### birds####
View(data_bird)


# here, we did not consider observation durations, distance to the center, etc.
sp_rich_bird <-data_bird |> 
            group_by(siteID, plotID, pointID) |> 
            summarise(n_observation = n_distinct(observation_datetime), # how many samples in total?
            start_year = min(lubridate::year(observation_datetime)), # when did it started?
            n_sp = n_distinct(taxon_id), # number of unique species
            latitude = mean(latitude, na.rm = T),
            longitude = mean(longitude, na.rm = T),
            elevation = mean(elevation, na.rm = T),
            land_class = unique(nlcdClass)[1],
            .groups = "drop") |># WHAT DOES THIS LINE DO?
            filter(!((pointID==21)))|> #REMOVE SINGLE Points from small sites
            #filter(!((n_observation<4)))#remove plots with less than 5 observations
       
plot(sp_rich_bird$n_observation,sp_rich_bird$n_sp)
good_plots<-as.data.frame(unique((sp_rich_bird$plotID)))#make a
colnames(good_plots)<-"plotID"
 
write.csv(good_plots,"Data/good_bird_plots_21.csv")
#### plants####
View(data_plant)

# 400 m^2 (whole plot)
sp_rich_plant_400m2 = data_plant |> 
            group_by(siteID, plotID) |> 
            summarise(n_observation = n_distinct(observation_datetime), # how many samples in total?
            start_year = min(lubridate::year(observation_datetime)), # when did it started?
            n_sp = n_distinct(taxon_id), # number of unique species
            latitude = mean(latitude, na.rm = T),
            longitude = mean(longitude, na.rm = T),
            elevation = mean(elevation, na.rm = T),
            land_class = unique(nlcdClass)[1],
            .groups = "drop")|> 
             group_by(siteID, plotID) |> 
             filter(!((n_observation<4)))

good_plots<-as.data.frame(unique((sp_rich_plant_400m2$plotID)))#make a list of plotw with over 10 sampling events
colnames(good_plots)<-"plotID"

write.csv(good_plots,"Data/good_plant_plots.csv")

# small mammals ----
View(data_small_mammal)




sp_rich_mammal = data_small_mammal |> 
            group_by(siteID, plotID) |> 
            summarise(n_observation = n_distinct(observation_datetime), # how many samples in total?
            start_year = min(lubridate::year(observation_datetime)), # when did it started?
            n_sp = n_distinct(taxon_id), # number of unique species
            latitude = mean(latitude, na.rm = T),
            longitude = mean(longitude, na.rm = T),
            elevation = mean(elevation, na.rm = T),
            land_class = unique(nlcdClass)[1],
            .groups = "drop")|> 
             filter(!((n_observation<4)))#remove plots with less than 3observations

hist(sp_rich_mammal$n_observation,breaks = 20)

good_plots<-as.data.frame(unique((sp_rich_mammal$plotID)))#make a list of plotw with over 2 sampling events
colnames(good_plots)<-"plotID"

write.csv(good_plots,"Data/good_mammal_plots.csv")

# ticks ----
View(data_tick)
unique(data_tick$totalSampledArea)

data_tick |> 
  group_by(plotID) |> 
  summarise(n_unique_sample_area = n_distinct(totalSampledArea))
# hummm, the same plot can have different sampling area for different samples...


sp_rich_tick = data_tick |> 
  filter(value > 0) |> 
  group_by(siteID, plotID) |> 
  summarise(n_observation = n_distinct(observation_datetime), # how many samples in total?
            start_year = min(lubridate::year(observation_datetime)), # when did it started?
            n_sp = n_distinct(taxon_id), # number of unique species
            latitude = mean(latitude, na.rm = T),
            longitude = mean(longitude, na.rm = T),
            elevation = mean(elevation, na.rm = T),
            land_class = unique(nlcdClass)[1],
            .groups = "drop")
# ranged from 1-8 species, would this be enough?
