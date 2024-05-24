#filter down NEON data for plots with at least 4 sampling events


if(!require("neonDivData"))
  install.packages('neonDivData', repos = c(
    daijiang = 'https://daijiang.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
library(neonDivData)
library(tidyverse)




#### beetles####
good_beetle_plots<-data_beetle %>%
                group_by(plotID) %>%
                summarise(n_observation = n_distinct(observation_datetime))%>%
                filter(!((n_observation<4)))%>%#remove plots with less than 4 observations/sampling bouts
                select(plotID)

write.csv(good_beetle_plots,"Data/good_beetle_plots_4.csv")

#### birds####
good_bird_plots<-data_bird %>%
                  filter(!((pointID==21)))%>%
                  group_by(plotID) %>%
                  summarise(n_observation = n_distinct(observation_datetime))%>%
                  filter(!((n_observation<4)))%>%#remove plots with less than 4 observations/sampling bouts
                  select(plotID)

write.csv(good_bird_plots,"Data/good_bird_plots_4.csv")


#### plants####
good_plant_plots<-data_plant %>%
                  group_by(plotID) %>%
                  summarise(n_observation = n_distinct(observation_datetime))%>%
                  filter(!((n_observation<4)))%>%#remove plots with less than 4 observations/sampling bouts
                  select(plotID)

write.csv(good_plant_plots,"Data/good_plant_plots_4.csv")


####small mammals ####
good_mammal_plots<-data_small_mammal %>%
                  group_by(plotID) %>%
                  summarise(n_observation = n_distinct(observation_datetime))%>%
                  filter(!((n_observation<4)))%>%#remove plots with less than 5 observations/sampling bouts
                  select(plotID)

write.csv(good_mammal_plots,"Data/good_mammal_plots_4.csv")



#### ticks ----
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
