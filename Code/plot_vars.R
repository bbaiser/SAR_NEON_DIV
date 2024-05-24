#Script to extract environmental variables for each taxa

#load packages

if(!require("neonDivData"))
  install.packages('neonDivData', repos = c(
    daijiang = 'https://daijiang.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
library(neonDivData)
library(tidyverse)
library(dplyr)
library(vegan)

####Beetles####

#need to filter for plots that have enough samples and where species richness is within 95% CI of asymptotic estimator 

good_beetle<-read.csv("Data/beetle_plot_rar.csv") %>% # plots that have been filtered for completeness
             filter(fit=="TRUE") 

beetle_df <- data_beetle %>% 
             filter(plotID%in%good_beetle$plotID)#take only "good" plots
              

#calculate Shannon diversity for habitat types at each site
nlcd_div_beetle<- beetle_df%>%
                select(siteID, plotID, nlcdClass)%>%
                unique(.)%>%
                group_by(siteID,nlcdClass)%>%
                count()%>%
                pivot_wider(names_from = "nlcdClass",  values_from = n,values_fill = 0)%>%
                column_to_rownames("siteID")%>%
                mutate(nlcd_div=(diversity(.)))%>%
                select(nlcd_div)%>%
                rownames_to_column("siteID")

#Calculate cv and mean of plot elevations per site and number of habitat types for each site
elev_vars <- beetle_df%>%
               select(siteID, plotID, elevation,nlcdClass)%>%
               unique(.)%>%
               group_by(siteID)%>%
               summarise(elv_cv= sd(elevation)/mean(elevation), 
                         mean_elev=mean(elevation),
                         nlcd= n_distinct(nlcdClass))%>%
               left_join(nlcd_div_beetle,by="siteID")
           
#get site level richness, sample#, and year sampling began
beetle_vars <- beetle_df %>% 
                  mutate(plot_date = paste(plotID, observation_datetime))%>%
                  group_by(siteID) %>% 
                  summarise(n_observation = n_distinct(plot_date), # how many sample days in total?
                            start_year = min(lubridate::year(observation_datetime)), # when did it started?
                            n_sp = n_distinct(taxon_id),# number of unique species
                            n_plot = n_distinct(plotID),# number plots per site
                            .groups = "drop")%>%#why this line?
                  mutate(obsplot=n_observation/n_plot)%>%    
                  left_join(elev_vars,by="siteID")




write.csv(beetle_vars,"./Data/beetle_vars.csv")


####Birds####

#need to filter for plots that have enough samples and where species richness is within 95% CI of asymptotic estimator 
good_bird<-read.csv("data/bird_plot_rar.csv",row=1)%>%
           filter(fit=="TRUE") #don't need to use good birds as of now because of filter out small grid "21" below

bird_df <-  data_bird %>% 
            filter(plotID%in%good_bird$plotID)#take only "good" plots

#extract plot level variables
#calculate Shannon diversity for habitat types at each site
nlcd_div_bird<- bird_df %>%
                  select(siteID, plotID, nlcdClass)%>%
                  unique(.)%>%
                  group_by(siteID,nlcdClass)%>%
                  count()%>%
                  pivot_wider(names_from = "nlcdClass",  values_from = n,values_fill = 0)%>%
                  column_to_rownames("siteID")%>%
                  mutate(nlcd_div=(diversity(.)))%>%
                  select(nlcd_div)%>%
                  rownames_to_column("siteID")

#Calculate cv and mean of plot elevations per site and number of habitat types for each site
bird_elev_vars <- bird_df%>%
                  select(siteID, plotID, elevation,nlcdClass)%>%
                  unique(.)%>%
                  group_by(siteID)%>%
                  summarise(elv_cv= sd(elevation)/mean(elevation), 
                            mean_elev=mean(elevation),
                            nlcd= n_distinct(nlcdClass))%>%
                  left_join(nlcd_div_bird,by="siteID")


#get site level richness, sample#, and year sampling began
bird_vars <- bird_df %>% 
             mutate(date= as.Date(observation_datetime))%>%
             mutate(plot_date = paste(plotID, date))%>%
             group_by(siteID) %>% 
             summarise(n_observation = n_distinct(plot_date), # how many sample days in total?
                       start_year = min(lubridate::year(date)), # when did it started?
                       n_sp = n_distinct(taxon_id),# number of unique species
                       n_plot = n_distinct(plotID),# number plots per site
                          .groups = "drop")%>%#why this line?
            mutate(obsplot=n_observation/n_plot)%>%              
            left_join(bird_elev_vars,by="siteID")



write.csv(bird_vars,"./data/bird_vars.csv")

####plants####

#need to filter for plots that have enough samples and where species richness is within 95% CI of asymptotic estimator 
good_plant<-read.csv("data/plant_plot_rar.csv",row=1)%>%
            filter(fit=="TRUE") #don't need to use good birds as of now because of filter out small grid "21" below

plant_df <-  data_plant %>% 
             filter(plotID%in%good_plant$plotID)#take only "good" plots


#extract plot level variables
#calculate Shannon diversity for habitat types at each site
nlcd_div_plant<- plant_df %>%
                select(siteID, plotID, nlcdClass)%>%
                unique(.)%>%
                group_by(siteID,nlcdClass)%>%
                count()%>%
                pivot_wider(names_from = "nlcdClass",  values_from = n,values_fill = 0)%>%
                column_to_rownames("siteID")%>%
                mutate(nlcd_div=(diversity(.)))%>%
                select(nlcd_div)%>%
                rownames_to_column("siteID")

#Calculate cv and mean of plot elevations per site and number of habitat types for each site
plant_elev_vars <- plant_df %>%
                  select(siteID, plotID, elevation,nlcdClass)%>%
                  unique(.)%>%
                  group_by(siteID)%>%
                  summarise(elv_cv= sd(elevation)/mean(elevation), 
                            mean_elev=mean(elevation),
                            nlcd= n_distinct(nlcdClass))%>%
                  left_join(nlcd_div_plant,by="siteID")


#get site level richness, sample#, and year sampling began
plant_vars <- plant_df  %>% 
            mutate(plot_date = paste(plotID, observation_datetime))%>% 
            group_by(siteID) %>% 
            summarise(n_observation = n_distinct(plot_date), # how many sample days in total?
                      start_year = min(lubridate::year(observation_datetime)), # when did it started?
                      n_sp = n_distinct(taxon_id),# number of unique species
                      n_plot = n_distinct(plotID),# number plots per site
                      .groups = "drop")%>%#why this line?
             mutate(obsplot=n_observation/n_plot)%>%
            left_join(plant_elev_vars,by="siteID")


              
write.csv(plant_vars,"./Data/plant_vars.csv")


####small mammals####
good_mammal<-read.csv("data/mammal_plot_rar.csv",row=1)%>%
            filter(fit=="TRUE") #don't need to use good birds as of now because of filter out small grid "21" below

mammal_df <-  data_small_mammal %>% 
             filter(plotID%in%good_mammal$plotID)#take only "good" plots
#extract plot level variables
#calculate Shannon diversity for habitat types at each site
nlcd_div_mammal<- mammal_df%>%
                select(siteID, plotID, nlcdClass)%>%
                unique(.)%>%
                group_by(siteID,nlcdClass)%>%
                count()%>%
                pivot_wider(names_from = "nlcdClass",  values_from = n,values_fill = 0)%>%
                column_to_rownames("siteID")%>%
                mutate(nlcd_div=(diversity(.)))%>%
                select(nlcd_div)%>%
                rownames_to_column("siteID")

#Calculate cv and mean of plot elevations per site and number of habitat types for each site
mammal_elev_vars <- mammal_df%>%
                    select(siteID, plotID, elevation,nlcdClass)%>%
                    unique(.)%>%
                    group_by(siteID)%>%
                    summarise(elv_cv= sd(elevation)/mean(elevation), 
                              mean_elev=mean(elevation),
                              nlcd= n_distinct(nlcdClass))%>%
                    left_join(nlcd_div_mammal,by="siteID")


#get site level richness, sample#, and year sampling began
mammal_vars <- mammal_df %>% 
               mutate(plot_date = paste(plotID, observation_datetime))%>% 
               group_by(siteID) %>% 
               summarise(n_observation = n_distinct(plot_date), # how many sample days in total?
                         start_year = min(lubridate::year(observation_datetime)), # when did it started?
                         n_sp = n_distinct(taxon_id),# number of unique species
                         n_plot = n_distinct(plotID),# number plots per site
                         .groups = "drop")%>%#why this line?
               mutate(obsplot=n_observation/n_plot)%>%   
               left_join(mammal_elev_vars,by="siteID")

write.csv(mammal_vars,"./data/mammal_vars.csv")
v

  