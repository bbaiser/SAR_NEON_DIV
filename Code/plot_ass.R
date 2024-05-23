#load packages

if(!require("neonDivData"))
  install.packages('neonDivData', repos = c(
    daijiang = 'https://daijiang.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
library(neonDivData)
library(tidyverse)
install.packages("iNEXT.3D")
library(iNEXT.3D)

####Beetles####

good_beetle<-read.csv("Data/good_beetle_plots_4.csv")

#make a species by sampling bout incidence (1/0) matrix
beetle_df <- data_beetle %>% 
            filter(plotID%in%good_beetle$plotID)%>%#take only "good" plots
            unite('sample',plotID, boutID, remove=F )%>%
            select(sample, plotID, taxon_name) %>% 
            mutate(present = 1) %>% 
            group_by(sample,plotID, taxon_name) %>% 
            summarise(present = sum(present)/sum(present)) %>% 
            pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0)%>%  
            ungroup()%>%
            select(!sample)

                       

#format as list of lists to run in iNEXT3D
lm<-split(beetle_df,beetle_df$plotID)%>%
    lapply(., function(x)x[,-1 ])%>%
    lapply(.,t)  

#run iNEXT3D
ass<-ObsAsy3D(lm, diversity = 'TD', q = c(0),
              datatype = "incidence_raw")



#see if observed species richness estimate falls within 95% CIs of asymptotic estimator (True/False)
beetle_plot_rar<-ass%>%
            rename(plotID=Assemblage)%>%
            arrange(plotID)%>%
            group_by(plotID)%>%
            mutate(fit= qTD>=qTD.LCL[row_number()-1] & qTD<=qTD.UCL[row_number()-1])%>%#compare the observed to the lcl and ucl of the extrapolation
            mutate(percent= qTD/qTD[row_number()-1])%>%
            filter(Method=="Observed")%>%
            select(plotID,qTD,fit,percent)

#PLOT
ggObsAsy3D(ass, profile = "q")



write.csv(beetle_plot_rar, "Data/beetle_plot_rar.csv")


####Birds####

good_bird<-read.csv("Data/good_bird_plots_4.csv")

#make a species by sampling bout incidence (1/0) matrix
bird_df <-  data_bird %>% 
              filter(plotID%in%good_bird$plotID)%>%#take only "good" plots
              unite('sample',plotID, unique_sample_id, remove=F )%>%
              select(sample, plotID, taxon_name) %>% 
              mutate(present = 1) %>% 
              group_by(sample,plotID, taxon_name) %>% 
              summarise(present = sum(present)/sum(present)) %>% 
              pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0)%>%  
              ungroup()%>%
              select(!sample)







#format as list of lists to run in iNEXT3D
lm<-split(bird_df,bird_df$plotID)%>%
    lapply(., function(x)x[,-1 ])%>%
    lapply(.,t)  


#run iNEXT3D
ass<-ObsAsy3D(lm, diversity = 'TD', q = c(0),
              datatype = "incidence_raw")



##see if observed species richness estimate falls within 95% CIs of asymptotic estimator (True/False)
bird_plot_rar<-ass%>%
            rename(plotID=Assemblage)%>%
            arrange(plotID)%>%
            group_by(plotID)%>%
            mutate(fit= qTD>=qTD.LCL[row_number()-1] & qTD<=qTD.UCL[row_number()-1])%>%#compare the observed to the lcl and ucl of the extrapolation
            mutate(percent= qTD/qTD[row_number()-1])%>%
            filter(Method=="Observed")%>%
            select(plotID,qTD,fit,percent)

#PLOT
ggObsAsy3D(ass, profile = "q")



write.csv(bird_plot_rar, "Data/bird_plot_rar.csv")
####Plants####

good_plant<-read.csv("Data/good_plant_plots_4.csv")

#make a species by sampling bout incidence (1/0) matrix
plant_df <-  data_plant %>% 
            filter(plotID%in%good_plant$plotID)%>%#take only "good" plots
            unite('sample',plotID, unique_sample_id, remove=F )%>%
            select(sample, plotID, taxon_name) %>% 
            mutate(present = 1) %>% 
            group_by(sample,plotID, taxon_name) %>% 
            summarise(present = sum(present)/sum(present)) %>% 
            pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0)%>%  
            ungroup()%>%
            select(!sample)






zz<-plant_df %>% 
  group_by(plotID) %>%
  summarise(no_rows = length(plotID))

#format as list of lists to run in iNEXT3D
lm<-split(plant_df,plant_df$plotID)%>%
    lapply(., function(x)x[,-1 ])%>%
    lapply(.,t)  


#run iNEXT3D
ass<-ObsAsy3D(lm, diversity = 'TD', q = c(0),
              datatype = "incidence_raw")


##see if observed species richness estimate falls within 95% CIs of asymptotic estimator (True/False)
plant_plot_rar<-ass%>%
              rename(plotID=Assemblage)%>%
              arrange(plotID)%>%
              group_by(plotID)%>%
              mutate(fit= qTD>=qTD.LCL[row_number()-1] & qTD<=qTD.UCL[row_number()-1])%>%#compare the observed to the lcl and ucl of the extrapolation
              mutate(percent= qTD/qTD[row_number()-1])%>%
              filter(Method=="Observed")%>%
              select(plotID,qTD,fit,percent)



#PLOT
ggObsAsy3D(ass, profile = "q")



write.csv(plant_plot_rar, "Data/plant_plot_rar.csv")


####mammals####

good_mammal<-read.csv("Data/good_mammal_plots_4.csv")

#make a species by sampling bout incidence (1/0) matrix
mammal_df <-  data_small_mammal %>% 
              filter(plotID%in%good_mammal$plotID)%>%#take only "good" plots
              unite('sample',plotID, unique_sample_id, remove=F )%>%
              select(sample, plotID, taxon_name) %>% 
              mutate(present = 1) %>% 
              group_by(sample,plotID, taxon_name) %>% 
              summarise(present = sum(present)/sum(present)) %>% 
              pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0)%>%  
              ungroup()%>%
              select(!sample)


#format as list of lists to run in iNEXT3D
lm<-split(mammal_df,mammal_df$plotID)%>%
    lapply(., function(x)x[,-1 ])%>%
    lapply(.,t)  

#run iNEXT3D

ass<-ObsAsy3D(lm, diversity = 'TD', q = c(0),
              datatype = "incidence_raw")



##see if observed species richness estimate falls within 95% CIs of asymptotic estimator (True/False)

mammal_plot_rar<-ass%>%
                rename(plotID=Assemblage)%>%
                arrange(plotID)%>%
                group_by(plotID)%>%
                mutate(fit= qTD>=qTD.LCL[row_number()-1] & qTD<=qTD.UCL[row_number()-1])%>%#compare the observed to the lcl and ucl of the extrapolation
                mutate(percent= qTD/qTD[row_number()-1])%>%
                filter(Method=="Observed")%>%
                select(plotID,qTD,fit,percent)

#PLOT
ggObsAsy3D(ass, profile = "q")



write.csv(mammal_plot_rar, "Data/mammal_plot_rar.csv")
