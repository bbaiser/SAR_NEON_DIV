#load packages

if(!require("neonDivData"))
  install.packages('neonDivData', repos = c(
    daijiang = 'https://daijiang.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
library(neonDivData)
library(tidyverse)


#need to load a previous version of iNext because the current one has a bug
install.packages("devtools")
library(devtools)
install_version("iNEXT", version = "2.0.20", repos = "http://cran.us.r-project.org")
library(iNEXT)


install.packages("iNEXT.3D")
library(iNEXT.3D)
####Beetles####

good_beetle<-read.csv("Data/good_beelte_plots5.csv")

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
            select(!plotID)%>%
            column_to_rownames(var = 'sample')%>%
            t(.)


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




                       
#counts of samples for each plot  
plot_count<-data_beetle %>%
            filter(plotID%in%good_beetle$plotID)%>%#take only "good" plots
            unite('sample',plotID, boutID, remove=F )%>%
            select(sample, plotID, taxon_name) %>% 
            mutate(present = 1) %>% 
            group_by(sample,plotID, taxon_name) %>% 
            summarise(present = sum(present)/sum(present)) %>% 
            pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0)%>% 
            ungroup()%>%
            group_by(plotID)%>%
            count(plotID)%>%
            column_to_rownames(var = 'plotID')
           
#format as list of lists to run in iNEXT
lm<-split(beetle_df,beetle_df$plotID)%>%
    lapply(., function(x)x[,-1 ])%>%
    lapply(.,t)  



ass<-ObsAsy3D(lm, diversity = 'TD', q = c(0),
              datatype = "incidence_raw")

b


beetle_plot_rar<-ass%>%
            rename(siteID=Assemblage)%>%
            arrange(siteID)%>%
            group_by(siteID)%>%
            mutate(fit= qTD>=qTD.LCL[row_number()-1] & qTD<=qTD.UCL[row_number()-1])%>%#compare the observed to the lcl and ucl of the extrapolation
            mutate(percent= qTD/qTD[row_number()-1])%>%
            filter(Method=="Observed")%>%
            select(siteID,qTD,fit,percent)


ggObsAsy3D(ass, profile = "q")



####plot####

p1<-ggiNEXT(out.raw,facet.var="Assemblage", type=1)
p1+facet_wrap_paginate(~Assemblage, ncol = 3, nrow = 3, page = 3)

write.csv(beetle_plot_rar, "Data/beetle_plot_rar.csv")
