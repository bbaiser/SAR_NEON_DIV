#SEM using piecewiseSEM

#load packages
library(tidyverse)
library(dplyr)
library(sars)
library(piecewiseSEM)
library(lme4)
library(lmerTest)
library(DHARMa)
library(car)
#install.packages("semPaths")

####beetle####
#get site data
site_data<-read.csv("Data/NEON_Field_Site_Metadata_20220412.csv")%>%
           select(2,12,13,21:25)%>%
           rename(siteID=field_site_id,lat=field_latitude,long=field_longitude,mean_elev2= field_mean_elevation_m,min_elev=field_minimum_elevation_m,
           max_elev=field_maximum_elevation_m,mean_temp=field_mean_annual_temperature_C, mean_precip=field_mean_annual_precipitation_mm)

#get beetle SAR parameters
beetle_params<-read.csv("Data/beetle_params.csv")

#get beetle sampling covariates
beetle_vars<-read.csv("Data/beetle_vars.csv",row=1)

#get inter plot distances
beetle_dist<-read.csv("Data/organismalPlotMeanDist.csv",row=1)%>%
             filter(taxon=="beetle")%>%
             rename(siteID = site)%>%
             select(siteID,aveDist)
                   
#get rarefaction information

beetle_rar<-read.csv("Data/beetle_rar.csv",row=1)

#combine into one dataframe
comb_beetle<-beetle_params%>%
             rename(siteID=X)%>%
             left_join(site_data,by="siteID")%>%
             left_join(beetle_vars,by="siteID")%>%
             left_join(beetle_dist,by="siteID")%>%
             left_join(beetle_rar,by="siteID")%>%
             subset(.,siteID!="GUAN"& siteID!="PUUM"& siteID!="LAJA" &siteID!="STER")%>%#remove puerto rico and Hawaiian sites and STER as a massive outlier for c
             #filter(percent>=.80)#to filter out sites with less than 40 obs

#species richness model
colnames(comb_beetle)
hist(comb_beetle$n_observation, breaks = 15)

#model without lat because lat and temp are colinear 
beetle_rich<-lm(n_sp~aveDist+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_beetle)

vif(beetle_rich)#no multicolinearity for richness model
summary(beetle_rich)
plot(beetle_rich)




#c model
beetle_c<-lm(c~aveDist+n_sp+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_beetle)

vif(beetle_c)#no multicolinearity for c model
summary(beetle_c)
plot(beetle_c)

#z model
beetle_z<-lm(z~aveDist+c+n_sp+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_beetle)

vif(beetle_z)
summary(beetle_z)
plot(beetle_z) #point 35 is has high leverage


#nlcd div  model
beetle_nlcd<-lm(nlcd_div~long+mean_temp+mean_precip+mean_elev, data=comb_beetle)

vif(beetle_nlcd)
summary(beetle_nlcd)
plot(beetle_nlcd) #point 35 is has high leverage

#elev_cv  model
beetle_elev<-lm(elv_cv~long+mean_temp+mean_precip+mean_elev, data=comb_beetle)

vif(beetle_elev)
summary(beetle_elev)
plot(beetle_elev) #point 35 is has high leverage

beetle_sem_mod<-psem(beetle_c,beetle_z,beetle_elev,beetle_nlcd,beetle_rich)


summary(beetle_sem_mod)
plot(beetle_sem_mod)



####Small_mammals####
#get site data
site_data<-read.csv("Data/NEON_Field_Site_Metadata_20220412.csv")%>%
           select(2,12,13,21:25)%>%
           rename(siteID=field_site_id,lat=field_latitude,long=field_longitude,mean_elev2= field_mean_elevation_m,min_elev=field_minimum_elevation_m,
           max_elev=field_maximum_elevation_m,mean_temp=field_mean_annual_temperature_C, mean_precip=field_mean_annual_precipitation_mm)

#get mammal SAR parameters
mammal_params<-read.csv("Data/mammal_params.csv")

# get mammal sampling covariates
mammal_vars<-read.csv("Data/mammal_vars.csv",row=1)

#get inter plot distances
mammal_dist<-read.csv("Data/organismalPlotMeanDist.csv",row=1)%>%
             filter(taxon=="mammal")%>%
             rename(siteID = site)%>%
             select(siteID,aveDist)

#get rarefaction information

mammal_rar<-read.csv("Data/mammal_rar.csv",row=1)

#combine into one dataframe
comb_mammal<-mammal_params%>%
              rename(siteID=X)%>%
              left_join(site_data,by="siteID")%>%
              left_join(mammal_vars,by="siteID")%>%
              left_join(mammal_dist,by="siteID")%>%
              left_join(mammal_rar,by="siteID")%>%
              subset(.,siteID!="GUAN"& siteID!="PUUM"& siteID!="LAJA")%>%#remove puerto rico and Hawaiian sites 
              filter(percent>=.75)

#species richness model
dim(comb_mammal)
hist(comb_mammal$n_sp, breaks = 15)



#model without lat because lat and temp are colinear (could run atemp model) 
mammal_rich<-lm(n_sp~aveDist+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_mammal)

vif(mammal_rich)
summary(mammal_rich)
plot(mammal_rich)


#c model
mammal_c<-lm(c~aveDist+n_sp+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_mammal)

vif(mammal_c)
summary(mammal_c)
plot(mammal_c)

#z model
mammal_z<-lm(z~aveDist+c+n_sp+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_mammal)

vif(mammal_z)
summary(mammal_z)
plot(mammal_z) #point 35 is has high leverage


#nlcd div  model
mammal_nlcd<-lm(nlcd_div~long+mean_temp+mean_precip+mean_elev, data=comb_mammal)

vif(mammal_nlcd)
summary(mammal_nlcd)
plot(mammal_nlcd) #point 35 is has high leverage

#elev_cv  model
mammal_elev<-lm(elv_cv~long+mean_temp+mean_precip+mean_elev, data=comb_mammal)

vif(mammal_elev)
summary(mammal_elev)
plot(mammal_elev) #point 35 is has high leverage


#piecwise sem model
mammal_sem_mod<-psem(mammal_c,mammal_z,mammal_elev,mammal_nlcd,mammal_rich)


summary(mammal_sem_mod)
plot(mammal_sem_mod)

####birds####

#get site data
site_data<-read.csv("Data/NEON_Field_Site_Metadata_20220412.csv")%>%
           select(2,12,13,21:25)%>%
           rename(siteID=field_site_id,lat=field_latitude,long=field_longitude,mean_elev2= field_mean_elevation_m,min_elev=field_minimum_elevation_m,
           max_elev=field_maximum_elevation_m,mean_temp=field_mean_annual_temperature_C, mean_precip=field_mean_annual_precipitation_mm)

#get bird SAR parameters
bird_params<-read.csv("Data/bird_params.csv")

# get bird sampling covariates
bird_vars<-read.csv("Data/bird_vars.csv",row=1)

#get inter plot distances
bird_dist<-read.csv("Data/organismalPlotMeanDist.csv",row=1)%>%
            filter(taxon=="bird")%>%
            rename(siteID = site)%>%
            select(siteID,aveDist)

#combine into one dataframe
comb_bird<-bird_params%>%
           rename(siteID=X)%>%
            left_join(site_data,by="siteID")%>%
            left_join(bird_vars,by="siteID")%>%
            left_join(bird_dist,by="siteID")%>%
            subset(.,siteID!="GUAN"&siteID!="PUUM"&siteID!="LAJA")%>%#remove puerto rico and Hawaii sites
            filter(n_observation>=20)#to filter out sites with less than 40 obs

#species richness model
colnames(comb_bird)
hist(comb_bird$n_sp, breaks = 15)

#model without lat because lat and temp are colinear (could run atemp model) 
bird_rich<-lm(n_sp~aveDist+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_bird)

vif(bird_rich)
summary(bird_rich)
plot(bird_rich)
plot(comb_bird$n_observation,comb_bird$n_sp)


#c model
bird_c<-lm(c~aveDist+n_sp+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_bird)

vif(bird_c)
summary(bird_c)
plot(bird_c)

#z model (based on AIC, use c over n_sp due to high correlation between c and nsp)
bird_z<-lm(z~aveDist+c+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_bird)

vif(bird_z)
summary(bird_z)
plot(comb_bird$n_sp,comb_bird$z) #point 35 is has high leverage


#nlcd div  model
bird_nlcd<-lm(nlcd_div~long+mean_temp+mean_precip+mean_elev, data=comb_bird)

vif(bird_nlcd)
summary(bird_nlcd)
plot(bird_nlcd) #point 35 is has high leverage

#elev_cv  model
bird_elev<-lm(elv_cv~long+mean_temp+mean_precip+mean_elev, data=comb_bird)

vif(bird_elev)
summary(bird_elev)
plot(bird_elev) #point 35 is has high leverage


#piecwise sem model
bird_sem_mod<-psem(bird_c,bird_z,bird_elev,bird_nlcd,bird_rich)


summary(bird_sem_mod)
plot(bird_sem_mod)

####plants####

#get site data
site_data<-read.csv("Data/NEON_Field_Site_Metadata_20220412.csv")%>%
           select(2,12,13,21:25)%>%
           rename(siteID=field_site_id,lat=field_latitude,long=field_longitude,mean_elev2= field_mean_elevation_m,min_elev=field_minimum_elevation_m,
           max_elev=field_maximum_elevation_m,mean_temp=field_mean_annual_temperature_C, mean_precip=field_mean_annual_precipitation_mm)

#get plant SAR parameters
plant_params<-read.csv("Data/plant_params.csv")

# get plant sampling covariates
plant_vars<-read.csv("Data/plant_vars.csv",row=1)

plant_dist<-read.csv("Data/organismalPlotMeanDist.csv",row=1)%>%
            filter(taxon=="plant")%>%
            rename(siteID = site)%>%
            select(siteID,aveDist)

#combine into one dataframe
comb_plant<-plant_params%>%
            rename(siteID=X)%>%
            left_join(site_data,by="siteID")%>%
            left_join(plant_vars,by="siteID")%>%
            left_join(plant_dist,by="siteID")%>%
            subset(.,siteID!="GUAN"&siteID!="PUUM"&siteID!="LAJA")%>%#remove puerto rico and Hawaii sites
            filter(n_observation>=20)#to filter out sites with less than 40 obs

#species richness model
colnames(comb_plant)
hist(comb_plant$n_sp, breaks = 15)

#model without lat because lat and temp are colinear (could run atemp model) 
plant_rich<-lm(n_sp~aveDist+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_plant)

vif(plant_rich)
summary(plant_rich)
plot(plant_rich)
plot(comb_plant$n_observation,comb_plant$n_sp)


#c model
plant_c<-lm(c~aveDist+n_sp+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_plant)

vif(plant_c)
summary(plant_c)
plot(plant_c)

#z model(deal with n_sp,nobs_c colinerity )
plant_z<-lm(z~aveDist+c+n_sp++n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv, data=comb_plant)

vif(plant_z)

AIC(plant_z)
summary(plant_z)
plot(plant_z) #point 35 is has high leverage


#nlcd div  model
plant_nlcd<-lm(nlcd_div~long+mean_temp+mean_precip+mean_elev, data=comb_plant)

vif(plant_nlcd)
summary(plant_nlcd)
plot(plant_nlcd) #point 35 is has high leverage

#elev_cv  model
plant_elev<-lm(elv_cv~long+mean_temp+mean_precip+mean_elev, data=comb_plant)

vif(plant_elev)
summary(plant_elev)
plot(plant_elev) #point 35 is has high leverage


#piecwise sem model
plant_sem_mod<-psem(plant_c,plant_z,plant_elev,plant_nlcd,plant_rich)
nlcd_div %~~% n_observation


summary(plant_sem_mod)
plot(plant_sem_mod)


####all taxa model####
#select variables for the combined model and join data sets
bird_join<-comb_bird%>%
           select(siteID,z,aveDist,c,n_sp,
                  n_observation,long,mean_temp,
                  mean_precip, mean_elev,nlcd_div,elv_cv)%>%
           mutate(taxa= "bird")

beetle_join<-comb_beetle%>%
             select(siteID,z,aveDist,c,n_sp,
                   n_observation,long,mean_temp,
                   mean_precip, mean_elev,nlcd_div,elv_cv)%>%
             mutate(taxa= "beetle")

mammal_join<-comb_mammal%>%
          select(siteID,z,aveDist,c,n_sp,
                n_observation,long,mean_temp,
                mean_precip, mean_elev,nlcd_div,elv_cv)%>%
          mutate(taxa= "mammal")

plant_join<-comb_plant%>%
            select(siteID,z,aveDist,c,n_sp,
                   n_observation,long,mean_temp,
                   mean_precip, mean_elev,nlcd_div,elv_cv)%>%
            mutate(taxa= "plant") 


all_tax<-bind_rows(bird_join,beetle_join,mammal_join,plant_join)


#species richness model
hist(all_tax$n_sp, breaks = 20)
#model without lat because lat and temp are colinear (could run atemp model) 
all_rich<-lm(log(n_sp)~aveDist+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+taxa, data=all_tax)

vif(all_rich)
summary(all_rich)
plot(all_rich)


#c model

hist(log(all_tax$c))
all_c<-lm(c~aveDist+n_sp+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv+taxa, data=all_tax)

vif(all_c)
summary(all_c)
plot(all_c)

#z model
all_z<-lm(z~aveDist+c+n_sp+n_observation+long+mean_temp+mean_precip+mean_elev+nlcd_div+elv_cv+taxa, data=all_tax)

vif(all_z)
summary(all_z)
plot(all_z) #point 35 is has high leverage


#nlcd div  model
all_nlcd<-lm(nlcd_div~long+mean_temp+mean_precip+mean_elev, data=all_tax)

vif(all_nlcd)
summary(all_nlcd)
plot(all_nlcd) #point 35 is has high leverage

#elev_cv  model
all_elev<-lm(elv_cv~long+mean_temp+mean_precip+mean_elev, data=all_tax)

vif(all_elev)
summary(all_elev)
plot(all_elev) #point 35 is has high leverage

all_sem_mod<-psem(all_c,all_z,all_elev,all_nlcd,all_rich)


summary(all_sem_mod)
plot(all_sem_mod)



