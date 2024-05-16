
#load packages

if(!require("neonDivData"))
  install.packages('neonDivData', repos = c(
    daijiang = 'https://daijiang.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
library(neonDivData)
library(tidyverse)
library(dplyr)

install.packages("sars")
library(sars)


####Beetles####
# assuming that you just want the species richness of each plotID

good_beetle<-read.csv("data/good_beelte_plots.csv",row=1)#beetle plots that have been filtered for completeness

#obtain a site by species data frame th
beetle_df <- data_beetle %>% 
             select(siteID, plotID, taxon_name) %>% 
             mutate(present = 1) %>% 
             group_by(plotID, taxon_name, siteID) %>% 
             summarise(present = sum(present)/sum(present)) %>% 
             pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0)%>%  
             remove_rownames() %>%
             filter(plotID%in%good_beetle$plotID)%>%#take only "good" plots
             column_to_rownames(var = 'plotID')  



#run species acc for one site        
#zz<-vegan::specaccum(beetle_df[,-1], method = "exact", subset = beetle_df$siteID=="ABBY")   

#make a list of site names
site_list <- unique(beetle_df$siteID)

#use for loop to loop species accumulation curves over plots in each site (using exact method)
rich<-list()

for(i in site_list) {                                              
  acc<-vegan::specaccum(beetle_df[,-1], method = "exact", subset = beetle_df$siteID==i)
  rich[[i]]<-acc$richness#extract the richness for each site number
}


#this is the number of plots at each location
plot_num<-beetle_df%>%
          count(siteID)

#just make a vector of plot numbers for loop below
plotnum<-plot_num$n

mean(plot_num$n)#Mean plot number
sd(plot_num$n)#sd plot number


#make empty matrix to fill with for loop
matrixe<-matrix(NA, length(plotnum), max(plotnum)+1)

#for loop returns the #40 as many times as there are sampling plots in a site (for loop from Isa)
for(i in 1:length(plotnum)){
  
  obj<-as.vector(plotnum[i])
  max<-plotnum[i]+1
  obj[2:max]<-rep(40,plotnum[i])
  matrixe[i,1:length(obj)]<-obj
  
}

#Matrix output
matrixe


#extract area and sprich from one site and run 
area<-na.omit(cumsum(matrixe[2,-1]))
sp_rich<-unlist(rich[2])

#for loop the sar_power function from the "sars" package over the site richness area data

#make data frame for for loop
beetle_params <- data.frame(matrix(NA,
                                   nrow = 45,
                                   ncol = 2),
                            row.names =site_list)
colnames(beetle_params)<-c("c","z")

#run forloop extracting c and z parameters
for(i in 1:45){
  
  area<-na.omit(cumsum(matrixe[i,-1]))
  sp_rich<-unlist(rich[i])
  t_dat<-cbind(area,sp_rich)
  fit <- sar_power(data=t_dat)
  beetle_params[i,1]<-fit$par[1]
  beetle_params[i,2]<-fit$par[2]
}

#data frame with parameters(note that c (intercept) is back transformed)
beetle_params

write.csv(beetle_params,"./data/beetle_params.csv")
#fit multiple models#
#fitC <- sar_multi(data = t_dat, obj = c("power", "loga", "monod"))

#fitC[1]

#fit_multi <- sar_average(data = t_dat, grid_start = "none")
#summary(fit_multi)
#plot(fit_multi)


####Plants####

#get pres/abs for each species per site

# plants is a little bit tricky, depends on which spatial scale to work with
# 1 m^2, 10 m^2, 100 m^2 or 400 m^2

# if at 1 m^2, need to use subplotID
#d = filter(data_plant, sample_area_m2 == "1")

# if at 10 m^2, need to use subplotID
#d = filter(data_plant, sample_area_m2 %in% c("1", "10"))

# if at 100 m^2, need to use subplotID
#d = filter(data_plant, sample_area_m2 %in% c("1", "10", "100"))
good_plant<-read.csv("data/good_plant_plots.csv",row=1)#beetle plots that have been filtered for completeness

# if at 400 m^2, use all data, i.e. combine all subplotID within each plotID
d = data_plant

# the code below works at 400 m^2
plant_df <- d %>% 
          select(siteID, plotID, taxon_name) %>% 
          mutate(present = 1) %>% 
          group_by(plotID, taxon_name, siteID) %>% 
          summarise(present = sum(present)/sum(present)) %>% 
          pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0)%>%  
          remove_rownames() %>%
          filter(plotID%in%good_plant$plotID)%>%#take only "good" plots
          column_to_rownames(var = 'plotID')  

#run species acc for one site        
#zz<-vegan::specaccum(plant_df[,-1], method = "exact", subset = plant_df$siteID=="ABBY")   

#make a list of site names
site_list <- unique(plant_df$siteID)

#use for loop to loop species accumulation curves over plots in each site (using exact method)
rich<-list()

for(i in site_list) {                                              
  acc<-vegan::specaccum(plant_df[,-1], method = "exact", subset = plant_df$siteID==i)
  rich[[i]]<-acc$richness#extract the richness for each site number
}


#this is the number of plots at each location
plot_num<-plant_df%>%
         count(siteID)

#plot numbers
min(plot_num$n)
max(plot_num$n)
mean(plot_num$n)
sd(plot_num$n)

#just make a vector of plot numbers for loop below
plotnum<-plot_num$n


#make empty matrix to fill with for loop
matrixe<-matrix(NA, length(plotnum), max(plotnum)+1)

#for loop returns the #40 as many times as there are sampling plots in a site (for loop from Isa)
for(i in 1:length(plotnum)){
  
  obj<-as.vector(plotnum[i])
  max<-plotnum[i]+1
  obj[2:max]<-rep(400,plotnum[i])
  matrixe[i,1:length(obj)]<-obj
  
}

#Matrix output
matrixe


#extract area and sprich from one site and run 
area<-na.omit(cumsum(matrixe[2,-1]))
sp_rich<-unlist(rich[2])

#for loop the sar_power function from the "sars" package over the site richness area data

#make data frame for for loop
plant_params <- data.frame(matrix(NA,
                                   nrow = 47,
                                   ncol = 2),
                            row.names =site_list)
colnames(plant_params)<-c("c","z")

#run forloop extracting c and z parameters
for(i in 1:47){
  
  area<-na.omit(cumsum(matrixe[i,-1]))
  sp_rich<-unlist(rich[i])
  t_dat<-cbind(area,sp_rich)
  fit <- sar_power(data=t_dat)
  plant_params[i,1]<-fit$par[1]
  plant_params[i,2]<-fit$par[2]
}

#data frame with parameters(note that c (intercept) is back transformed)
plant_params


write.csv(plant_params,"./data/plant_params.csv")
#fit multiple models#
#fitC <- sar_multi(data = t_dat, obj = c("power", "loga", "monod"))

#fitC[1]

#fit_multi <- sar_average(data = t_dat, grid_start = "none")
#summary(fit_multi)
#plot(fit_multi)




####Birds####
good_bird<-read.csv("data/good_bird_plots.csv",row=1)#beetle plots that have been filtered for completeness
#don't need to use good birds as of now because of filter out small grid "21" below
#obtain a site by species data frame th
bird_df <- data_bird %>%
        filter(!(pointID=='21')) %>%#get rid of sites to small to have grids
        select(siteID, plotID, taxon_name) %>% 
        mutate(present = 1) %>% 
        group_by(plotID, taxon_name, siteID) %>% 
        summarise(present = sum(present)/sum(present)) %>% 
        pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0)%>%  
        remove_rownames() %>%
        column_to_rownames(var = 'plotID')  


#run species acc for one site        
zz<-vegan::specaccum(bird_df[,-1], method = "exact", subset = bird_df$siteID=="OSBS")   


#plot for one site
plot(zz, ci.type = 'poly', ci.lty=0, col='blue', lwd=2, ci.col='lightblue', xlab="Plots",ylab="Species Richenss") #
#title("OSBS Birds")
#grid()


#make a list of site names
site_list <- unique(bird_df$siteID)

#use for loop to loop species accumulation curves over plots in each site (using exact method)
rich<-list()

for(i in site_list) {                                              
  acc<-vegan::specaccum(bird_df[,-1], method = "exact", subset = bird_df$siteID==i)
  rich[[i]]<-acc$richness#extract the richness for each site number
}






#this is the number of plots at each location
plot_num<-bird_df%>%
  count(siteID)

#plot numbers
min(plot_num$n)
max(plot_num$n)
mean(plot_num$n)
sd(plot_num$n)

#just make a vector of plot numbers for loop below
plotnum<-plot_num$n


#make empty matrix to fill with for loop
matrixe<-matrix(NA, length(plotnum), max(plotnum)+1)

#for loop returns the #750 as many times as there are sampling plots in a site (for loop from Isa)
for(i in 1:length(plotnum)){
  
  obj<-as.vector(plotnum[i])
  max<-plotnum[i]+1
  obj[2:max]<-rep(750,plotnum[i])
  matrixe[i,1:length(obj)]<-obj
  
}

#Matrix output
matrixe


#extract area and sprich from one site and run 
area<-na.omit(cumsum(matrixe[29,-1]))
sp_rich<-unlist(rich[29])

#plot sar for one site
#model<-lm(log10(sp_rich)~log10(area))
#summary(model)
#plot(log10(area), log10(sp_rich),pch=20, col='blue',cex.lab=1.25, lwd=2, xlab=expression(log[10](m^2)),ylab=expression(log[10](Richness)))
#abline(model, col="blue")
#title("OSBS Birds")
#grid()


#for loop the sar_power function from the "sars" package over the site richness area data

#make data frame for for loop
bird_params <- data.frame(matrix(NA,
                                   nrow = 32,
                                   ncol = 2),
                            row.names =site_list)
colnames(bird_params)<-c("c","z")

#run forloop extracting c and z parameters
for(i in 1:32){
  
  area<-na.omit(cumsum(matrixe[i,-1]))
  sp_rich<-unlist(rich[i])
  t_dat<-cbind(area,sp_rich)
  fit <- sar_power(data=t_dat)
  bird_params[i,1]<-fit$par[1]
  bird_params[i,2]<-fit$par[2]
}

#data frame with parameters(note that c (intercept) is back transformed)
bird_params

write.csv(bird_params,"./data/bird_params.csv")

#fit multiple models#
#fitC <- sar_multi(data = t_dat, obj = c("power", "loga", "monod"))

#fitC[1]

#fit_multi <- sar_average(data = t_dat, grid_start = "none")
#summary(fit_multi)
#plot(fit_multi)




####Mammals####
good_mammal<-read.csv("data/good_mammal_plots.csv",row=1)#beetle plots that have been filtered for completeness


mammal_df <- data_small_mammal %>% 
          select(plotID, taxon_name) %>% 
          mutate(present = 1) %>% 
          group_by(plotID, taxon_name) %>% 
          summarise(present = sum(present), .groups = "drop") %>%
          pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0) 

# if you want to convert it to a matrix / data frame with row names
mammal_df = as.data.frame(mammal_df)
row.names(mammal_df) = mammal_df$plotID
mammal_df$plotID = NULL

# if you just want presence/absence
mammal_df[mammal_df > 0] = 1







mammal_df <- data_small_mammal%>% 
  select(siteID, plotID, taxon_name) %>% 
  mutate(present = 1) %>% 
  group_by(plotID, taxon_name, siteID) %>% 
  summarise(present = sum(present)/sum(present)) %>% 
  pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0)%>%  
  remove_rownames() %>%
  filter(plotID%in%good_mammal$plotID)%>%#take only "good" plots
  column_to_rownames(var = 'plotID')  


#run species acc for one site        
#zz<-vegan::specaccum(mammal_df[,-1], method = "exact", subset = mammal_df$siteID=="ABBY")   

#make a list of site names
site_list <- unique(mammal_df$siteID)

#use for loop to loop species accumulation curves over plots in each site (using exact method)
rich<-list()

for(i in site_list) {                                              
  acc<-vegan::specaccum(mammal_df[,-1], method = "exact", subset = mammal_df$siteID==i)
  rich[[i]]<-acc$richness#extract the richness for each site number
}


#this is the number of plots at each location
plot_num<-mammal_df%>%
  count(siteID)

#plot numbers
min(plot_num$n)
max(plot_num$n)
mean(plot_num$n)
sd(plot_num$n)




#just make a vector of plot numbers for loop below
plotnum<-plot_num$n


#make empty matrix to fill with for loop
matrixe<-matrix(NA, length(plotnum), max(plotnum)+1)

#for loop returns the #750 as many times as there are sampling plots in a site (for loop from Isa)
for(i in 1:length(plotnum)){
  
  obj<-as.vector(plotnum[i])
  max<-plotnum[i]+1
  obj[2:max]<-rep(90,plotnum[i])
  matrixe[i,1:length(obj)]<-obj
  
}

#Matrix output
matrixe


#extract area and sprich from one site and run 
area<-na.omit(cumsum(matrixe[2,-1]))
sp_rich<-unlist(rich[2])

#for loop the sar_power function from the "sars" package over the site richness area data

#make data frame for for loop
mammal_params <- data.frame(matrix(NA,
                                 nrow = 43,
                                 ncol = 2),
                          row.names =site_list)
colnames(mammal_params)<-c("c","z")

#run forloop extracting c and z parameters
for(i in 1:43){
  
  area<-na.omit(cumsum(matrixe[i,-1]))
  sp_rich<-unlist(rich[i])
  t_dat<-cbind(area,sp_rich)
  fit <- sar_power(data=t_dat)
  mammal_params[i,1]<-fit$par[1]
  mammal_params[i,2]<-fit$par[2]
}

#data frame with parameters(note that c (intercept) is back transformed)
mammal_params

write.csv(mammal_params,"./data/mammal_params.csv")

#fit multiple models#
#fitC <- sar_multi(data = t_dat, obj = c("power", "loga", "monod"))

#fitC[1]

#fit_multi <- sar_average(data = t_dat, grid_start = "none")
#summary(fit_multi)
#plot(fit_multi)


####bacteria####
# assuming that you just want the species richness of each plotID

#bacteria_df <-read.delim("Data/NEON_16S_otu_table_reduced_rarefied_pres_abs.txt",sep=',',header=T)#otu table from Freedman Lab

#check data frame
head(bacteria_df, c(20, 5))#data frame is big, check out a section

bacteria2<-bacteria_df %>% 
           select(-c(X,siteID))%>%
           slice(1:100)%>%
           group_by(plotID)%>%
           summarise_all(sum)

bacteria[,1]
print(unique(bacteria2[,1]))

dim(bacteria2)

#check data frame
head(bacteria2, c(100, 5))#data frame is big, check out a section






  
  mutate(present = 1) %>% 
  group_by(plotID, taxon_name, siteID) %>% 
  summarise(present = sum(present)/sum(present)) %>% 
  pivot_wider(names_from = taxon_name, values_from = present, values_fill = 0)%>%  
  remove_rownames() %>%
  column_to_rownames(var = 'plotID')  
  
  head(bacteria, c(10, 5))

#run species acc for one site        
#zz<-vegan::specaccum(beetle_df[,-1], method = "exact", subset = beetle_df$siteID=="ABBY")   

#make a list of site names
site_list <- unique(beetle_df$siteID)

#use for loop to loop species accumulation curves over plots in each site (using exact method)
rich<-list()

for(i in site_list) {                                              
  acc<-vegan::specaccum(beetle_df[,-1], method = "exact", subset = beetle_df$siteID==i)
  rich[[i]]<-acc$richness#extract the richness for each site number
}


#this is the number of plots at each location
plot_num<-beetle_df%>%
  count(siteID)

#just make a vector of plot numbers for loop below
plotnum<-plot_num$n


#make empty matrix to fill with for loop
matrixe<-matrix(NA, length(plotnum), max(plotnum)+1)

#for loop returns the #40 as many times as there are sampling plots in a site (for loop from Isa)
for(i in 1:length(plotnum)){
  
  obj<-as.vector(plotnum[i])
  max<-plotnum[i]+1
  obj[2:max]<-rep(40,plotnum[i])
  matrixe[i,1:length(obj)]<-obj
  
}

#Matrix output
matrixe


#extract area and sprich from one site and run 
area<-na.omit(cumsum(matrixe[2,-1]))
sp_rich<-unlist(rich[2])

#for loop the sar_power function from the "sars" package over the site richness area data

#make data frame for for loop
beetle_params <- data.frame(matrix(NA,
                                   nrow = 47,
                                   ncol = 2),
                            row.names =site_list)
colnames(beetle_params)<-c("c","z")

#run forloop extracting c and z parameters
for(i in 1:47){
  
  area<-na.omit(cumsum(matrixe[i,-1]))
  sp_rich<-unlist(rich[i])
  t_dat<-cbind(area,sp_rich)
  fit <- sar_power(data=t_dat)
  beetle_params[i,1]<-fit$par[1]
  beetle_params[i,2]<-fit$par[2]
}

#data frame with parameters(note that c (intercept) is back transformed)
beetle_params
citation("sars")
write.csv(beetle_params,"./data/beetle_params.csv")
#fit multiple models#
#fitC <- sar_multi(data = t_dat, obj = c("power", "loga", "monod"))

#fitC[1]

#fit_multi <- sar_average(data = t_dat, grid_start = "none")
#summary(fit_multi)
#plot(fit_multi)