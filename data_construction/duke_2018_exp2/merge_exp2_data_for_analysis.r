#link together all experiment 2 data, clean up names.
rm(list=ls())
source('paths.r')
library(data.table)

#set output path.----
output.path <- duke_2018_to_analyze.path

#load data.----
co2 <- readRDS(duke_2018_co2_workup.path)
moist <- read.csv(raw_soil_moist_exp.2_2018.path)
mass  <- read.csv(duke_2018_exp2_biomass_n.trees.path)
enz   <- read.csv(duke_2018_exp2_enzymes.path)
nitrogen <- readRDS(duke_2018_nitrogen_workup.path)
soil <- read.csv(Exp2_2018_bulk.soil_isotope_data.path)

#merge.----
out <- merge(co2[,c('ID','Block','suillus','fert','resp.soil','resp.plant')], moist[,c('ID','grav_moist')], by = 'ID', all = T)
out <- merge(out, mass, by = 'ID', all = T)
out <- merge(out, nitrogen[,c('ID','no3','nh4','Nmin')], by = 'ID', all = T)
out <- merge(out, enz, by = 'ID', all = T)

#Rename and recode some variables.----
out <- data.table(out)
setnames(out, 'resp.soil' ,'soil.resp' )
setnames(out, 'resp.plant','plant.resp')
out$suillus <- ifelse(out$suillus == 'yes', 1, 0)
out <- as.data.frame(out)

#workup, merge in soil C, N and isotope data.----
soil$Sample.ID <- sub("\\-.*", "", soil$Sample.ID)
#drop apple  standards.
soil <- soil[-grep('Apple',soil$Sample.ID),]

#calculate C:N.
soil$cn <- soil$total_C_ug / soil$total_N_ug

#estimate initial som C remaining based on isotope.
old.13C <- -11
new.13C <- -26
soil$f.old <- (soil$d13C - new.13C)/(old.13C - new.13C)
soil$old.som_ugC <- soil$total_C_ug * soil$f.old
soil$new.som_ugC <- soil$total_C_ug * (1 - soil$f.old)

#subset columns of interest, merge with total dataset.
soil.sub <- soil[,c('Sample.ID','d13C','d15N','cn','old.som_ugC','new.som_ugC')]
out <- merge(out, soil.sub, by.x = 'ID',by.y = 'Sample.ID')

#Save output.----
saveRDS(out, output.path)