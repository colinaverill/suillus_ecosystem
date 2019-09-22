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

#merge.----
out <- merge(co2[,c('ID','Block','suillus','fert','resp.soil','resp.plant')], moist[,c('ID','grav_moist')], by = 'ID', all = T)
out <- merge(out, mass, by = 'ID', all = T)

#Rename and recode some variables.----
out <- data.table(out)
setnames(out, 'resp.soil' ,'soil.resp' )
setnames(out, 'resp.plant','plant.resp')
out$suillus <- ifelse(out$suillus == 'yes', 1, 0)
out <- as.data.frame(out)

#Save output.----
saveRDS(out, output.path)