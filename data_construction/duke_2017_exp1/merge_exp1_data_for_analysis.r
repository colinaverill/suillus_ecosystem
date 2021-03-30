#merge 2017 experiment 1 'master' with root data. Make sure everything links to SV tables.
rm(list=ls())
source('paths.r')

#set output path.----
output.path <- Duke_2017_exp1_to_analyze.path

#load data.----
d <- read.csv(Duke_2017_EMxN_master.path)
root <- read.csv(Duke_2017_exp1_root.mass.path)
fungi <- readRDS(duke_exp1_fungi_r.abundance.path)
soil <- read.csv(Exp1_2017_bulk.soil_isotope_data.path)


#merge root data in.----
colnames(root) <- c('ID','below_mass')
d <- merge(d, root, all.x = T)

#merge fungal relative abundances in.----
d <- merge(d, fungi, all.x = T)

#Change respiration fluxes from ug to mg, and rename.----
#All respiration fluxes should be reported as mg C / h
names(d)[grepl("plant_ugC_h", names(d))] <- "plant.resp"
names(d)[grepl( "soil_ugC_h", names(d))] <-  "soil.resp"
d$plant.resp <- d$plant.resp / 1000
d$ soil.resp <- d$ soil.resp / 1000

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
d <- merge(d, soil.sub, by.x = 'ID',by.y = 'Sample.ID')

#Save output.----
saveRDS(d, output.path)
