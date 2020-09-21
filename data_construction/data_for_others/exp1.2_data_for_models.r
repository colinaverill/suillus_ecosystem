#Working up data for Chris Walter and Eddie Brzostek so they can calibrate process models.
#Goal: plant biomass above and belowground, plant belowground resp, soil-derived resp, and soil-resp adjusted for the model.
#also treatment codes, and any adjustments for mis-placed suillus treatments based on observation.
rm(list=ls())
source('paths.r')
library(data.table)

#load analyses and data from experiments 1 and 2.
d1 <- readRDS(exp1_linear_analysis.path)
d2 <- readRDS(exp2_linear_analysis.path)
dat.1 <- d1$data
dat.2 <- d2$data
dat.1 <- as.data.frame(dat.1)
dat.2 <- as.data.frame(dat.2)

#order and harmonize.
ref <- c('Block','suillus','fert','above_mass','plant.resp','soil.resp','soil.resp.adj')
dat.1 <- dat.1[,ref]
dat.2 <- dat.2[,ref]

#save outputs.
write.csv(dat.1,'figures_csvs/exp1_data_for_Chris_09.21.2020.csv')
write.csv(dat.2,'figures_csvs/exp2_data_for_Chris_09.21.2020.csv')