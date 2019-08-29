#workup CO2 data from Duke 2018 Suillus experiment, adding lots of POM.
rm(list=ls())
source('paths.r')
library(MCMCglmm)
library(lmmfit)

#set output path.----
output.path <- duke_2018_co2_workup.path

#load data.
d <- read.csv(raw_CO2_exp.2_2018.path)

#Set 2-pool 13C sources for mixing model.
soil.d13C <- -10
plant.d13C <- -26

#subtring times to get time duration.----
d$initial_hour   <- as.numeric(substr(d$time_intial,1,1))
d$final_hour     <- as.numeric(substr(d$time_final ,1,1))
d$initial_minute <- as.numeric(substr(d$time_intial,2,3))
d$final_minute   <- as.numeric(substr(d$time_final ,2,3))
d$duration <- (d$final_hour*60 + d$final_minute) - (d$initial_hour*60 + d$initial_minute)

#Break CO2 concentrations into soil vs. plant derived via mixing model.----
#Get 13C of accumulated CO2.
d$d13C_accumulated <- (d$d13C_final - d$d13C_initial*(d$ppm.CO2_intial/d$ppm.CO2_final)) / ((d$ppm.CO2_final-d$ppm.CO2_intial)/d$ppm.CO2_final)
#Caclulate fraction accumulated CO2 derived from plants vs. soil.
d$f.soil <- (d$d13C_accumulated - plant.d13C) / (soil.d13C - plant.d13C)
d$f.plant <- 1 - d$f.soil

#Calculate total respiration, soil respiration and plant respiration.----
d$resp <- (d$ppm.CO2_final - d$ppm.CO2_intial) / d$duration
d$resp.soil <- d$resp * d$f.soil
d$resp.plant <- d$resp * d$f.plant

#remove observations where fraction is greater than 1 or flux is negative.----
d$resp.soil  <- ifelse(d$f.soil     > 1, NA, d$resp.soil )
d$resp.plant <- ifelse(d$f.plant    > 1, NA, d$resp.plant)
d$resp.soil  <- ifelse(d$resp.soil  < 0, NA, d$resp.soil )
d$resp.plant <- ifelse(d$resp.plant < 0, NA, d$resp.plant)

#format and save output.----
out <- d[,c('ID','Block','suillus','fert','f.soil','f.plant','d13C_accumulated','resp','resp.soil','resp.plant')]
saveRDS(out, output.path)
