#13C respiration data. Duke 2017 Experiment 1 - no POM addition.
#clear environment, load packages.
rm(list=ls())
source('paths.r')
library(nlme)
library(data.table)
library(lmmfit)
library(MCMCglmm)

#load data
d <- read.csv(Duke_2017_EMxN_master.path)
d <- data.table(d)

#okay. remove anything where the fracntion that is plant or soil is >1
#removes 5 observations. There are also 2 NA observations. 113/120 total.
d <- d[!(d$plant_fracation > 1),]
d <- d[!(d$soil_fraction   > 1),]

#make N treatments quantitative
d[,fert.kg := NA]
d$fert.kg <- ifelse(d$fert == 'C', 0,
                    ifelse(d$fert == 'N1', 16.66,
                           ifelse(d$fert == 'N2', 33.33,NA))) 

#grab complete cases
d.fig <- d[,.(n.trees,plant_ugC_h,soil_ugC_h,suillus_adj,fert,above_mass,grav_moist, Block)]
d.fig <- d.fig[complete.cases(d.fig),]
#remove an extremely low soil-CO2-respiration outlier
d.fig <- d.fig[soil_ugC_h > 10,]

#
mod <- MCMCglmm(soil_ugC_h ~ plant_ugC_h * suillus_adj + fert * suillus_adj + n.trees + above_mass, random = ~Block, data = d.fig, pr = T)
#mod <- lme(log10(C.per.plant)~ grav_moist + suillus_adj + fert * suillus_adj, random = ~1|Block, data = d.fig, na.action = na.omit)
summary(mod)

plot(soil_ugC_h ~ plant_ugC_h, data = d.fig, pch = 16, col = ifelse(d$suillus_adj == 'yes','purple','red'))
abline(lm(soil_ugC_h ~ plant_ugC_h, data = d.fig), lty = 2, lwd = 2, col = 'gray', xpd = F)
abline(lm(soil_ugC_h ~ plant_ugC_h, data = d.fig[d.fig$suillus_adj == 'yes',]), lty = 1, lwd = 2, col = 'purple', xpd = F)
abline(lm(soil_ugC_h ~ plant_ugC_h, data = d.fig[d.fig$suillus_adj ==  'no',]), lty = 1, lwd = 2, col = 'red', xpd = F)
