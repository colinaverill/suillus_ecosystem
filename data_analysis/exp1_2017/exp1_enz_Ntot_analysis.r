#Experiment 1- Soil Enzyme Analysis.
#Suillus stimulates C-degrading, so does plant allocation. 
#Suillus stimulation of C-degrading goes away at highest N fert. Entirely driven by NAG.
#no bg effects. non-significant CBH trends.
#AP increases with plant C-allocation (p = 0.04).
#Suilus increases inorganic N, plant allocation reduces it.
#Highest fert increases N, suillus interaction reduces it. 
#clear environment, load packages.----
rm(list=ls())
source('paths.r')
library(nlme)
library(data.table)
library(lmmfit)
library(MCMCglmm)

#set output path.----

#load data.----
d <- read.csv(Duke_2017_EMxN_master.path)
d <- data.table(d)

d$Ntot <- d$nh4 + d$no3
d$cbh <- ifelse(d$cbh < 0, NA, d$cbh)
d$Cenz <- d$bg + d$cbh + d$nag

#model.
m <- lm(log10(ap) ~ suillus*fert + plant_ugC_h, data = d)
summary(m)
plot(residuals(m) ~ fitted(m))

