#workup CO2 data from Duke 2018 Suillus experiment, adding lots of POM.
rm(list=ls())
source('paths.r')
library(MCMCglmm)
library(lmmfit)

#load data.----
d <- readRDS(duke_2020_exp3_obs1_co2_workup.path)

#subset to positive observations.----
d <- d[d$resp.plant > 0 & d$resp.soil > 0,]
d <- d[complete.cases(d),]


#soil resp - respiring more with POM. No interactons or other terms significant.
#trends to respire less w/ co2, less w/ suillus.
mod <- MCMCglmm(log10(resp.soil) ~  nitrogen +suillus + co2 + pom, random = ~block, data = d, pr =T)
summary(mod);plot(log10(d$resp.soil) - predict(mod) ~ predict(mod))

#plant resp - suillus increases plant resp, co2 decreases.
#interactions suillus by co2 - les resp under co2, less negative w/ suillus.
mod <- MCMCglmm(log10(resp.plant) ~  nitrogen + suillus + co2 + pom, random = ~block, data = d, pr =T)
summary(mod);plot(log10(d$resp.plant) - predict(mod) ~ predict(mod))

