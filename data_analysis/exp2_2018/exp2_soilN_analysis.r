#Experiment 2 - Nitrogen - Fitting MCMCglmm linear model, removing block effects, saving adjusted data and model for plotting.
#The only relationships w/ no3, nh4 and Ntot are w/ aboveground mass. no treatment effects.
#Nmin slightly higher in N3 treatment, p = 0.503. Only pops if you include above-Mass in model.

#clear environment, load packages.----
rm(list=ls())
source('paths.r')
library(nlme)
library(data.table)
library(lmmfit)
library(MCMCglmm)

#set output path.----

#load data.----
d <- readRDS(duke_2018_to_analyze.path)
d <- data.table(d)
d$Ntot <- d$no3 + d$nh4

#grab complete cases----
d.fig <- d[,.(no3,nh4,Ntot,Nmin,above_mass,suillus,fert,Block)]
d.fig <- d.fig[complete.cases(d.fig),]

#Fit model.----
#errors heteroscedastic, hence log10 transformation. 
#mod <- MCMCglmm(log10(Nmin) ~ fert * suillus, random = ~Block, data = d.fig, pr = T)
mod <- lm((Nmin) ~ fert*suillus + above_mass, data = d.fig[d.fig$Nmin < 500,])
summary(mod)
plot(residuals(mod) ~ fitted(mod))

#plot
plot((Ntot) ~ (above_mass), data = d.fig[d.fig$Nmin < 500,])
