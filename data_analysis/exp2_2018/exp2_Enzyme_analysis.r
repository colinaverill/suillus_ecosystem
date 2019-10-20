#Experiment 2 - Enzymes - Fitting MCMCglmm linear model, removing block effects, saving adjusted data and model for plotting.
#Also saving nls fits to adjusted data.
#Above mass not important for total enzymes.
#Slightly lower total enzymes in presence of suillus (p = 0.0572)
#CBH goes down in presence of suillus (yes!) interactions with nitrogen that offset this. Nfert relaxes suppresive effect of suillus.
#No effects on AP, BG, or NAG.
#CBH effect is present when all C-degrading enzymes are considered together (CBH + BG + NAG)! N-interactions stronger, still consistent with N relaxing suppressive effect.

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
d$enz.tot <- d$BG + d$CBH + d$NAG + d$AP
d$enz.C   <- d$BG + d$CBH + d$NAG

#grab complete cases----
d.fig <- d[,.(BG,CBH,NAG,AP,enz.tot,enz.C,above_mass,suillus,fert,Block)]
d.fig <- d.fig[complete.cases(d.fig),]
d.fig <- d.fig[enz.tot < 50000,]

#Fit model.----
#errors heteroscedastic, hence log10 transformation. 
#mod <- MCMCglmm(log10(Nmin) ~ fert * suillus, random = ~Block, data = d.fig, pr = T)
mod <- lm(log10(enz.C) ~ fert*suillus, data = d.fig)
summary(mod)
plot(residuals(mod) ~ fitted(mod))

#plot
plot((Ntot) ~ (above_mass), data = d.fig[d.fig$Nmin < 500,])
