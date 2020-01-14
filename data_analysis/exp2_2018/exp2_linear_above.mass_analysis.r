#Experiment 2 - ABOVEGROUND BIOMASS - Fitting MCMCglmm linear model, removing block effects, saving adjusted data and model for plotting.
#Also saving nls fits to adjusted data.
#clear environment, load packages.----
rm(list=ls())
source('paths.r')
library(nlme)
library(data.table)
library(lmmfit)
library(MCMCglmm)

#set output path.----
output.path <- exp2_linear_analysis_aboveground.path

#load data.----
d <- readRDS(duke_2018_to_analyze.path)
d <- data.table(d)

#grab complete cases----
d.fig <- d[,.(above_mass,suillus,fert,Block)]
d.fig <- d.fig[complete.cases(d.fig),]
#remove an extremely low soil-CO2-respiration outlier
d.fig <- d.fig[soil_ugC_h > 10,]

#Fit model.----
#errors heteroscedastic, hence log10 transformation. 
mod <- MCMCglmm(log10(above_mass) ~ fert * suillus, random = ~Block, data = d.fig, pr = T)

#Show this model fits reasonably well. However, we still need to get that non-linearity in there.
#summary(mod)
#lin.fit <- lm(log10(d.fig$above_mass) ~ predict(mod));summary(lin.fit)
#plot(log10(d.fig$above_mass) ~ predict(mod), bty = 'l')
#abline(lin.fit, lwd = 2, lty = 3)
#abline(0,1, lwd = 2)
#rsq <- round(summary(lin.fit)$r.squared, 2)
#mtext(paste0('R2=',rsq), side = 3, line = -2, adj = 0.05)

#Detrend for block effects. This basically doesn't matter in fully interactive model.----
pars <- colMeans(mod$Sol)
plot.dat <- fastDummies::dummy_cols(d.fig, select_columns = c('Block'))
#make interaction columns.
exclude <- c('suillus','fert')
plot.dat$Block <- NULL
plot.dat$fert <- NULL
colnames(plot.dat)[grep('Block',colnames(plot.dat))] <- gsub('_','.',colnames(plot.dat)[grep('Block',colnames(plot.dat))])
plot.dat <- as.data.frame(plot.dat)
plot.dat <- plot.dat[,colnames(plot.dat) %in% names(pars)]
plot.dat <- plot.dat[,!(colnames(plot.dat) %in% exclude)]
pars <- pars[names(pars) %in% colnames(plot.dat)]
pars <- pars[match(colnames(plot.dat),names(pars))]
adjust <- as.matrix(plot.dat) %*% pars
#add back in the mean number of trees, plant repiration and above_mass.
d.fig$above_mass.adj <- log10(d.fig$above_mass) - adjust

#block adjustments barely change anything.
plot(d.fig$above_mass.adj ~ log10(d.fig$above_mass));abline(0,1)
d.fig$treatment <- paste0(d.fig$suillus,d.fig$fert)
     mu <- aggregate(above_mass.adj ~ suillus*fert, data = d.fig, FUN = mean  )
std.dev <- aggregate(above_mass.adj ~ suillus*fert, data = d.fig, FUN = sd    )
      N <- aggregate(above_mass.adj ~ suillus*fert, data = d.fig, FUN = length)
detrend.sum <- data.frame(mu,std.dev[,3],N[,3])
colnames(detrend.sum)[4:5] <- c('sd','N')

#get un-detrended means and sd.----
     mu <- aggregate(above_mass ~ suillus*fert, data = d.fig, FUN = mean  )
std.dev <- aggregate(above_mass ~ suillus*fert, data = d.fig, FUN = sd    )
      N <- aggregate(above_mass ~ suillus*fert, data = d.fig, FUN = length)
gen.sum <- data.frame(mu,std.dev[,3],N[,3])
colnames(gen.sum)[4:5] <- c('sd','N')

#save model and other output.----
output <- list()
output$model <- mod
output$data  <- d.fig
output$detrend.sum <- detrend.sum
output$gen.sum <- gen.sum

#save line.----
saveRDS(output, output.path)