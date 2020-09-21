#check aboveground biomass vs. belowground biomass vs. plant-derived soil respiration values Experiment 1.
rm(list=ls())
source('paths.r')

#load data.----
d <- readRDS(Duke_2017_exp1_to_analyze.path)

#make plot.----
par(mfrow = c(1,3), mar = c(5,5,1,1))

#plot 1. Belowground repsiration ~ aboveground biomass.
y <- d$plant_ugC_h
x <- d$above_mass
dat <- data.frame(y,x)
labs <- c('plant-derived soil respiration','aboveground biomass')
mod1 <- lm(log10(y) ~ log10(x), data = dat)
plot(log10(y) ~ log10(x), bty = 'l', ylab = NA, xlab = NA, data = dat)
abline(mod1, lwd = 2)
rsq <- round(summary(mod1)$r.squared, 2)
mtext(paste0('R2=',rsq), side = 1, line = -2, adj = 0.95)
mtext(labs[1],side=2, line = 3, cex = 1.2)
mtext(labs[2],side=1, line = 3, cex = 1.2)

#plot 2. Belowground respiration ~ belowground biomass.
y <- d$plant_ugC_h
x <- d$below_mass
dat <- data.frame(y,x)
labs <- c('plant-derived soil respiration','belowground biomass')
mod1 <- lm(log10(y) ~ log10(x), data = dat)
plot(log10(y) ~ log10(x), bty = 'l', ylab = NA, xlab = NA, data = dat)
abline(mod1, lwd = 2)
rsq <- round(summary(mod1)$r.squared, 2)
mtext(paste0('R2=',rsq), side = 1, line = -2, adj = 0.95)
mtext(labs[1],side=2, line = 3, cex = 1.2)
mtext(labs[2],side=1, line = 3, cex = 1.2)

#plot 3. aboveground biomass ~ belowground biomass.
y <- d$above_mass
x <- d$below_mass
dat <- data.frame(y,x)
labs <- c('aboveground biomass','belowground biomass')
mod1 <- lm(log10(y) ~ log10(x), data = dat)
plot(log10(y) ~ log10(x), bty = 'l', ylab = NA, xlab = NA, data = dat)
abline(mod1, lwd = 2)
rsq <- round(summary(mod1)$r.squared, 2)
mtext(paste0('R2=',rsq), side = 1, line = -2, adj = 0.95)
mtext(labs[1],side=2, line = 3, cex = 1.2)
mtext(labs[2],side=1, line = 3, cex = 1.2)
