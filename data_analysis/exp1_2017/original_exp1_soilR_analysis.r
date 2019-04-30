#13C respiration data. Duke 2017 Experiment 1 - no POM addition.
#clear environment, load packages.
rm(list=ls())
source('paths.r')
library(nlme)
library(data.table)
library(lmmfit)

#load data
d <- read.csv(Duke_2017_EMxN_master.path)
d <- data.table(d)

#check correlation betwen the two independent respiration metrics
#well correlated, r2 = 0.834. Bodes well for previous analysis.
plot(co2.time ~ ugC_h, data = d)
summary(lm((co2.time) ~ (ugC_h), data = d))

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
#remove an outlier
d.fig <- d.fig[soil_ugC_h > 10,]

m <- lme(log(soil_ugC_h) ~ n.trees * above_mass + suillus_adj * fert, data = d.fig, random = ~1|Block, na.action = na.omit)
m <- lme(log(plant_ugC_h) ~ suillus_adj * fert, data = d.fig, random = ~1|Block, na.action = na.omit)
qqnorm(residuals(m))
plot(residuals(m) ~ fitted(m))
summary(m)
lmmR2(m)

#r2 and visualization of overall model fit.
summary.fit <- (lm(fitted(m) ~ log(d.fig$soil_ugC_h)))
par(mfrow=c(1,1))
plot(fitted(m) ~ log(d.fig$soil_ugC_h), cex = 0.7, pch = 16)
#abline(0,1, lwd = 2)
abline(lm(fitted(m) ~ log(d.fig$soil_ugC_h)), lty =2, lwd = 2, col = 'purple')
r.sq <- as.numeric(summary(summary.fit)[8])
mtext(paste0('R2 =',round(r.sq,2)), line = -1.2, adj = 0.05)

#get correct log biomass values, accounting for moisture and block effects.
d.fig$y <- log(d.fig$soil_ugC_h)
block.effects <- data.frame(m$coefficients$random$Block)
block.effects$Block <- c(1:8)
colnames(block.effects) <- c('block_effect','Block')
d.fig <- merge(d.fig,block.effects, by = 'Block')


#subtract block effects and above_mass effects, add back in mean moisture
#d.fig$y_adj <- d.fig$y - d.fig$block_effect - d.fig$above_mass * m$coefficients$fixed[2] + mean(d.fig$n.trees)* m$coefficients$fixed[2]

#interaction between n.trees and above-mass correction
d.fig$y_adj <- d.fig$y - d.fig$block_effect - 
  d.fig$n.trees * m$coefficients$fixed[2] -
  d.fig$above_mass * m$coefficients$fixed[3] -
  d.fig$n.trees * d.fig$above_mass * m$coefficients$fixed[8] +
  mean(d.fig$n.trees) * m$coefficients$fixed[2] +
  mean(d.fig$above_mass) * m$coefficients$fixed[3] + 
  mean(d.fig$n.trees) * mean(d.fig$above_mass) * m$coefficients$fixed[8]

#convert back to original scale
d.fig$y_adj <- exp(d.fig$y_adj)

#aggregate values by treatments.
#aggregate means, standard deviation, n
out    <- aggregate(y_adj ~ suillus_adj*fert, data = d.fig, FUN = 'mean'  )
out$sd <- aggregate(y_adj ~ suillus_adj*fert, data = d.fig, FUN = 'sd'    )[,3]
out$n  <- aggregate(y_adj ~ suillus_adj*fert, data = d.fig, FUN = 'length')[,3]
out$err <- out$sd / sqrt(out$n)
out$lab <- paste0(out$suillus_adj,'.',out$fert)
pch.code <- rep(c(1,16),3)

#get difference between treatments
diff    <- out[out$suillus_adj == 'yes',]$y_adj - out[out$suillus_adj == 'no',]$y_adj
diff.sd <- sqrt((out[out$suillus_adj == 'yes',]$sd)^2    + (out[out$suillus_adj == 'no',]$sd)^2)
diff.err <- diff.sd / sqrt(mean(out$n))
fert <- c(0,16.66,33.33,66.66)

par(xpd=F)
par(mfrow = c(1,2))
plot(out$y_adj, ylim = c(50, 700), pch = pch.code, xlab = NA, ylab = 'soil derived R (ugC per hour)', xaxt='n')
arrows(c(1:8), out$y_adj-out$err, c(1:8), out$y_adj+out$err, length=0.05, angle=90, code=3)
text('N0',x=1.5, y = 50)
text('N1',x=3.5, y = 50)
text('N2',x=5.5, y = 50)
text('N3',x=7.5, y = 50)
abline(v = 2.5, lty = 2)
abline(v = 4.5, lty = 2)
abline(v = 6.5, lty = 2)
par(xpd = T)
legend(c(' (-) EM','(+) EM'), x = 1, y = 820, bty='n', y.intersp = 2, pch = c(1,16))

#plot difference
plot(diff ~ fert, ylim = c(-140, 500), pch = 16
     , xlab = 'ug N added per g soil', ylab = NA)
arrows(fert, diff - diff.err, fert, diff + diff.err, length=0.05, angle=90, code=3)
mtext('soil R response', side = 2, line = 3)
mtext('(inoculated - control)', side = 2, line = 2)

