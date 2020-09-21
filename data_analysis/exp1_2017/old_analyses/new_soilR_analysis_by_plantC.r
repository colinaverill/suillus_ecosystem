#13C respiration data. Duke 2017 Experiment 1 - no POM addition.
#clear environment, load packages.
rm(list=ls())
source('paths.r')
library(nlme)
library(data.table)
library(lmmfit)
library(MCMCglmm)

#load data.----
d <- read.csv(Duke_2017_EMxN_master.path)
d <- data.table(d)

#remove anything where the fraction that is plant or soil is >1.-----
#removes 5 observations. There are also 2 NA observations. 113/120 total.
d <- d[!(d$plant_fracation > 1),]
d <- d[!(d$soil_fraction   > 1),]

#grab complete cases----
d.fig <- d[,.(n.trees,plant_ugC_h,soil_ugC_h,suillus_adj,fert,above_mass,grav_moist, Block)]
d.fig <- d.fig[complete.cases(d.fig),]
#remove an extremely low soil-CO2-respiration outlier
d.fig <- d.fig[soil_ugC_h > 10,]

#Fit model.----
mod <- MCMCglmm(soil_ugC_h ~ plant_ugC_h * suillus_adj + fert * suillus_adj + n.trees + above_mass, random = ~Block, data = d.fig, pr = T)
summary(mod)

#correct for block and total plant mass effects.----
pars <- colMeans(mod$Sol)
plot.dat <- fastDummies::dummy_cols(d.fig, select_columns = c('Block', 'fert'))
exclude <- c('suillus','plant_ugC_h')
plot.dat$Block <- NULL
plot.dat$fert <- NULL
colnames(plot.dat)[grep('Block',colnames(plot.dat))] <- gsub('_','.',colnames(plot.dat)[grep('Block',colnames(plot.dat))])
colnames(plot.dat)[grep('fert' ,colnames(plot.dat))] <- gsub('_','', colnames(plot.dat)[grep('fert' ,colnames(plot.dat))])
plot.dat <- as.data.frame(plot.dat)
plot.dat <- plot.dat[,colnames(plot.dat) %in% names(pars)]
plot.dat <- plot.dat[,!(colnames(plot.dat) %in% exclude)]
pars <- pars[names(pars) %in% colnames(plot.dat)]
pars <- pars[match(colnames(plot.dat),names(pars))]
adjust <- as.matrix(plot.dat) %*% pars
d.fig$resp.soil.adj <- d.fig$soil_ugC_h- adjust

#Fit non-linear relatiships between plant C allocation and soil derived respiration.----
d.fig$suillus <- ifelse(d.fig$suillus_adj == 'yes',1,0)
nls.dat <- d.fig[,c('plant_ugC_h','resp.soil.adj','suillus')]
nls.dat <- as.data.frame(nls.dat[complete.cases(nls.dat),])
all <- nls(resp.soil.adj ~ (plant_ugC_h)*(V + suillus*b1) / (plant_ugC_h + K + suillus*b2), data = nls.dat, start = list(K = 1000, V = 500, b1 = 0, b2 = 0))
  a <- nls(resp.soil.adj ~ (plant_ugC_h)*V / (plant_ugC_h + K), data = nls.dat[nls.dat$suillus == 0,], start = list(K = 1000, V = 500))
  b <- nls(resp.soil.adj ~ (plant_ugC_h)*V / (plant_ugC_h + K), data = nls.dat[nls.dat$suillus == 1,], start = list(K = 1000, V = 500))
#get lines
par <- coef(a)
x <- data.frame(seq(0, max(nls.dat$plant_ugC_h), by = 10))
colnames(x) <- c('plant_ugC_h')
y1 <- predict(a, newdata = x)
y2 <- predict(b, newdata = x)

#Plot the thing.----
plot(resp.soil.adj ~ plant_ugC_h, data = d.fig, pch = 16, col = ifelse(d$suillus_adj == 'yes','purple','red'), bty = 'l')
  lines(smooth.spline(y1 ~ x$plant_ugC_h), lwd = 2, col = 'red')
  lines(smooth.spline(y2 ~ x$plant_ugC_h), lwd = 2, col = 'purple')



#linear viz.
#abline(lm(soil_ugC_h ~ plant_ugC_h, data = d.fig), lty = 2, lwd = 2, col = 'gray', xpd = F)
#plot(log10(resp.soil.adj) ~ log10(plant_ugC_h), data = d.fig, pch = 16, col = ifelse(d$suillus_adj == 'yes','purple','red'), bty = 'l')
#abline(lm(log10(soil_ugC_h) ~ log10(plant_ugC_h), data = d.fig[d.fig$suillus_adj == 'yes',]), lty = 1, lwd = 2, col = 'purple', xpd = F)
#abline(lm(log10(soil_ugC_h) ~ log10(plant_ugC_h), data = d.fig[d.fig$suillus_adj ==  'no',]), lty = 1, lwd = 2, col = 'red', xpd = F)
