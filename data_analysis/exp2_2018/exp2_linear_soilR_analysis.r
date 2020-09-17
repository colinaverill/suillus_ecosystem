#Experiment 2- Fitting MCMCglmm linear model, removing block effects, saving adjusted data and model for plotting.
#Also saving nls fits to adjusted data.
#Eventually I will write a mixed effects nonlinear model, and the non-linear lines will be based on the full model fit, after detrending for fert and block.
#clear environment, load packages.----
rm(list=ls())
source('paths.r')
library(nlme)
library(data.table)
library(lmmfit)
library(MCMCglmm)
library(fastDummies)

#set output path.----
output.path <- exp2_linear_analysis.path

#load data.----
d <- readRDS(duke_2018_to_analyze.path)
d <- data.table(d)

#grab complete cases of variables of interest.----
#still need to add N trees and aboveground biomass to this model once data entered.
d.sub <- d[,.(plant.resp,soil.resp,suillus,fert,grav_moist,Block,above_mass)]
d.sub <- d.sub[complete.cases(d.sub),]


#Fit model.----
mod <- MCMCglmm(log10(soil.resp) ~ fert + suillus + log10(plant.resp)*suillus, random = ~Block, data = d.sub, pr = T)

#Check that this model fits reasonably well, visualize.
summary(mod)
total.fit <- lm(log10(d.sub$soil.resp) ~ predict(mod));summary(lin.fit)
plot(log10(d.sub$soil.resp) ~ predict(mod), bty = 'l')
abline(total.fit, lwd = 2, lty = 3)
abline(0,1, lwd = 2)
rsq <- round(summary(total.fit)$r.squared, 2)
mtext(paste0('R2=',rsq), side = 3, line = -2, adj = 0.05)

#Detrend for block effects and differences in belowground plant respiration.----- 
#Below is a bit of matrix algebra to detrend for block effects and the effect of plant respiration.
#It generates the column "soil.resp.adj", where raw soil respiration values are adjusted to account for block effects, and differences in belowground plant C allocation.
pars <- colMeans(mod$Sol)
plot.dat <- dummy_cols(d.sub, select_columns = c('Block'))

exclude <- c('suillus','grav.moist','fert')
plot.dat$Block <- NULL
plot.dat$fert <- NULL
colnames(plot.dat)[grep('Block',colnames(plot.dat))] <- gsub('_','.',colnames(plot.dat)[grep('Block',colnames(plot.dat))])
plot.dat$plant.resp <- log10(plot.dat$plant.resp)
colnames(plot.dat)[grepl('plant.resp',colnames(plot.dat))] <- 'log10(plant.resp)'
plot.dat <- as.data.frame(plot.dat)
plot.dat <- plot.dat[,colnames(plot.dat) %in% names(pars)]
plot.dat <- plot.dat[,!(colnames(plot.dat) %in% exclude)]
pars <- pars[names(pars) %in% colnames(plot.dat)]
pars <- pars[match(colnames(plot.dat),names(pars))]
adjust <- as.matrix(plot.dat) %*% pars
#add back in the mean number of trees, plant respiration
ref.dat <- plot.dat[,c('log10(plant.resp)')]
ref.dat <- colMeans(as.matrix(ref.dat))
names(ref.dat) <- 'log10(plant.resp)' #this is because we don't have other variables so it collapses to a vector earlier.
ref.par <- pars[names(pars) %in% names(ref.dat)]
ref.norm <- ref.dat %*% ref.par
d.sub$soil.resp.adj <- log10(d.sub$soil.resp) - adjust
d.sub$soil.resp.adj <- d.sub$soil.resp.adj + c(ref.norm)

#compare soil respiration values before and after accounting for block effects. Pretty correlated.
plot(d.sub$soil.resp.adj ~ log10(d.sub$soil.resp));abline(0,1)

#Summarize results.
d.sub$treatment <- paste0(d.sub$suillus,d.sub$fert)
mu <- aggregate(soil.resp.adj ~ suillus*fert, data = d.sub, FUN = mean  )
std.dev <- aggregate(soil.resp.adj ~ suillus*fert, data = d.sub, FUN = sd    )
N <- aggregate(soil.resp.adj ~ suillus*fert, data = d.sub, FUN = length)
detrend.sum <- data.frame(mu,std.dev[,3],N[,3])
colnames(detrend.sum)[4:5] <- c('sd','N')

#get un-detrended means and sd.----
mu <- aggregate(soil.resp ~ suillus*fert, data = d.sub, FUN = mean  )
std.dev <- aggregate(soil.resp ~ suillus*fert, data = d.sub, FUN = sd    )
N <- aggregate(soil.resp ~ suillus*fert, data = d.sub, FUN = length)
gen.sum <- data.frame(mu,std.dev[,3],N[,3])
colnames(gen.sum)[4:5] <- c('sd','N')

#Fit non-linear relatiships between plant C allocation and soil derived respiration.----
nls.dat <- d.sub[,c('plant.resp','soil.resp','soil.resp.adj','suillus','fert')]
nls.dat <- as.data.frame(nls.dat[complete.cases(nls.dat),])
all <- nls(soil.resp ~ (plant.resp)*(V + suillus*b1) / (plant.resp + K + suillus*b2), data = nls.dat, start = list(K = 1000, V = 500, b1 = 0, b2 = 0))
a <- nls(soil.resp ~ (plant.resp)*V / (plant.resp + K), data = nls.dat[nls.dat$suillus == 0,], start = list(K = 1000, V = 500))
b <- nls(soil.resp ~ (plant.resp)*V / (plant.resp + K), data = nls.dat[nls.dat$suillus == 1,], start = list(K = 1000, V = 500))

#get lines
par <- coef(a)
x <- data.frame(seq(0, max(nls.dat$plant.resp), by = 10))
colnames(x) <- c('plant.resp')
y1 <- predict(a, newdata = x)
y2 <- predict(b, newdata = x)


#save model and other output.----
output <- list()
output$model <- mod
output$data  <- d.sub
nonlinear <-data.frame(x,y1,y2)
colnames(nonlinear) <- c('plant.resp','no.suillus','yes.suillus')
output$nonlinear <- nonlinear
output$detrend.sum <- detrend.sum
output$gen.sum <- gen.sum
#save line.
saveRDS(output, output.path)
