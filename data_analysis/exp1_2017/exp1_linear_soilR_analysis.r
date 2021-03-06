#Experiment 1- Fitting MCMCglmm linear model, removing block effects, saving adjusted data and model for plotting.
#Also saving nls fits to adjusted data.
#Eventually I will write a mixed effects nonlinear model, and the non-linear lines will be based on the full model fit, after detrending for fert and block.
#clear environment, load packages.----
rm(list=ls())
source('paths.r')
library(nlme)
library(data.table)
#library(lmmfit)
library(MCMCglmm)

#set output path.----
output.path <- exp1_linear_analysis.path

#load data.----
d <- readRDS(Duke_2017_exp1_to_analyze.path)
d <- data.table(d)

#remove anything where the fraction that is plant or soil is >1.-----
#removes 5 observations. There are also 2 NA observations. 113/120 total.
d <- d[!(d$plant_fracation > 1),]
d <- d[!(d$soil_fraction   > 1),]

#grab complete cases----
d.fig <- d[,.(n.trees,plant.resp,soil.resp,suillus_adj,fert,above_mass,grav_moist, Block)]
d.fig <- d.fig[complete.cases(d.fig),]
#remove an extremely low soil-CO2-respiration outlier
d.fig <- d.fig[soil.resp > .01,]

#Some pots had suillus that should not have according to the original design. Update treatment codes.----
d.fig$suillus <- ifelse(d.fig$suillus_adj == 'yes',1, 0)
d.fig$suillus_adj <- NULL

#Fit model.----
#mod <- MCMCglmm(log10(soil.resp) ~ fert * suillus + log10(plant.resp) + n.trees + above_mass, random = ~Block, data = d.fig, pr = T)
mod <- MCMCglmm(log10(soil.resp) ~ fert * suillus + log10(plant.resp), random = ~Block, data = d.fig, pr = T)

#Show this model fits reasonably well. However, we still need to get that non-linearity in there.
#summary(mod)
#lin.fit <- lm(log10(d.fig$soil.resp) ~ predict(mod));summary(lin.fit)
#plot(log10(d.fig$soil.resp) ~ predict(mod), bty = 'l')
#abline(lin.fit, lwd = 2, lty = 3)
#abline(0,1, lwd = 2)
#rsq <- round(summary(lin.fit)$r.squared, 2)
#mtext(paste0('R2=',rsq), side = 3, line = -2, adj = 0.05)

#Detrend for block effects. This basically doesn't matter in fully interactive model.----
#Hopefully will work in fertilization effects detrend in the future.
pars <- colMeans(mod$Sol)
#plot.dat <- fastDummies::dummy_cols(d.fig, select_columns = c('Block', 'fert'))
plot.dat <- fastDummies::dummy_cols(d.fig, select_columns = c('Block'))
#make interaction columns.
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
#add back in the mean number of trees, plant repiration and above_mass.
#ref.dat <- plot.dat[,c('n.trees','log10(plant.resp)','above_mass')]
grab <- c('log10(plant.resp)')
ref.dat <- data.frame(plot.dat[,grab])
colnames(ref.dat) <- grab
ref.dat <- colMeans(ref.dat)
ref.par <- pars[names(pars) %in% names(ref.dat)]
ref.norm <- ref.dat %*% ref.par
d.fig$soil.resp.adj <- log10(d.fig$soil.resp) - adjust
d.fig$soil.resp.adj <- d.fig$soil.resp.adj + ref.norm
plot(d.fig$soil.resp.adj ~ log10(d.fig$soil.resp));abline(0,1)
d.fig$treatment <- paste0(d.fig$suillus,d.fig$fert)
     mu <- aggregate(soil.resp.adj ~ suillus*fert, data = d.fig, FUN = mean  )
std.dev <- aggregate(soil.resp.adj ~ suillus*fert, data = d.fig, FUN = sd    )
      N <- aggregate(soil.resp.adj ~ suillus*fert, data = d.fig, FUN = length)
detrend.sum <- data.frame(mu,std.dev[,3],N[,3])
colnames(detrend.sum)[4:5] <- c('sd','N')
    
#get un-detrended means and sd.----
     mu <- aggregate(soil.resp ~ suillus*fert, data = d.fig, FUN = mean  )
std.dev <- aggregate(soil.resp ~ suillus*fert, data = d.fig, FUN = sd    )
      N <- aggregate(soil.resp ~ suillus*fert, data = d.fig, FUN = length)
gen.sum <- data.frame(mu,std.dev[,3],N[,3])
colnames(gen.sum)[4:5] <- c('sd','N')

#Fit non-linear relatiships between plant C allocation and soil derived respiration.----
nls.dat <- d.fig[,c('plant.resp','soil.resp','soil.resp.adj','suillus','fert')]
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
output$data  <- d.fig
nonlinear <-data.frame(x,y1,y2)
colnames(nonlinear) <- c('plant.resp','no.suillus','yes.suillus')
output$nonlinear <- nonlinear
output$detrend.sum <- detrend.sum
output$gen.sum <- gen.sum
#save line.
saveRDS(output, output.path)
