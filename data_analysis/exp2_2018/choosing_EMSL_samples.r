#selecting tubes for EmSL. requires CO2 analysis.
#workup CO2 data from Duke 2018 Suillus experiment, adding lots of POM.
rm(list=ls())
source('paths.r')
library(MCMCglmm)
library(lmmfit)

#load data.
d <- read.csv(raw_CO2_exp.2_2018.path)
#co2 <- readRDS(duke_2018_co2_workup.path)
moist <- read.csv(raw_soil_moist_exp.2_2018.path)
d <- merge(d, moist[,c('ID','grav_moist')])
#emsl tubes set aside in freezer.
ref <- read.csv(exp2_available_EMSL_tubes.path)

#Set 2-pool 13C sources for mixing model.
soil.d13C <- -10
plant.d13C <- -26

#subtring times to get time duration.----
d$initial_hour   <- as.numeric(substr(d$time_intial,1,1))
d$final_hour     <- as.numeric(substr(d$time_final ,1,1))
d$initial_minute <- as.numeric(substr(d$time_intial,2,3))
d$final_minute   <- as.numeric(substr(d$time_final ,2,3))
d$duration <- (d$final_hour*60 + d$final_minute) - (d$initial_hour*60 + d$initial_minute)

#Break CO2 concentrations into soil vs. plant derived via mixing model.----
#Get 13C of accumulated CO2.
d$d13C_accumulated <- (d$d13C_final - d$d13C_initial*(d$ppm.CO2_intial/d$ppm.CO2_final)) / ((d$ppm.CO2_final-d$ppm.CO2_intial)/d$ppm.CO2_final)
#Caclulate fraction accumulated CO2 derived from plants vs. soil.
d$f.soil <- (d$d13C_accumulated - plant.d13C) / (soil.d13C - plant.d13C)
d$f.plant <- 1 - d$f.soil

#Calculate total respiration, soil respiration and plant respiration.----
d$resp <- (d$ppm.CO2_final - d$ppm.CO2_intial) / d$duration
d$resp.soil <- d$resp * d$f.soil
d$resp.plant <- d$resp * d$f.plant

#convert suillus to 0-1
d$suillus <- ifelse(d$suillus == 'yes', 1, 0)

#analyze.
library(nlme)
dat <- d[d$d13C_accumulated > plant.d13C & d$d13C_accumulated < soil.d13C & 
           d$resp > 0 & d$resp.plant > 0 & d$resp.soil > 0,]
dat <- dat[,c('ID','Block','suillus','fert','resp','resp.plant','resp.soil','grav_moist')]
dat <- dat[complete.cases(dat),]

#LME model
#mod <- lme((resp.soil) ~ grav_moist + resp.plant + fert  + suillus + resp.plant:suillus, random = ~1|Block, data = dat, na.action = na.omit)
#lmmR2(mod)
#MCMCglmm model
mod <- MCMCglmm(resp.soil ~ resp.plant + fert  + suillus + resp.plant:suillus, random = ~Block, data = dat, pr =T)
mod2 <- MCMCglmm(resp.soil ~ grav_moist + resp.plant + fert  + suillus + resp.plant:suillus, random = ~Block, data = dat, pr =T)
summary(mod)

#plot, correcting for block and fertilization main effects.
pars <- colMeans(mod$Sol)
plot.dat <- fastDummies::dummy_cols(dat, select_columns = c('Block', 'fert'))
exclude <- c('suillus','resp.plant')
plot.dat$Block <- NULL
plot.dat$fert <- NULL
colnames(plot.dat)[grep('Block',colnames(plot.dat))] <- gsub('_','.',colnames(plot.dat)[grep('Block',colnames(plot.dat))])
colnames(plot.dat)[grep('fert' ,colnames(plot.dat))] <- gsub('_','', colnames(plot.dat)[grep('fert' ,colnames(plot.dat))])
plot.dat <- plot.dat[,colnames(plot.dat) %in% names(pars)]
plot.dat <- plot.dat[,!(colnames(plot.dat) %in% exclude)]
pars <- pars[names(pars) %in% colnames(plot.dat)]
pars <- pars[match(colnames(plot.dat),names(pars))]
adjust <- as.matrix(plot.dat) %*% pars
dat$resp.soil.adj <- dat$resp.soil- adjust
#plot raw vs. adjusted side by side.
plot(resp.soil     ~ resp.plant, data = dat, col = adjustcolor(ifelse(dat$suillus == 1,'purple','red'), 0.4), pch  =16)
plot(resp.soil.adj ~ resp.plant, data = dat, 
     col = adjustcolor(ifelse(dat$suillus == 1,'purple','red'), 0.7), 
     pch  =16, ylim = c(0, max(dat$resp.soil.adj)),
     ylab = 'soil-derived respiration',
     xlab = 'plant belowground C allocation')
legend(0, 11.5, legend = c('suillus present','suillus absent'), pch = 16, col = adjustcolor(c('purple','red'), 0.7), bty = 'n')
#try drawing saturating lines.
all <- nls(resp.soil.adj ~ (resp.plant)*(V + suillus*b1) / (resp.plant + K + suillus*b2), data = dat)
a <- nls(resp.soil.adj ~ (resp.plant)*V / (resp.plant + K), data = dat[dat$suillus == 0,])
b <- nls(resp.soil.adj ~ (resp.plant)*V / (resp.plant + K), data = dat[dat$suillus == 1,])
par <- coef(a)
x <- seq(0, max(dat$resp.plant), by = 0.1)
y1 <- coef(all)[1]*x / (coef(all)[3] + x)
y2 <- (coef(all)[1] + coef(all)[2])*x / (coef(all)[3] + coef(all)[4] + x)
lines(smooth.spline(y1 ~ x), lwd = 2, col = 'red')
lines(smooth.spline(y2 ~ x), lwd = 2, col = 'purple')

#get EMSL data subset.----
#subset to C and N3 treatments, order by decreasing plant respiration.
k <- dat
k <- k[k$fert %in% c('C','N3'),]
#subset to emsl_ref
k <- k[k$ID %in% ref$available_emsl_tubes,]
k <- k[order(k$resp.plant, decreasing = T),]

#grab top 5 per treatment
#or 6 per treatment for 24 metatranscriptomes for Sunny and koko.
k$suill_fert <- paste0(k$suillus, k$fert)
for.EMSL      <- by(k, k["suill_fert"], head, n=5)
for.metatrans <- by(k, k["suill_fert"], head, n=6)
for.EMSL <- Reduce(rbind, for.EMSL)
for.metatrans <- Reduce(rbind, for.metatrans)
for.EMSL      <-      for.EMSL[,c('ID','Block','suillus','fert','grav_moist')]
for.metatrans <- for.metatrans[,c('ID','Block','suillus','fert','grav_moist')]
for.EMSL <- for.EMSL[order(for.EMSL$ID),]
for.metatrans <- for.metatrans[order(for.metatrans$ID),]

#write output
write.csv(for.EMSL     ,'Duke_2018_exp.2_for_EMSL.csv')
write.csv(for.metatrans,'Duke_2018_exp.2_for_metatranscriptome.csv')

