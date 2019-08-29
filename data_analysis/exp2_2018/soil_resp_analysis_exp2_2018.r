#workup CO2 data from Duke 2018 Suillus experiment, adding lots of POM.
rm(list=ls())
source('paths.r')
library(MCMCglmm)
library(lmmfit)

#load data.----
d <- read.csv(raw_CO2_exp.2_2018.path)
#co2 <- readRDS(duke_2018_co2_workup.path)
moist <- read.csv(raw_soil_moist_exp.2_2018.path)
d <- merge(d, moist[,c('ID','grav_moist')])

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
mod3 <-MCMCglmm(log10(resp.soil) ~  log10(resp.plant)*suillus + fert, random = ~Block, data = dat, pr =T)
summary(mod3)
fit <- predict(mod3)
obs <- log10(dat$resp.soil)
plot(obs ~ fit, bty = 'l')
lin.fit <- lm(obs ~ fit)
abline(lin.fit, lwd = 2, lty = 3)
abline(0,1, lwd = 2)
rsq <- round(summary(lin.fit)$r.squared,2)
mtext(paste0('R2=',rsq), line = -2, adj = 0.05)

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
k <- k[order(k$resp.plant, decreasing = T),]

#grab top 5 per treatment
#or 6 per treatment for 24 metatranscriptomes for Sunny and koko.
k$suill_fert <- paste0(k$suillus, k$fert)
test <- by(k, k["suill_fert"], head, n=6)
test <- Reduce(rbind, test)
test <- test[order(test$ID),]

for_EMSL <- test[,c('ID','Block','suillus','fert','grav_moist')]
#for_EMSL$priority <- ifelse(k$ID %in% test$ID, 1, 0 )
for_EMSL <- for_EMSL[order(for_EMSL$ID),]
write.csv(for_EMSL,'Duke_2018_exp.2_for_metatranscriptome.csv')



test <- plot.dat[,match(colnames(plot.dat),names(pars))]
pars <- order(match())
test <- plot.dat %*% pars

#N reduces soil resp.
#Suillus increases it.
#plant resp increases soil resp.
#plant resp and suillus interaction, to reduce the effect of plant resp on soil resp.
#at low plant resp suillus stimulates soil resp, at high its about the same, this isn't a N thing like last time.
#I think this implies at suillus is increasing in abundance (if more plant resp = more C allocation to ECM), its stimulatory effect on decomp is diminishing, and may be flipping.
#Wish we had bigger plants.
#If we normalized soil resp to POM, AND suillus soil had more POM (gadgil), then we might see inhibition at high plant resp.
#BUT. soil resp in POM + MAOM. may have slightly diff isotope signatures. 
#wait if you dont log transform effect sizes are bigger.
#I think cause the relationship is linear in no suillus but saturating in suillus.
dat$soil.resp.trans <- dat$resp.soil
dat$soil.resp.trans <- ifelse(dat$fert == 'N1', dat$resp.soil -1.47, d$soil.resp.trans)
dat$soil.resp.trans <- ifelse(dat$fert == 'N2', dat$resp.soil -2.06, d$soil.resp.trans)
dat$soil.resp.trans <- ifelse(dat$fert == 'N2', dat$resp.soil -2.27, d$soil.resp.trans)

plot(resp.soil ~ resp.plant, data = dat, col = adjustcolor(ifelse(dat$suillus == 'yes','purple','red'), 0.4), pch  =16)
par(mfrow = c(1,2))
plot(resp.soil ~ resp.plant, data = dat[dat$suillus == 'yes',], col = adjustcolor('purple',0.4), ylim = c(0,11), pch = 16)
plot(resp.soil ~ resp.plant, data = dat[dat$suillus == 'no',], col = adjustcolor('red',0.4), ylim = c(0,11), pch = 16)
s.resp.soil <- dat[dat$suillus == 'yes',]$resp.soil
n.resp.soil <- dat[dat$suillus == 'no' ,]$resp.soil
s.resp.plant <- dat[dat$suillus == 'yes',]$resp.plant
n.resp.plant <- dat[dat$suillus == 'no' ,]$resp.plant
lines(smooth.spline(s.resp.soil ~ s.resp.plant, spar = 0.9), col = adjustcolor('purple', 0.6), lwd = 2)
lines(smooth.spline(n.resp.soil ~ n.resp.plant), col = adjustcolor('red'   , 0.6), lwd = 2)

abline(lm(resp.soil ~ resp.plant -1, data = dat[dat$suillus == 'yes',]), col = 'purple', lwd = 2)
abline(lm(resp.soil ~ resp.plant -1, data = dat[dat$suillus == 'no' ,]), col = 'red', lwd = 2)


#generate design matrix.
mcmc <- mod$Sol
int <- c(1,1)
suillus <- c(0,1)
resp.plant <- c(7.9,7.9)
suillus_resp.plant <- c(0,7.9)
design.matrix <- data.frame(int,suillus,resp.plant,suillus_resp.plant)
design.matrix <- as.matrix(design.matrix)

#Get posteriors.
posterior <- list()
for(i in 1:1000){
  par <- mcmc[sample(nrow(mcmc),1),]
  posterior[[i]] <- t(design.matrix %*% par)
}
posterior <- do.call(rbind, posterior)
mu <- colMeans(posterior)
lo95 <- apply(posterior, 2, quantile, prob = c(0.025))
hi95 <- apply(posterior, 2, quantile, prob = c(0.975))
se <- (hi95 - lo95)/4
xpos = c(.08,.12)
limx = c(0,.2)
limy = c(0,max(se+mu)*1.2)

plot(mu ~ xpos, xlim = limx, ylim = limy, pch = 16)
arrows(xpos, mu+se, xpos, mu-se, length=0.05, angle=90, code=3)
