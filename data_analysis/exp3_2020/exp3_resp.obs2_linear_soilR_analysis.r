#workup CO2 data from Duke 2018 Suillus experiment, adding lots of POM.
rm(list=ls())
source('paths.r')
library(MCMCglmm)
library(lmmfit)

#load data.----
d <- readRDS(duke_2020_exp3_obs2_co2_workup.path)

#subset to positive observations.----
d <- d[d$resp.plant > 0 & d$resp.soil > 0,]
d <- d[complete.cases(d),]


#soil resp - respiring more with POM and suillus.
#trends to respire less w/ co2, less w/ suillus.
mod <- MCMCglmm(log10(resp.soil) ~ nitrogen + suillus + co2 + pom, data = d, pr =T)
summary(mod);plot(log10(d$resp.soil) - predict(mod) ~ predict(mod))

#plant resp - suillus increases plant resp, co2 decreases.
#interactions suillus by co2 - les resp under co2, less negative w/ suillus.
mod <- MCMCglmm(log10(resp.plant) ~  nitrogen + suillus + co2 + pom, data = d, pr =T)
summary(mod);plot(log10(d$resp.plant) - predict(mod) ~ predict(mod))


d <- d[d$d13C.emitted > -Inf,]
mean(d$d13C.emitted)
mean(d[d$resp.total < 0.1,]$d13C.emitted)
d[d$d13C.emitted > 0,]

#Plant-dervied respiration as a function of suillus presence.
new.dat <- data.frame(rbind(
  c(0,0,0,0), 
  c(0,0,1,0))
  )
colnames(new.dat) <- c('nitrogen','pom','suillus','co2')
new.dat$resp.plant <- NA
predict(mod, newdata=new.dat)

a <- d[d$suillus == 0,]
b <- d[d$suillus == 1,]
mu.a <- mean(a$resp.plant)
mu.b <- mean(b$resp.plant)
se.a <- sd(a$resp.plant) / sqrt(nrow(a))
se.b <- sd(b$resp.plant) / sqrt(nrow(b))
y <- c(mu.a, mu.b)
se.y <- c(se.a, se.b)
x <- c(1,2)

#png save line.
png('figures_csvs/exp3_root.resp_suillus.png', height = 5, width = 4, units = 'in', res = 300)

#figure code.
plot(y ~ x, bty = 'l', pch = 16, cex = 2, ylim = c(0,max(y+se.y)), xlim = c(0.5,2.5), xlab = NA, ylab = NA, xaxt = 'n')
arrows(x, y - se.y, x, y + se.y, length=0.05, angle=90, code=3)
mtext('Root Respiration', side = 2, line = 3)
mtext( expression('mg C * h'^-1*''), side = 2, line=1.75, cex = 1)
 text('Suillus\n absent' , x = 1, y = -0.3, xpd = T)
 text('Suillus\n present', x = 2, y = -0.3, xpd = T)

#end plot.
dev.off()

#Soil derived resipiration in the pressence / absence of POM.
a <- d[d$pom == 0,]
b <- d[d$pom == 1,]
mu.a <- mean(a$resp.soil)
mu.b <- mean(b$resp.soil)
se.a <- sd(a$resp.soil) / sqrt(nrow(a))
se.b <- sd(b$resp.soil) / sqrt(nrow(b))
y <- c(mu.a, mu.b)
se.y <- c(se.a, se.b)
x <- c(1,2)
#png save line.
png('figures_csvs/exp3_soil.resp_pom.png', height = 5, width = 4, units = 'in', res = 300)

#plot code.
plot(y ~ x, bty = 'l', pch = 16, cex = 2, ylim = c(0,max(y+se.y)), xlim = c(0.5,2.5), xlab = NA, ylab = NA, xaxt = 'n')
arrows(x, y - se.y, x, y + se.y, length=0.05, angle=90, code=3)
mtext('Soil-Derived Respiration', side = 2, line = 3)
mtext( expression('mg C * h'^-1*''), side = 2, line=1.75, cex = 1)
text('(-) POM' , x = 1, y = -0.3, xpd = T)
text('(+) POM', x = 2, y = -0.3, xpd = T)

#end plot.
dev.off()
