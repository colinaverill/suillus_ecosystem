#plot Experiment 2 results, controlling for plant belowground C-allocation.
rm(list = ls())
library(MCMCglmm)
source('paths.r')

#load model fit and data.----
d <- readRDS(exp1_linear_analysis_aboveground.path)
mod <- d$model
sum <- d$detrend.sum
d <- d$data

#Get means and 95% CIs.----
colnames(sum)[3] <- 'soil.resp'
mu <- sum$soil.resp
lo95 <- mu - (sum$sd / sqrt(sum$N))
hi95 <- mu + (sum$sd / sqrt(sum$N))
mu <- 10^mu
lo95 <- 10^lo95
hi95 <- 10^hi95

#Get difference between treatments.
fert <- c(0,16.66,33.33,66.66)
#Draw from sd distribution to get variance.
set.seed(69)
diff.var <- list()
for(i in 1:1000){
  a.mu <- sum[sum$suillus == 1,]$soil.resp
  a.sd <- sum[sum$suillus == 1,]$sd
  b.mu <- sum[sum$suillus == 0,]$soil.resp
  b.sd <- sum[sum$suillus == 0,]$sd
  #Draw from distribution.
  a    <- rnorm(n = length(a.mu), mean=a.mu, sd = a.sd)
  b    <- rnorm(n = length(b.mu), mean=b.mu, sd = b.sd)
  #backlog transform, grab difference.
  diff.var[[i]] <- 10^a - 10^b
}
diff.var <- do.call(rbind, diff.var)
diff.mu <- colMeans(diff.var)
diff.sd <- apply(diff.var, 2, sd)
diff.se <- diff.sd / sqrt(mean(sum$N))
diff.lo <- diff.mu - diff.se
diff.hi <- diff.mu + diff.se

#save line.----
png('exp1_plot_abovemass.png',width = 10, height = 5, units = 'in', res = 300)

#Plot biomass results.----
limy <- c(min(lo95)*0.95 , max(hi95)*1.05)
o.cex <- 1.2
pch.code <- c(rep(c(1,16),4))
par(mfrow = c(1,2),
    mar = c(4,5,4,1))
plot(mu, ylim = limy, pch = pch.code, xlab = NA, ylab = NA, xaxt='n', bty = 'l')
mtext('Plant Biomass', side=2, line=3.5, cex = o.cex)
mtext('grams C', side = 2, line=2, cex = 1)
arrows(c(1:8), lo95, c(1:8), hi95, length=0.05, angle=90, code=3)
abline(v = 2.5, lty = 2)
abline(v = 4.5, lty = 2)
abline(v = 6.5, lty = 2)
mtext('N0', side = 1, line = -1.5, adj = 0.1)
mtext('N1', side = 1, line = -1.5, adj = 0.375)
mtext('N2', side = 1, line = -1.5, adj = 0.65)
mtext('N3', side = 1, line = -1.5, adj = 0.925)
legend(c(' (-) EM','(+) EM'), x = 'topright', bty='n', y.intersp = 2, pch = c(1,16))

#plot difference
limy <- c(min(diff.lo)*1.05, max(diff.hi)*1.05)
plot(diff.mu ~ fert, ylim = limy, pch = 16, xlab = NA, ylab = NA, bty = 'l', cex = 2)
mtext(expression(mu*'g N added * (g soil)'^-1*''), side = 1, line = 2.5)
arrows(fert, diff.lo, fert, diff.hi, length=0.05, angle=90, code=3)
abline(h = 0, lty = 3)
mtext('Biomass Response', side = 2, line = 4, cex = o.cex)
mtext('(inoculated - control)', side = 2, line = 2.5, cex = o.cex)

#add outer label.
mtext('Experiment 1: low particulate organic matter', side =3, outer = T, line = -1.75, adj = 0.05, cex = 1.5)

#end plot.----
dev.off()