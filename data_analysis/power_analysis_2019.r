#power analysis for 2019 Suillus Experiments.
rm(list=ls())
source('paths.r')

#load data.-----
d <- readRDS(exp2_linear_analysis.path)
d <- d$data
d <- d[,c('soil.resp.adj','treatment')]
colnames(d) <- c('y','treatment')


#check assumptions.-----
#normally distributed? yes.
hist(d$y)

#errors normally distributed? yes.
mod <- lm(y ~ treatment, data = d)
hist(mod$residuals)
plot(mod$residuals ~ fitted(mod))

#Get variance by treatment, see if it varies (plots suggest it doesn't.)
mu  <- aggregate(y ~ treatment, FUN = 'mean', data = d)
var <- aggregate(y ~ treatment, FUN = 'sd'  , data = d)
study.var <- mean(var$y)

#Run power analysis at different sampling efforts, effect size 20%.
N <- c(10:20)
n.trial <- 1000
effect <- -0.25

#within sampling effort level loop.
across.levs <- list()
for(k in 1:length(N)){
  lev.result <- list()
  for(i in 1:n.trial){
    ctrl.mu <- mu[1,2]
    trmt.mu <- ctrl.mu + ctrl.mu*effect
    ctrl.sd <- var$y[1]
    trmt.sd <- var$y[5]
    ctrl <- rnorm(N[k], mean = ctrl.mu, sd = ctrl.sd)
    trmt <- rnorm(N[k], mean = trmt.mu, sd = trmt.sd)
    x <- c(rep(0, length(ctrl)), rep(1, length(trmt)))
    y <- c(ctrl, trmt)
    mod <- lm(y~x)
    par <- coef(mod)
    z <- summary(mod)$coefficients[2,4]
    out <- c(par, z)
    names(out) <- c('intercept','effect','p')
    lev.result[[i]] <- out
  }
  lev.result <- data.frame(do.call(rbind, lev.result))
  across.levs[[k]] <- lev.result
  msg <- paste0(k,' of ',length(N),' sampling efforts simulated.\n')
  cat(msg)
}
names(across.levs) <- N

#Calculate fraction of times you detect significant negative effect.----
result <- list()
for(i in 1:length(across.levs)){
  z <- across.levs[[i]]
  out <- nrow(z[z$effect < 0 & z$p < 0.05,]) / nrow(z)
  result[[i]] <- out
}
result <- unlist(result)
fig <- data.frame(N, result)
fig$result <- fig$result*100

#Plot result.----
plot(result ~ N, data = fig, bty = 'l', pch = 16, cex = 1.5, 
     xlab = 'sampling effort', ylab = 'percent of significant results')
abline(h = 95, lty = 2, col = 'light gray')

#What was the inihibition effect size in experiment 2?----
ctrl <- mu$y[1]
trt  <- mu$y[5]
#ctrl <- exp(ctrl)
#trt  <- exp(trt)
(trt - ctrl) / ctrl
