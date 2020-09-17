#Block randomizing experiment 3.
#16 treatments, 16 blocks, 16 experimental units per block.
#Half blocks are eCO2.
#suillus, POM, nitrogen.
rm(list=ls())
set.seed(42069) #NEVER CHANGE THIS FROM 42069 or you will get a different block randomize outcome.
blocks <- list()
name.lab <- paste0('Block_',1:16)

#loop through, make data frames.----
for(i in 1:length(name.lab)){
  suillus  <- c(rep(0,8),rep(1,8))
  pom      <- c(rep(0,4),rep(1,4),rep(0,4),rep(1,4))
  nitrogen <- c(rep(0,2),rep(1,2),rep(0,2),rep(1,2),rep(0,2),rep(1,2),rep(0,2),rep(1,2))
  if(i < 9){co2 <- rep(0,16)}
  if(i > 9){co2 <- rep(1,16)}
  rando <- runif(16,0,1)
  block <- rep(name.lab[i], 16)
  block.dat <- data.frame(block,nitrogen,pom,suillus,co2,rando)
  #order treatments based on random number.
  block.dat <- block.dat[order(block.dat$rando),]
  blocks[[i]] <- block.dat
}
names(blocks) <- name.lab


#merge data frames.----
dat <- data.frame(do.call(rbind, blocks))
rownames(dat) <- NULL
ID <- c(1:nrow(dat))
dat <- cbind(ID,dat)

#save output.----
write.csv(dat,'Experiment_3_treatment_key_randomized.csv')
