#choose samples for POM-MAOM analysis.
rm(list=ls())
source('paths.r')

#output path.----
output.path <- 'POM_MAOM_IDs_for_Eddie_Jan_2020.csv'

#set RNG seed.----
set.seed(42069)

#load data.----
d1 <- readRDS(Duke_2017_exp1_to_analyze.path)
d2 <- readRDS(duke_2018_to_analyze.path)

#grab 3 treatments of interest.----
d1 <- d1[d1$fert == 'C' | d1$fert == 'N3',]
d1 <- d1[!(d1$fert == 'N3' & d1$suillus_adj == 'no'),]
d2 <- d2[d2$fert == 'C' | d2$fert == 'N3',]
d2 <- d2[!(d2$fert == 'N3' & d2$suillus == 0),]

#rename somethings, get a single treatment code field.
d1$suillus <- d1$suillus_adj
d1$suillus <- ifelse(d1$suillus == 'yes',1,0)
d1$treatment <- paste0(d1$fert,'_',d1$suillus)
d2$treatment <- paste0(d2$fert,'_',d2$suillus)

#Make sure everything has complete plant and soil respiration fluxes and biomass.----
d1.check <- d1[,c('ID','Block','suillus','fert','plant_ugC_h','soil_ugC_h','above_mass','treatment')]
d1.check <- d1.check[complete.cases(d1.check),]
d2.check <- d2[,c('ID','Block','suillus','fert','plant.resp','soil.resp','above_mass','treatment')]
d2.check <- d2.check[complete.cases(d2.check),]

#randomly grab 10 of each treatment per data set.----
#subset d1.
d1.sub <- list()
treatments <- unique(d1.check$treatment)
n.samp <- 10
for(i in 1:length(treatments)){
  sub <- d1.check[d1.check$treatment == treatments[i],]
  sub <- sub[sample(nrow(sub),n.samp),]
  d1.sub[[i]] <- sub
}
d1.sub <- data.frame(do.call(rbind, d1.sub))
d1.sub$experiment <- 'exp_1'
#subset d2.
d2.sub <- list()
treatments <- unique(d2.check$treatment)
for(i in 1:length(treatments)){
  sub <- d2.check[d2.check$treatment == treatments[i],]
  sub <- sub[sample(nrow(sub),n.samp),]
  d2.sub[[i]] <- sub
}
d2.sub <- data.frame(do.call(rbind, d2.sub))
d2.sub$experiment <- 'exp_2'

#Wrap together for final .csv output.----
d1.sub <- d1.sub[,c('experiment','ID','treatment')]
d2.sub <- d2.sub[,c('experiment','ID','treatment')]
output <- rbind(d1.sub, d2.sub)
write.csv(output,output.path)
