#merge 2017 experiment 1 'master' with root data. Make sure everything links to SV tables.
rm(list=ls())
source('paths.r')

#set output path.----
output.path <- Duke_2017_exp1_to_analyze.path

#load data.----
d <- read.csv(Duke_2017_EMxN_master.path)
root <- read.csv(Duke_2017_exp1_root.mass.path)
fungi <- readRDS(duke_exp1_fungi_r.abundance.path)


#merge root data in.----
colnames(root) <- c('ID','below_mass')
d <- merge(d, root, all.x = T)

#merge fungal relative abundances in.----
d <- merge(d, fungi, all.x = T)

#Save output.----
saveRDS(d, output.path)
