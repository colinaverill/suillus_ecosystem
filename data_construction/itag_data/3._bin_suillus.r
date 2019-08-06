#count suillus and NMDS samples.
rm(list=ls())
source('paths.r')

d <- readRDS(duke_exp1_SV_table_merged.path)
tax <- readRDS(duke_exp1_tax.fun_table.path)


#Require depth of at least 1000 reads to be included. Drops 26 samples.
#Drops a handful of OTUs as well.
d <- d[rowSums(d) > 1000,]
d <- d[,colSums(d) > 1]
tax <- tax[rownames(tax) %in% colnames(d),]

#rarefy OTU table.
set.seed(420)
r.depth <- min(rowSums(d))
d.rare  <- vegan::rrarefy(d, r.depth)
d.rel   <- d.rare/rowSums(d.rare)

#Get suillus relative abundances.
suillus.seq <- rownames(tax[grep('Suillus', tax$genus),])
suillus     <- rowSums(d.rel[,colnames(d.rel) %in% suillus.seq])




