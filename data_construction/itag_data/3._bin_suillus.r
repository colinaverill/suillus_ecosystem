#count suillus and NMDS samples.
#only 90 of 118 rows in mapping file found in sample names. None of map2 filenames in rownames of d.
#Need to re-download all itag files, subset based on maps to bacteria or fungi, and rerun pipeline.
#This will also require some modifying of filenames to reflect ITS vs. 16S.
rm(list=ls())
source('paths.r')

d <- readRDS(duke_exp1_SV_table_merged.path)
tax <- readRDS(duke_exp1_tax.fun_table.path)
map1 <- read.table(duke_exp1_plate_1_map.path)
map2 <- read.table(duke_exp1_plate_2_map.path)

#Touch up the map files.----
map  <- rbind(map1, map2)
colnames(map) <- c('sampleID','type','filename')
map$type <- NULL
map$filename <- gsub('.fastq.gz','',map$filename)
map$sampleID <- gsub('S','',map$sampleID)
map[] <- lapply(map, as.character)
map <- map[order(match(map$sampleID, rownames(d))),]

#Only 90 of mapping file filenames in rownames of d.
nrow(map[map$filename %in% rownames(d),])

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




