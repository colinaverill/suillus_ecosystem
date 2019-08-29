#count suillus and NMDS samples.
#only 90 of 118 rows in mapping file found in sample names. None of map2 filenames in rownames of d.
#Need to re-download all itag files, subset based on maps to bacteria or fungi, and rerun pipeline.
#This will also require some modifying of filenames to reflect ITS vs. 16S.
rm(list=ls())
source('paths.r')

d <- readRDS(duke_exp1_SV_table_merged_ITS.path)
tax <- readRDS(duke_exp1_tax.fun_table_ITS.path)
map1 <- read.table(exp1.p1_map_ITS.path)
map2 <- read.table(exp1.p2_map_ITS.path)

#Touch up the map files.----
map  <- rbind(map1, map2)
colnames(map) <- c('sampleID','filename')
map$filename <- gsub('.fastq.gz','',map$filename)
map$sampleID <- gsub('S','',map$sampleID)
map$sampleID <- gsub('_iTag','',map$sampleID)
map[] <- lapply(map, as.character)
map <- map[order(match(map$filename, rownames(d))),]

#All 88 of mapping file filenames in rownames of d.
nrow(map[map$filename %in% rownames(d),])

#rarefy OTU table to lowest sequencing depth.
set.seed(420)
r.depth <- min(rowSums(d))
d.rare  <- vegan::rrarefy(d, r.depth)
d.rel   <- d.rare/rowSums(d.rare)

#Get suillus relative abundances.
suillus.seq <- rownames(tax[grep('Suillus'        , tax$genus),])
    ecm.seq <- rownames(tax[grep('Ectomycorrhizal',tax$fg)    ,])
suillus     <- rowSums(d.rel[,colnames(d.rel) %in% suillus.seq])
ecm         <- rowSums(d.rel[,colnames(d.rel) %in%     ecm.seq])
plot(ecm ~ suillus)
check <- data.frame(ecm, suillus)
plot(ecm ~ suillus, data = check[check$suillus > 0,])
