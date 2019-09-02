#rarefy 16S and ITS SV tables.
#quantify suillus, ECM and SAP based on sequence abundance.
rm(list=ls())
source('paths.r')

#set output path.----
    output_16S_rare.path <- duke_exp1_16S_rare.path
    output_ITS_rare.path <- duke_exp1_ITS_rare.path
output_fungal_group.path <- duke_exp1_fungi_r.abundance.path

#load data.-----
d.ITS <- readRDS(duke_exp1_SV_table_merged_ITS.path)
d.16S <- readRDS(duke_exp1_SV_table_merged_16S.path)
tax <- readRDS(duke_exp1_tax.fun_table_ITS.path)

#rarefy OTU table to lowest sequencing depth.----
set.seed(420)
r.depth.ITS <- min(rowSums(d.ITS))
r.depth.16S <- min(rowSums(d.16S)[rowSums(d.16S) > 500])
d.rare.ITS <- vegan::rrarefy(d.ITS, r.depth.ITS)
d.rare.16S <- vegan::rrarefy(d.16S, r.depth.16S)
d.rel.ITS  <- d.rare.ITS/rowSums(d.rare.ITS)
d.rel.16S  <- d.rare.16S/rowSums(d.rare.16S)

#Get fungal group relative abundances.----
suillus.seq <- rownames(tax[grep('Suillus'        , tax$genus),])
    ecm.seq <- rownames(tax[grep('Ectomycorrhizal',tax$fg)    ,])
    sap.seq <- rownames(tax[grep('Saprotrop',tax$fg)    ,])
suillus.f   <- rowSums(d.rel.ITS[,colnames(d.rel.ITS) %in% suillus.seq])
ecm.f       <- rowSums(d.rel.ITS[,colnames(d.rel.ITS) %in%     ecm.seq])
sap.f       <- rowSums(d.rel.ITS[,colnames(d.rel.ITS) %in%     sap.seq])
group.bin <- data.frame(names(suillus.f),suillus.f,ecm.f,sap.f)
colnames(group.bin)[1] <- 'ID'
group.bin$ID <- as.integer(group.bin$ID)

#Save output.----
saveRDS(d.rare.ITS, output_ITS_rare.path)
saveRDS(d.rare.16S, output_16S_rare.path)
saveRDS(group.bin , output_fungal_group.path)
