#assign taxonomy and function to Duke Suillus Experiment 1 sequences.
#Taxonomy assignment takes ~41 minutes with 16 cores in parallel.
#clear environment, load packages, functions and paths.----
rm(list=ls())
library(doParallel)
library(data.table)
source('paths.r')
source('functions/tic_toc.r')
source('functions/fg_assign.r')

#load SV table, set output path.----
#this needs a lot of memory.
p1 <- readRDS(duke_exp1.p1_SV_table_16S.path)
p2 <- readRDS(duke_exp1.p2_SV_table_16S.path)
output.path <- duke_exp1_tax.fun_table_16S.path
merged_SV.table_output.path <- duke_exp1_SV_table_merged_16S.path
data.dir <- scc_gen_dir #where to download the greengenes database to.

#merge SV tables, save composite SV table.----
d <- dada2::mergeSequenceTables(table1 = p1, table2 = p2)

#1. download GREENGENES training set.----
cat('downloading taxonomic reference database...\n')
gg_url <- 'https://zenodo.org/record/158955/files/gg_13_8_train_set_97.fa.gz?download=1'
gg_path.zip <- paste0(data.dir,'gg.fa.gz')
gg_path     <- paste0(data.dir,'gg.fa')
cmd <- paste0('curl ',gg_url,' > ',gg_path.zip)
system(cmd)
cmd <- paste0('gunzip ',gg_path.zip)
system(cmd)
cat('Taxonomic reference database download complete.\n')

#2. assign taxonomy in parallel using colin's parallel hack. Native dada2 multithread isn't working.----
#n <- detectCores()
n <- 16 #if you only request 16 and get assigned a 28 core machine this will over-allocate and process reaper shows up.
registerDoParallel(cores=n)

#set breakpoints for subsetting taxonomy list.
to_assign <- colnames(d)
brk <- floor(length(to_assign) / n) #floor rounds down. Important here.

#use a foreach loop to do this in parallel on subsets.
tic()
cat('assigning taxonomy with the RDP classifier and unite training set...\n')
output.list <-
  foreach(i = 1:n) %dopar% {
    #tell loop where i of n taxonomy subset starts and ends.
    start <- (brk*i - brk) + 1
    end <- brk*i
    #if you on the last subset go to end.
    if(i == n){end = length(to_assign)}
    
    #assign taxa
    tax.out <- dada2::assignTaxonomy(to_assign[start:end], gg_path)
    
    #return output to list
    return(tax.out)
  }
cat('Taxonomy assignment complete! yeahhhh.\n')
toc()

#merge together output of parallel assignment.
tax <- data.frame(do.call('rbind',output.list))

#remove leading characters, push to lower case.----
#remove leading "X__".
for(i in 1:ncol(tax)){
  tax[,i] <- substring(tax[,i],4)
}
colnames(tax) <- tolower(colnames(tax))

#remove taxa that do not assign to bacteria.----
tax <- tax[!is.na(tax$kingdom),]
tax <- tax[tax$kingdom == 'Bacteria',]


#Subset otu table to remove non-fungi, make sure order matches.----
d <- d[,colnames(d) %in% rownames(tax)]

#4. save output.----
saveRDS(tax, output.path)
saveRDS(  d, merged_SV.table_output.path)
cat('Taxonomy output saved.\n')

#5. cleanup.----
system(paste0('rm -f ',gg_path))

#end script.
