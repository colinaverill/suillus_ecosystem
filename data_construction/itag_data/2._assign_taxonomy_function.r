#assign taxonomy and function to tedersoo sequences.
#clear environment, load packages, functions and paths.----
rm(list=ls())
library(doParallel)
library(data.table)
source('paths.r')
source('functions/tic_toc.r')
source('functions/fg_assign.r')

#load SV table, set output path.----
#this needs a lot of memory.
p1 <- readRDS(duke_exp1.p1_SV_table.path)
p2 <- readRDS(duke_exp1.p2_SV_table.path)
output.path <- duke_exp1_tax.fun_table.path
merged_SV.table_output.path <- duke_exp1_SV_table_merged.path
data.dir <- scc_gen_dir #where to download the unite database to.

#merge SV tables, save composite SV table.----
d <- dada2::mergeSequenceTables(table1 = p1, table2 = p2)

#1. download unite training set.----
#cat('downloading UNITE database...\n')
unite_url <- 'https://files.plutof.ut.ee/doi/B2/07/B2079372C79891519EF815160D4467BBF4AF1288A23E135E666BABF2C5779767.zip'
unite_path.zip <- paste0(data.dir,'unite.fa.zip')
unite_path     <- paste0(data.dir,'sh_general_release_dynamic_01.12.2017.fasta')
cmd <- paste0('curl ',unite_url,' > ',unite_path.zip)
system(cmd)
cmd <- paste0('unzip ',unite_path.zip,' -d ',data.dir)
system(cmd)
cat('UNITE download complete.\n')

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
    tax.out <- dada2::assignTaxonomy(to_assign[start:end], unite_path)
    
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

#remove taxa that do not assign to fungi.----
tax <- tax[!is.na(tax$kingdom),]
tax <- tax[tax$kingdom == 'Fungi',]


#3. assign function based on FUNGuid.----
fg <- fg_assign(tax)

#collapse functional assignments into a single vector for ECM, AM, SAP and pathogen.-----
fg <- data.table(fg)
#Assign species based on functions you want to trump other functions. 
#For instance, if something assigns to both SAP and ECM this will have ECM override SAP.
fg[grep('Arbuscular'     , guild), fg := 'Arbuscular'     ]
fg[grep('Plant Pathogen' , guild), fg := 'Plant_Pathogen' ]
fg[grep('Animal Pathogen', guild), fg := 'Animal_Pathogen']
fg[grep('Saprotroph'     , guild), fg := 'Saprotroph'     ]
fg[grep('Wood Saprotroph', guild), fg := 'Wood_Saprotroph']
fg[grep('Ectomycorrhizal', guild), fg := 'Ectomycorrhizal']

#append functional groups to taxonomy table.----
tax$fg <- fg$fg

#Subset otu table to remove non-fungi, make sure order matches.----
d <- d[,colnames(d) %in% rownames(tax)]

#4. save output.----
saveRDS(tax, output.path)
saveRDS(  d, merged_SV.table_output.path)
cat('Taxonomy output saved.\n')

#5. cleanup.----
system(paste0('rm -f ',unite_path.zip))
system(paste0('rm -f ',unite_path))

#end script.
