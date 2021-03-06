#Constructing SV table from Duke 2017 experiment 1- no POM addition. Plate 2 - 16S.
#quality filtering, sample inference, chimera filtering using the dada2 pipeline.
#assign taxonomy using RDP via dada2.
#require qsub script to load the following modules: R
#clear environment, load packages and functions.----
rm(list=ls())
source('paths.r')
source('functions/tic_toc.r')
source('functions/permute_read.r')
library(data.table)
library(dada2)

#set input/output paths, load map, specify primers.----
#seq.path <- '/projectnb/talbot-lab-data/caverill/suillus_eco_data/big_data/itag/EctintiTagplate2/Raw_Data/'
seq.path <- exp1.p2_rawseq.path
map <- read.table(exp1.p2_map_16S.path)
colnames(map) <- c('sample_ID','filename')
map[] <- lapply(map, as.character)
map.files <- gsub('.gz','',map$filename)

#output file path.
output_filepath1 <-  paste0(seq.path,'SV_table_16S.rds')
output_filepath2 <- duke_exp1.p2_SV_table_16S.path
output_track     <-  paste0(seq.path,'track_16S.rds')

#bacterial primers from JGI:
#Primer one at the front of reads in file, confirmed in the orientation below.
primer.1 <- 'GTGYCAGCMGCCGCGGTAA'  #515F-Y this is the forward primer in correct orientation on "front" of read. 4 unique possible primers.
#This is the reverse primer, which must be permuted (using custom) for flex positions and then reverse complemented.
primer.2 <- 'CCGYCAATTYMTTTRAGTTT' #926R this is reverse primer. 16 unique possible primers.
#permute primers 1+2.
primer.1 <- permute_read(primer.1)
primer.2 <- permute_read(primer.2)
#reverse complement primer 2.
primer.2 <- rc(primer.2)
#collapse primers to comma separated string, compatible with bbduk.
primer.1 <- paste(primer.1, collapse = ',')
primer.2 <- paste(primer.2, collapse = ',')

#old fungal primers
#reverse primers (there is a flex position)
#rev.primers <- 'TCCTGCGCTTATTGATATGC,TCCTCCGCTTATTGATATGC'
#foward primers: there are 6. These are their reverse complements.
#rc.fwd.primers <- ('CAGCGTTCTTCATCGATGACGAGTCTAG,CTGCGTTCTTCATCGTTGACGAGTCTAG,CTGCGTTCTTCATCGGTGACGAGTCTAG,CTACGTTCTTCATCGATGACGAGTCTAG,CCACGTTCTTCATCGATGACGAGTCTAG,CAGCGTTCTTCATCGATGACGAGTCTAG')

#unzip files if they are zipped.----
fastq.files <- list.files(seq.path)
if(sum(grepl('.gz',fastq.files) > 0)){
  cat('Unzipping .gz files...\n');tic()
  cmd <- paste0('gunzip ',seq.path,'*.gz')
  system(cmd)
  cat('Files unzipped. ');toc()
}

#get fastq file names, only include files that end in .fastq.----
fastq.files <- list.files(seq.path)
fastq.files <- fastq.files[grep('.fastq',fastq.files)]

#Subset to files in particular mapping file.----
fastq.files <- fastq.files[fastq.files %in% map.files]

#subset for testing
testing = F
if(testing == T){
  fastq.files <- fastq.files[1:3]
}

#you need to get rid of q.trim and filtered directories if they are left over from a previous dada2 run.----
system(paste0('rm -rf ',seq.path,'q.trim'))
system(paste0('rm -rf ',seq.path,'filtered'))

#Trim primers.----
#from here we need to trim out primers and leading adapter/barcode sequence which is variable length.
#DOE has a great tool to find a primer and trim anything preceding in, called bbuk.sh in the bbmap package.
#Find theat here: https://jgi.doe.gov/data-and-tools/bbtools/bb-tools-user-guide/bbduk-guide/
#I copied this indivudal script into the NEFI_tools directory.
for(i in 1:length(fastq.files)){
  #"left" trim from 5' end the forward primer, primer.1
  sample.name <- fastq.files[i]
  sample.name <- substr(sample.name,1,nchar(sample.name)-6)
  output.dir1 <- 'q.trim.L/'
  bbduk.path <- 'functions/bbmap/bbduk.sh'
  sample.path <- paste0(seq.path,sample.name,'.fastq')
  output.path <- paste0(seq.path,output.dir1,sample.name,'.fastq')
  cmd <- paste0(bbduk.path,
                ' literal=',primer.1,
                ' ktrim=l k=10 ',
                'in=',sample.path,
                ' out=',output.path,' ordered=t')
  system(cmd)
  #"right" trim from 3' end reverse primer, primer.2
  output.dir2 <- 'q.trim.R/'
  sample.path <- paste0(seq.path,output.dir1,sample.name,'.fastq')
  output.path <- paste0(seq.path,output.dir2,sample.name,'.fastq')
  cmd <- paste0(bbduk.path,
                ' literal=',primer.2,
                ' ktrim=r k=10 ',
                'in=',sample.path,
                ' out=',output.path,' ordered=t')
  system(cmd)
}

#clean up and rename some things.
cmd <- paste0('rm -rf ',seq.path,'q.trim.L')
system(cmd)
cmd <- paste0('mv ',seq.path,'q.trim.R ',seq.path,'q.trim')
system(cmd)

#perform quality filtering in dada2.----
filtered_output.dir <- paste0(seq.path, 'filtered/')
filtered_input.dir  <- paste0(seq.path,'q.trim/')
filtered_input  <- paste0(filtered_input.dir,list.files(filtered_input.dir))
filtered_output <- paste0(filtered_output.dir, basename(filtered_input))
tic()
cat('Begin quality filtering via dada2...\n')
out <- filterAndTrim(filtered_input, filtered_output, maxN = 0, maxEE = 2,  #make maxEE = c(2,2) if processing multiple files at once (like fwd/rev reads separately).
                     truncQ = 2, minLen = 50, rm.phix = TRUE, compress = TRUE, multithread = T)
cat('quality filtering complete.')
toc()

#Some reads don't make it through. Update paths and sample.names.
filtered <- paste0(filtered_output.dir, list.files(filtered_output.dir))
sample.names <- sapply(strsplit(basename(filtered), ".fastq"), `[`, 1)

#Learn the error rates.----
#This is a machine learning algorithm to dial in the error rate model of your reads.
#Colin isn't really sure what an error rate model is. ¯\_(ツ)_/¯
tic() #start timer loop.
cat('Learning error rates...\n')
err <- learnErrors(filtered, multithread = TRUE)
cat('Error models fitted!')
toc() #end timer loop.

#Dereplicate the reads.----
tic()
cat('Dereplicating sequences...\n')
derep <- derepFastq(filtered, verbose = T)
cat('Sequences dereplicated.')
toc()
#add sample names.
names(derep) <- sample.names

#Sample Inference. Send it through dada2!-----
tic()
cat('Performing sample inference with the dada2 algorithm...\n')
dada_out <- dada(derep, err = err, multithread = TRUE)
cat('Sample inference complete.')
toc()
seqtab <- makeSequenceTable(dada_out)

#Remove Chimeras.----
tic()
cat('Removing chimeric sequences...\n')
seqtab.nochim <- removeBimeraDenovo(seqtab, method="consensus", multithread=TRUE, verbose=TRUE)
#check proportion of reads that pass chimeras filter.
#lots of ESVs can be chimeras, but they usually represent less than 5-10% of total reads.
chim.retain <- sum(seqtab.nochim / sum(seqtab))
cat('Chimeric sequences removed.',chim.retain*100,'% of sequences retained.')
toc()

#Track reads through pipeline.----
#This is a useful check. If you are losing all your reads at some point this will give you an idea of where.
#this dataframe saves to dada2_output within the sequence folder.
getN <- function(x) sum(getUniques(x))
out_sub <- out[rownames(out) %in% paste0(sample.names,'.fastq'),]
track <- cbind(out_sub, sapply(dada_out, getN), rowSums(seqtab.nochim))
percent_survive <- (round(track[,4]/track[,1],2)*100)
track <- cbind(track,percent_survive)
colnames(track) <- c("input", "filtered", "denoised","nonchim","percent_made_it")
rownames(track) <- sample.names


#drop singletons, reads less than 100 bp.----
seqtab.nochim <- seqtab.nochim[,!colSums(seqtab.nochim) == 1]
seqtab.nochim <- seqtab.nochim[,nchar(colnames(seqtab.nochim)) > 99]

#save output.----
cat('Saving output...\n')
saveRDS(seqtab.nochim, output_filepath1)
saveRDS(seqtab.nochim, output_filepath2)
saveRDS(track        , output_track    )
cat('Output saved, script complete.\n')
