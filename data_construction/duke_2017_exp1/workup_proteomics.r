#annotate proteins experiment 1.
rm(list=ls())
source('paths.r')
library(stringr)

#load data.----
d <- read.csv(exp1_proteomics.path)


#strip out KEGG (KO) values from the protein name field.----
ko.out <- list()
for(i in 1:length(d$Protein)){
  ko.lab <- unlist(str_extract_all(d$Protein[i], boundary("word")))
  ko.lab <- str_subset(ko.lab, "KO")
  ko.lab <- gsub('KO:','',ko.lab)
  if(length(ko.lab) == 0){ko.lab <- NA}
  ko.out[[i]] <- ko.lab
}
#make additional rows, one for each KO assignment.
row.out <- list()
for(i in 1:length(ko.out)){
  data.lab <- as.matrix(d[i,])
  mid.out <- list()
  for(k in 1:length(ko.out[[i]])){
    mid.out[[k]] <- c(data.lab, ko.out[[i]][k])
  }
  mid.out <- do.call(rbind, mid.out)
  row.out[[i]] <- mid.out
}
row.out <- data.frame(do.call(rbind, row.out))
colnames(row.out) <- c(colnames(d),'kegg_ortholog')
ko.out <- row.out

#Make new KO columns (old approach)
#ko.out <- lapply(ko.out, "length<-", max(lengths(ko.out)))
#ko.out <- lapply(ko.out, noquote)
#ko.out <- data.frame(do.call(rbind, ko.out))
#lab <- paste0('ko_',1:ncol(ko.out))
#colnames(ko.out) <- lab
#d <- cbind(d, ko.out)

#making additional rows - 1 for every unique KO assignment.
#ko.out <- list()
#for(i in 1:length(d$Protein)){
#  ko.lab <- unlist(str_extract_all(d$Protein[i], boundary("word")))
#  ko.lab <- str_subset(ko.lab, "KO")
#  ko.lab <- gsub('KO:','',ko.lab)
#  if(length(ko.lab) == 0){ko.lab <- NA}
#  ko.out[[i]] <- ko.lab
#}


#grab all cog assignments in protein name field.----
cog.out <- list()
for(i in 1:length(d$Protein)){
  cog.lab <- unlist(regmatches(d$Protein[i], gregexpr("COG\\d+", d$Protein[i])))
  if(length(cog.lab) == 0){cog.lab <- NA}
  cog.out[[i]] <- cog.lab
}
#make additional rows, one for each cog assignment.
row.out <- list()
for(i in 1:length(cog.out)){
  data.lab <- as.matrix(d[i,])
  mid.out <- list()
  for(k in 1:length(cog.out[[i]])){
    mid.out[[k]] <- c(data.lab, cog.out[[i]][k])
  }
  mid.out <- do.call(rbind, mid.out)
  row.out[[i]] <- mid.out
}
row.out <- data.frame(do.call(rbind, row.out))
colnames(row.out) <- c(colnames(d),'cog')
cog.out <- row.out

#old approach - adding columns.
#cog.out <- lapply(cog.out, "length<-", max(lengths(cog.out)))
#cog.out <- lapply(cog.out, noquote)
#cog.out <- data.frame(do.call(rbind, cog.out))
#lab <- paste0('cog_',1:ncol(cog.out))
#colnames(cog.out) <- lab
#drop into dataframe.
#d <- cbind(d, cog.out)

#grab all pgam assignemnts in protein name field.----
pfam.out <- list()
for(i in 1:length(d$Protein)){
  pfam.lab <- unlist(regmatches(d$Protein[i], gregexpr("pfam\\d+", d$Protein[i])))
  pfam.lab <- gsub('pfam','',pfam.lab)
  if(length(pfam.lab) == 0){pfam.lab <- NA}
  pfam.out[[i]] <- pfam.lab
}
#make additional rows, one for each cog assignment.
row.out <- list()
for(i in 1:length(pfam.out)){
  data.lab <- as.matrix(d[i,])
  mid.out <- list()
  for(k in 1:length(pfam.out[[i]])){
    mid.out[[k]] <- c(data.lab, pfam.out[[i]][k])
  }
  mid.out <- do.call(rbind, mid.out)
  row.out[[i]] <- mid.out
}
row.out <- data.frame(do.call(rbind, row.out))
colnames(row.out) <- c(colnames(d),'pfam')
pfam.out <- row.out

#old approach - adding columns.
#pfam.out <- lapply(pfam.out, "length<-", max(lengths(pfam.out)))
#pfam.out <- lapply(pfam.out, noquote)
#pfam.out <- data.frame(do.call(rbind, pfam.out))
#lab <- paste0('pfam_',1:ncol(pfam.out))
#colnames(pfam.out) <- lab
#drop into dataframe
#d <- cbind(d, pfam.out)

#new data output for JMB.----
#write.csv(d, 'proteome_data_for_JMB.csv')
write.csv(  ko.out,'kegg_proteome_data_for_JMB.csv')
write.csv(pfam.out,'pfam_proteome_data_for_JMB.csv')
write.csv(pfam.out, 'cog_proteome_data_for_JMB.csv')

