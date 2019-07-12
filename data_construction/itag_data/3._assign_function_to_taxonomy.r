#Appending functional groups to taxonomy table. Also doing some mild reformatting.
rm(list=ls())
source('paths.r')
source('functions/fg_assign.r')
library(data.table)

#set output path.----
tax_output.path <- ted_2014_tax_fg.path

#load taxonomy data.----
tax <- readRDS(duke_exp1_tax_table.path)
otu <- readRDS(duke_exp1_SV_table_merged.path)

#remove leading characters, push to lower case.----
#remove leading "X__".
for(i in 1:ncol(tax)){
  tax[,i] <- substring(tax[,i],4)
}
colnames(tax) <- tolower(colnames(tax))

#assign function based on FUNGuid.----
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
otu <- otu[,colnames(otu) %in% rownames(tax)]

#save output.----
saveRDS(tax, tax_output.path)
