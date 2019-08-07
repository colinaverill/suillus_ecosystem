#working up NO3 NH4 data for Duke 2018 experiment 2.
#lots of zeros in NH4 data.
rm(list=ls())
source('paths.r')

#Load raw data.----
#load ppm-N data.
no3 <- read.csv(duke_2017_exp2_no3_ppmN.path)
nh4 <- read.csv(duke_2017_exp2_nh4_ppmN.path)
no3$sample_type <- as.character(no3$sample_type)
nh4$sample_type <- as.character(nh4$sample_type)
#load data on water content.
water <- read.csv(raw_soil_moist_exp.2_2018.path)
#load soil masses used to extract.

#Order Ni/Nf observations, blank correct.----
#NO3
no3 <- no3[!is.na(no3$sample_type),]
no3.Ni <- no3[no3$sample_type == 'Ci',]
no3.Nf <- no3[no3$sample_type == 'Nf',]
no3.bk <- no3[no3$sample_type == 'blank',]
#Order Ni and Nf data.
no3.Ni <- no3.Ni[order(no3.Ni$sample_ID),]
no3.Nf <- no3.Nf[order(no3.Nf$sample_ID),]
#blank correct NO3 data.
no3.bk <- mean(no3.bk$ppmN)
no3.Ni$ppmN <- no3.Ni$ppmN - no3.bk
no3.Nf$ppmN <- no3.Nf$ppmN - no3.bk
#deal with zeros.
no3.Nf$ppmN <- ifelse(no3.Nf$ppmN < 0, 0, no3.Nf$ppmN)

#NH4
nh4 <- nh4[!is.na(nh4$sample_type),]
nh4.Ni <- nh4[nh4$sample_type == 'Ci',]
nh4.Nf <- nh4[nh4$sample_type == 'Nf',]
nh4.bk <- nh4[nh4$sample_type == 'blank',]
#Order Ni and Nf data.
nh4.Ni <- nh4.Ni[order(nh4.Ni$sample_ID),]
nh4.Nf <- nh4.Nf[order(nh4.Nf$sample_ID),]
#blank correct nh4 data.
nh4.bk <- mean(nh4.bk$ppmN)
nh4.Ni$ppmN <- nh4.Ni$ppmN - nh4.bk
nh4.Nf$ppmN <- nh4.Nf$ppmN - nh4.bk
#deal with zeros.
nh4.Ni$ppmN <- ifelse(nh4.Ni$ppmN < 0, 0, nh4.Ni$ppmN)
nh4.Nf$ppmN <- ifelse(nh4.Nf$ppmN < 0, 0, nh4.Nf$ppmN)


#calculate ug N / g soil.----
#ppm is ug/mL.
#concentration * extraction volume / (g soil extracted * fraction soil dry) = ug N / g soil.
#extraction volume in mL.
extraction_volume <- 40
