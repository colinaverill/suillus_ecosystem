#Source partitioning CO2 fluxes Duke Experiment 3, first CO2 measurement.
rm(list=ls())
source('paths.r')

#set output path.----
output.path <- duke_2020_exp3_obs2_co2_workup.path

#load data.-----
d <- read.csv(raw_CO2_obs2_exp.3_2020.path)
time <- read.csv(samp_times_co2_obs2_exp.3_2020.path)
treatment_key <- read.csv('figures_csvs/Experiment_3_treatment_key_randomized.csv')
treatment_key$X <- NULL
#rename some columns, drop an unnecessary column.
colnames(d) <- c('analysis.ID','sample.ID','d13C','ppm.co2','comments')
d$analysis.ID <- NULL

#break out time into hours and minutes, calculate number of minutes elapsed.----
time$hour.initial <- as.numeric(sub("\\:.*", "", time$t.initial))
time$hour.final   <- as.numeric(sub("\\:.*", "", time$t.final))
time$min.initial  <- as.numeric(sub(".*:"  , "", time$t.initial))
time$min.final    <- as.numeric(sub(".*:"  , "", time$t.final))
time$time.delta <-(time$hour.final*60 + time$min.final)  - (time$hour.initial*60 + time$min.initial) 

#grab initial/final code.----
d$sample.ID <- as.character(d$sample.ID)
d$time.point <- substr(d$sample.ID, nchar(d$sample.ID), nchar(d$sample.ID))
d$sample.ID <- substr(d$sample.ID, 1, nchar(d$sample.ID) - 1)

#break out initial and final observations.----
initial <- d[d$time.point == 'i',]
final <- d[d$time.point == 'f',]
initial$time.point <- NULL
final$time.point <- NULL
colnames(initial) <- c('sample.ID','d13C.initial','ppm.initial','comments.initial')
colnames(final) <- c('sample.ID','d13C.final','ppm.final','comments.final')
d <- merge(initial, final)
d <- d[order(as.numeric(d$sample.ID)),]

#In this case, everything with a comment is a good reason to exclude an observation. Drop these.----
d$comments.final   <- as.character(d$comments.final  )
d$comments.initial <- as.character(d$comments.initial)
d <- d[d$comments.final == '' & d$comments.initial == '',]
d$comments.final <- NULL
d$comments.initial <- NULL

#merge time interval into larger spreadsheet.----
d <- merge(d,time[,c('ID','time.delta')], by.x = 'sample.ID',by.y='ID',all.x=T)

#Calculate accumulated CO2 quantity and its 13C signature.----
##NOTE: update co2 emittied to be a mass by multiplying by chamber volume in mL eventually!!!
d$ co2.emitted <- d$ppm.final - d$ppm.initial
d$d13C.emitted <- (d$d13C.final*d$ppm.final - d$d13C.initial*d$ppm.initial) / (d$ppm.final - d$ppm.initial)

#source partition emitted co2 to get plant derived and soil organic matter derived respiration rates.----
#assign d13C of soil and plants.
d13C.soil  <- -11
d13C.plant <- -26

#calculate plant fraction of co2 flux.
d$plant.fraction <- (d$d13C.emitted - d13C.soil) / (d13C.plant - d13C.soil)

#calculate plant and soil fluxes.
d$resp.plant <- (d$co2.emitted *      d$plant.fraction ) / d$time.delta
d$resp.soil  <- (d$co2.emitted * (1 - d$plant.fraction)) / d$time.delta
d$resp.total <- d$co2.emitted / d$time.delta

#cleaup output and save.----
output <- d[,c('sample.ID','resp.plant','resp.soil','resp.total','d13C.emitted')]
output <- output[order(as.numeric(output$sample.ID)),]
output <- merge(output, treatment_key, by.x = 'sample.ID', by.y = 'ID', all.x = T)
saveRDS(output, output.path) #note: update with your own output path.
