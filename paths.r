#all paths for Suillus Ecosystem analysis project.
#The path to the data directory will depend if you are on pecan2 or the scc.
#To deal with this Colin has condiitonally setup the path to data based on the hostname of the computer.
#Forst instance, if the hostname is 'pecan2' colin tells the computer that all the data are in /fs/data3/caverill/suillus_eco_data/
#I often save data objects as ".rds" files, which is just an R data storage type.
#.rds files can be loaded with "readRDS()", and saved with "saveRDS(R_object, path/to/file.rds)".

#### High level directory structure.----
host <- system('hostname', intern=T)
#data directory conditional to which computer you are working on - default geo/scc.
data.dir <- '/projectnb/talbot-lab-data/caverill/suillus_eco_data/'
#conditional data directory assignment.
if(host == 'pecan2'){data.dir <- '/fs/data3/caverill/suillus_eco_data/'}
#make directory if it doesn't exist
cmd <- paste0('mkdir -p ',data.dir)
system(cmd)

#two main sub directories. big_data and small_data.
#big_data only lives in the scc.
#scc_gen and pecan_gen live on both, but updating the directory from one computer to the other only goes one way.
#scc_gen only goes scc->pecan. pecan_gen only goes pecan->scc.
  big_data_dir <- paste0(data.dir,  'big_data/')
small_data_dir <- paste0(data.dir,'small_data/')
 pecan_gen_dir <- paste0(small_data_dir,'pecan_gen/')
   scc_gen_dir <- paste0(small_data_dir,'scc_gen/')
  raw_data_dir <- paste0(small_data_dir,'raw_data/')
#make directories if they don't exist.
system(paste0('mkdir -p ',  big_data_dir))
system(paste0('mkdir -p ',small_data_dir))
system(paste0('mkdir -p ',pecan_gen_dir))
system(paste0('mkdir -p ',  scc_gen_dir))

#duke 2017 experiment 1 raw data.----
dir <- paste0(raw_data_dir,'exp.1_raw/')
system(paste0('mkdir -p ',dir))
Duke_2017_EMxN_master.path <- paste0(dir,'Duke_2017_EMxN_master.csv')

#duke 2018 experiment 2 raw data.----
dir <- paste0(raw_data_dir,'exp.2_raw/')
system(paste0('mkdir -p ',dir))
raw_CO2_exp.2_2018.path <- paste0(dir,'gas_13C_workup.csv')
raw_soil_moist_exp.2_2018.path <- paste0(dir,'soil_moist_Duke_Dec2018.csv')
exp2_available_EMSL_tubes.path <- paste0(dir,'available_EMSL_tubes.csv')

#duke 2018 experiment 2 processed data.----
dir <- paste0(pecan_gen_dir,'exp.2_processed.dat/')
system(paste0('mkdir -p ',dir))
duke_2018_co2_workup.path <- paste0(dir,'duke_2018_co2_workup.rds')

#Duke 2017 itag data.----
    duke_exp1.p1_SV_table.path <- paste0(scc_gen_dir,'duke_exp1.p1_SV_table.rds')
    duke_exp1.p2_SV_table.path <- paste0(scc_gen_dir,'duke_exp1.p2_SV_table.rds')
duke_exp1_SV_table_merged.path <- paste0(scc_gen_dir,'duke_exp1_SV_table_merged.rds')
#      duke_exp1_tax_table.path <- paste0(scc_gen_dir,'duke_exp1_tax_table.rds')
  duke_exp1_tax.fun_table.path <- paste0(scc_gen_dir,'duke_exp1_tax.fun_table.rds')

