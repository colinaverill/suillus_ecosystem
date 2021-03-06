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
#if(host == 'pecan2'){data.dir <- '/fs/data3/caverill/suillus_eco_data/'}
#if(host == 'Colins-MBP-2'){data.dir <- '/Users/colin/Desktop/suillus_eco_data/'}
#If you're not on pecan, geo or scc, you're on your local machine.
if(host != 'pecan2' & grepl('scc',host)==F & grepl('geo',host)==F){data.dir <- '/Users/colinaverill/Documents/data_storage/suillus_eco_data/'}
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
Duke_2017_exp1_root.mass.path <- paste0(dir,'Dukeroot.mass_exp1_2017.csv')
Exp1_2017_bulk.soil_isotope_data.path <- paste0(dir,'Exp1_2017_bulk.soil_isotope_data.csv')

#proteomics.
exp1_proteomics.path <- paste0(dir,'Proteins.Exp1.merged.zeros.csv')

#Duke 2017 experiment 1 processed data.----
Duke_2017_exp1_to_analyze.path <- paste0(dir,'duke_2017_exp1_to_analyze.rds')

#duke 2018 experiment 2 raw data.----
dir <- paste0(raw_data_dir,'exp.2_raw/')
system(paste0('mkdir -p ',dir))
raw_CO2_exp.2_2018.path <- paste0(dir,'gas_13C_workup.csv')
raw_soil_moist_exp.2_2018.path <- paste0(dir,'soil_moist_Duke_Dec2018.csv')
exp2_available_EMSL_tubes.path <- paste0(dir,'available_EMSL_tubes.csv')
duke_2018_exp2_no3_ppmN.path <- paste0(dir,'Duke_Exp2_no3ppm.csv')
duke_2018_exp2_nh4_ppmN.path <- paste0(dir,'Duke_Exp2_nh4ppm.csv')
duke_2018_exp2_biomass_n.trees.path <- paste0(dir,'biomass_n.trees_exp2_2018.csv')
duke_2018_exp2_enzymes.path <- paste0(dir,'all_enzymes_exp2_final.csv')
duke_2018_exp2_assay_masses.path <- paste0(dir,'assay_masses_exp2_dec_2018.csv')
Exp2_2018_bulk.soil_isotope_data.path <- paste0(dir,'Exp2_2018_bulk.soil_isotope_data.csv')

#duke 2020 experiment 3 raw data.----
dir <- paste0(raw_data_dir,'exp.3_raw/')
system(paste0('mkdir -p ',dir))
       raw_CO2_obs1_exp.3_2020.path <- paste0(dir,'13C_co2_data_duke_Jan_2020.csv')
samp_times_CO2_obs1_exp.3_2020.path <- paste0(dir,'Duke_experiment_3_gas_sampling_1_times.csv')
       raw_CO2_obs2_exp.3_2020.path <- paste0(dir,'suillus_exp3_gas.sampling.2_d13C.ppmC.csv')
samp_times_co2_obs2_exp.3_2020.path <- paste0(dir,'suillus_exp3_gas.sampling.2_timepoints.csv')

#duke 2018 experiment 2 processed data.----
dir <- paste0(pecan_gen_dir,'exp.2_processed.dat/')
system(paste0('mkdir -p ',dir))
     duke_2018_co2_workup.path <- paste0(dir,'duke_2018_co2_workup.rds')
duke_2018_nitrogen_workup.path <- paste0(dir,'duke_2018_nitrogen_workup.rds')
     duke_2018_to_analyze.path <- paste0(dir,'duke_2018_to_analyze.rds')

#Duke Exp 2 2018 analysis data.----
exp2_linear_analysis.path <- paste0(dir,'exp2_linear_analysis.rds')
exp2_linear_analysis_aboveground.path <- paste0(dir,'exp2_linear_analysis_aboveground.rds')
     

#duke 2020 experiment 3 processed data.----
dir <- paste0(pecan_gen_dir,'exp.3_processed.dat/')
system(paste0('mkdir -p ',dir))
duke_2020_exp3_obs1_co2_workup.path <- paste0(dir,'duke_2020_exp3_obs1_co2_workup.rds')     
duke_2020_exp3_obs2_co2_workup.path <- paste0(dir,'duke_2020_exp3_obs2_co2_workup.rds')     

#Duke Exp 1 2017 analysis data.----
exp1_linear_analysis.path <- paste0(dir,'exp1_linear_analysis.rds')
exp1_linear_analysis_aboveground.path <- paste0(dir,'exp1_linear_analysis_aboveground.rds')


#Duke 2017 Experiment 1 itag data.----
#Experiment 1 processed itag sequence files.
    duke_exp1.p1_SV_table_ITS.path <- paste0(scc_gen_dir,'duke_exp1.p1_SV_table_ITS.rds')
    duke_exp1.p2_SV_table_ITS.path <- paste0(scc_gen_dir,'duke_exp1.p2_SV_table_ITS.rds')
duke_exp1_SV_table_merged_ITS.path <- paste0(scc_gen_dir,'duke_exp1_SV_table_merged_ITS.rds')
  duke_exp1_tax.fun_table_ITS.path <- paste0(scc_gen_dir,'duke_exp1_tax.fun_table_ITS.rds')
    duke_exp1.p1_SV_table_16S.path <- paste0(scc_gen_dir,'duke_exp1.p1_SV_table_16S.rds')
    duke_exp1.p2_SV_table_16S.path <- paste0(scc_gen_dir,'duke_exp1.p2_SV_table_16S.rds')
duke_exp1_SV_table_merged_16S.path <- paste0(scc_gen_dir,'duke_exp1_SV_table_merged_16S.rds')
  duke_exp1_tax.fun_table_16S.path <- paste0(scc_gen_dir,'duke_exp1_tax.fun_table_16S.rds')
           duke_exp1_16S_rare.path <- paste0(scc_gen_dir,'duke_exp1_16S_rare.rds')
           duke_exp1_ITS_rare.path <- paste0(scc_gen_dir,'duke_exp1_ITS_rare.rds')
  duke_exp1_fungi_r.abundance.path <- paste0(scc_gen_dir,'duke_exp1_fungi_r.abundance.rds')
  
#Experiment 1 big data raw itag sequence paths and mapping files direct from JGI.
 exp1.p1_rawseq.path <- paste0(big_data_dir,'itag/EctintiTagplate1/Raw_Data/')
 exp1.p2_rawseq.path <- paste0(big_data_dir,'itag/EctintiTagplate2/Raw_Data/')
exp1.p1_map_ITS.path <- paste0(big_data_dir,'itag/EctintiTagplate1/itags/ITS2/2-3445838/mapping.tsv')
exp1.p2_map_ITS.path <- paste0(big_data_dir,'itag/EctintiTagplate2/itags/ITS2/2-3503462/mapping.tsv')
exp1.p1_map_16S.path <- paste0(big_data_dir,'itag/EctintiTagplate1/itags/16S-V4-ver2-PNA/2-3485866/mapping.tsv')
exp1.p2_map_16S.path <- paste0(big_data_dir,'itag/EctintiTagplate2/itags/16S-V4-ver2-PNA/2-3485857/mapping.tsv')

