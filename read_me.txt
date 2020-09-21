Suillus ecosystem ecology data analysis. Project led by Colin Averill.

This repo contains code to:

1. Analyze Suillus ecosystem biogeochemical data.
2. process and analyze itag sequences (JGI).
3. process and analyze meta-transcriptome sequences (JGI).
4. process and analyze metabolomic data (EMSL).

Currently, all itag data are processed using the scripts in data_construction/itag_data. These scripts are intended to be run on the BU scc computing cluster, and the folder "qsub/" within the itag_data script folder contains scripts to submit the processing jobs.

Raw data from sequencing output and other files is stored in "suillus_eco_data/big_data/". These files are only on the BU scc computer, not on colin's local computer.

Small raw data files are stored in "suillus_eco_data/small_data/raw_data/".

Once raw data are processed:
All output of   scc analysis is saved to "suillus_eco_data/small_data/scc_gen/"
All output of local analysis is saved to "suillus_eco_data/small_data/pecan_gen/"

"pecan_gen" refers to the pecan2 machine, run by the Dietze lab, where this project used to be run from. Keeping this name since paths have already been written, but it can be thought of as "local_gen" - where locally generated intermediate data files live.

Transfer scripts are setup such that local->scc never overwrites scc output, and vice versa. You can setup more complicated data syncing procedures, but this works, so long as you keep the file dependencies right. Because the cluster is only used to porcess itag data, this keeps things straightforward.

Currently, local is Colin's laptop, where all data and code is backed up to Google Drive.