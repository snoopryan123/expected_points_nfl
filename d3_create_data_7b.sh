#!/bin/bash
#$ -N create_data_7b.csv
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## this command is to ask for multiple cores for running RStan files: [4]
#$ -pe openmp 1

#$ -o job_output/$JOB_NAME-$JOB_ID.log
## MORE RAM
####$ -l m_mem_free=15G

Rscript --vanilla d3_data_TeamQualityMetrics_epa0.R