#!/bin/bash
#$ -N team_quality_knockoffs
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## this command is to ask for multiple cores for running RStan files: [4]
#$ -pe openmp 1

#$ -o job_output/$JOB_NAME-$JOB_ID.log
## MORE RAM
#$ -l m_mem_free=100G

Rscript --vanilla team_quality_knockoffs.R