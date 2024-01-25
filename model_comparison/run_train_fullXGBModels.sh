#!/bin/bash
#$ -N train_fullXGBModels
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## ARRAY JOB
#$ -t 1-3
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
#$ -l m_mem_free=10G

Rscript --vanilla train_fullXGBModels.R ${SGE_TASK_ID}


