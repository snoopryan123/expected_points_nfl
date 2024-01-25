#!/bin/bash
#$ -N XGB_ParamTuning
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## ARRAY JOB
#$ -t 1-3
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
#$ -l m_mem_free=10G

Rscript --vanilla param_tuning.R ${SGE_TASK_ID} FALSE