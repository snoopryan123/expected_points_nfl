#!/bin/bash
#$ -N run_eval_driveEP_AJ
#$ -j y
####$ -m e -M ryguy123@sas.upenn.edu 

## ARRAY JOB
#$ -t 1-101
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
#$ -l m_mem_free=15G

Rscript --vanilla eval_EP_EvalLosses_AJ.R FALSE FALSE ${SGE_TASK_ID} 101
