#!/bin/bash
#$ -N run_eval_driveEP_models
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

#$ -o job_output/$JOB_NAME-$JOB_ID.log
## MORE RAM
#$ -l m_mem_free=30G

Rscript --vanilla eval_EP_models.R FALSE FALSE