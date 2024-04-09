#!/bin/bash
#$ -N run_eval_epochEP_models
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

#$ -o job_output/$JOB_NAME-$JOB_ID.log
## MORE RAM
#$ -l m_mem_free=15G

Rscript --vanilla eval_EP_models.R TRUE FALSE