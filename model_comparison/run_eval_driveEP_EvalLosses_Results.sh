#!/bin/bash
#$ -N run_eval_driveEP_EvalLosses_Results
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

#$ -o job_output/$JOB_NAME-$JOB_ID.log
## MORE RAM
#$ -l m_mem_free=15G

Rscript --vanilla eval_EP_EvalLosses.R FALSE 100000 101
