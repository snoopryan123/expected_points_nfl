#!/bin/bash
#$ -N run_eval_driveEP_EvalLosses
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## MORE RAM
#$ -l m_mem_free=15G

Rscript --vanilla eval_EP_TrainModels.R FALSE 100000 101
