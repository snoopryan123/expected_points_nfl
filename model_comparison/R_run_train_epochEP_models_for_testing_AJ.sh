#!/bin/bash
#$ -N run_train_epochEP_models_for_testing_AJ
#$ -j y
####$ -m e -M ryguy123@sas.upenn.edu 

## ARRAY JOB
#$ -t 1-101
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
#$ -l m_mem_free=10G

Rscript --vanilla D_eval_EP_TrainModels.R TRUE FALSE FALSE ${SGE_TASK_ID} 101
