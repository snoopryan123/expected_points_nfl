#!/bin/bash
#$ -N test_MLR
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

#$ -o job_output/$JOB_NAME-$JOB_ID.log
## MORE RAM
#$ -l m_mem_free=10G

Rscript --vanilla test_predPerf_MLR.R