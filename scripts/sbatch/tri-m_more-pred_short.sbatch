#!/bin/bash
#SBATCH -J tri-m_more-pred_short
#SBATCH --account=gts-mhimmelstein3
#SBATCH -C graniterapids
#SBATCH -N 1 --ntasks-per-node=1
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=8G
#SBATCH -t 1:00:00
#SBATCH -q embers
#SBATCH -o report-%j.out
#SBATCH --chdir=/storage/home/hcoda1/4/jhelmer3/r-mhimmelstein3-0/WOA/advice-taking-metaanalysis

dir

free -h
cat /proc/meminfo

module purge
module load r/4.5.2-tidy
Rscript Scripts/tri-m_more-pred_2.1_model.R