#!/bin/bash
#SBATCH -J tri-m_more-pred
#SBATCH --account=gts-mhimmelstein3
#SBATCH -N 1 --ntasks-per-node=1
#SBATCH --cpus-per-task=6
#SBATCH --mem-per-cpu=32G
#SBATCH -t 250:00:00
#SBATCH -q inferno
#SBATCH -o report-%j.out
#SBATCH --chdir=/storage/home/hcoda1/4/jhelmer3/r-mhimmelstein3-0/WOA/advice-taking-metaanalysis

dir

free -h

module purge
module load r/4.5.2-tidy
Rscript Scripts/tri-m_more-pred_2.1_model.R