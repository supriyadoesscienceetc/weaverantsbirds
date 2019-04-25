#!/bin/bash
#SBATCH --job-name=dada2_sbatch
#SBATCH --output=dada2_sbatch.out
#SBATCH --error=dada2_sbatch.err
#SBATCH --time=00:30:00
#SBATCH --partition=broadwl
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=16
#SBATCH --mem-per-cpu=2000

module load R/3.4.3
R CMD BATCH --no-save --no-restore dada2_out3.R
