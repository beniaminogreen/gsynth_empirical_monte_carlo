#!/bin/bash
#SBATCH --job-name=gsynth_empirical_monte_carlo
#SBATCH --output=logs/targets_%j.out
#SBATCH --error=logs/targets_%j.err
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=20
#SBATCH --time=1-00:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=bfg9@yale.edu
#SBATCH --requeue

module load R

mkdir -p logs

Rscript -e "targets::tar_make()"
