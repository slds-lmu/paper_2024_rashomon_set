#!/bin/bash

#SBATCH --time=12:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4096
#SBATCH -A mallet
#SBATCH --exclude=tknl[01-12]
#SBATCH --array=1-1000

module load gcc
module load r
module load libxml2

export RENV_WATCHDOG_ENABLED=false
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1

# Calculate dataset ID based on lowest 2 bits
case $((SLURM_ARRAY_TASK_ID & 3)) in
  0) DATASET="st" ;;
  1) DATASET="bs" ;;
  2) DATASET="cs" ;;
  3) DATASET="gc" ;;
esac

# Calculate seed by right-shifting by 2 bits and adding 1
SEED=$(((SLURM_ARRAY_TASK_ID >> 2) + 1))

# Run the R script with arguments
Rscript run_eval_lse.R "$DATASET" "$SEED" "$OPTIMIZER"


