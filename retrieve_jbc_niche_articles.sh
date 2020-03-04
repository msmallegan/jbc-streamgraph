#!/bin/bash
#SBATCH -p short
#SBATCH --job-name=retrieve_pubmed
#SBATCH --mail-type=END,FAIL            # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=michael.smallegan@colorado.edu
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=40gb                 # Memory limit
#SBATCH --time=15:00:00            # Time limit hrs:min:sec
#SBATCH --output=retrieve_pubmed.out
#SBATCH --error=retrieve_pubmed.err

pwd; hostname; date

echo "You've requested $SLURM_CPUS_ON_NODE core."

Rscript retrieve_pubmed.R

date