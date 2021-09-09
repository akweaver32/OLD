#!/bin/bash
# Job name:
#SBATCH --job-name=sand_extract
#
# Account:
#SBATCH --account=fc_envids
#
# Partition:
#SBATCH --partition=savio2_htc
#
# Request one node:
#SBATCH --nodes=1
#
# Specify task:
#SBATCH --ntasks-per-node=1
#
# Number of processors for threading:
#SBATCH --cpus-per-task=1
#
# Wall clock limit:
#SBATCH --time=12:00:00
#
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=amanda_weaver@berkeley.edu

## Command(s) to run (example):
module load r
module load r-packages
module load r-spatial
R CMD BATCH raster_extract.R 