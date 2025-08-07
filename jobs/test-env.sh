#!/bin/bash
#
# test-env.sh
# 
# A quick SLURM test script to verify that environment variables from .env 
# are loading correctly before submitting long-running computational jobs.
# 
# This script:
# - Loads variables from .env file 
# - Tests directory changes using PROJECT_WORKDIR
# - Verifies R module loading
# - Runs a quick R test
#
# To use: sbatch test-env.sh
#
#SBATCH --job-name=test-env                 # create a short name for your job
#SBATCH --output=slurm-logs/slurm-%A.%a.out # stdout file
#SBATCH --error=slurm-logs/slurm-%A.%a.err  # stderr file
#SBATCH --open-mode=append
#SBATCH --nodes=1                          # node count
#SBATCH --ntasks=1                         # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                  # minimal CPU
#SBATCH --array=1
#SBATCH --mem-per-cpu=1G                   # minimal memory
#SBATCH --time=0:02:00                     # only 2 minutes!
#SBATCH --mail-type=all                    # send email on start, end and fault
#SBATCH --mail-user=${SLURM_MAIL_USER}     # using env variable

# Load environment variables from .env file
if [ -f .env ]; then
    echo "Loading environment variables from .env file..."
    export $(cat .env | xargs)
else
    echo "ERROR: .env file not found!"
    exit 1
fi

# Change to project directory using env variable
cd ${PROJECT_WORKDIR}

echo "========== ENVIRONMENT TEST RESULTS =========="
echo "My SLURM_JOB_ID is $SLURM_JOB_ID."
echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)
echo ""
echo "Environment Variables from .env:"
echo "SLURM_MAIL_USER = ${SLURM_MAIL_USER}"
echo "PROJECT_WORKDIR = ${PROJECT_WORKDIR}"
echo ""
echo "Current working directory: $(pwd)"
echo "Directory contents:"
ls -la
echo ""
echo "Job started at: $(date)"

# Test R is available
module purge
module load R/4.4.2
echo "R version:"
Rscript --version

# Quick R test
echo "Testing R execution:"
Rscript -e "cat('R is working! Current time:', as.character(Sys.time()), '\n')"

echo "Job finished at: $(date)"
echo "========== TEST COMPLETED SUCCESSFULLY =========="