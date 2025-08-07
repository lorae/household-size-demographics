#!/bin/bash
#SBATCH --job-name=2019-p                        # short name for job
#SBATCH --output=slurm-logs/slurm-%A.%a.out      # stdout file
#SBATCH --error=slurm-logs/slurm-%A.%a.err       # stderr file
#SBATCH --open-mode=append
#SBATCH --nodes=1                                # node count
#SBATCH --ntasks=1                               # total number of tasks across all nodes
#SBATCH --cpus-per-task=10                       # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --array=1
#SBATCH --mem=2000G                              # → 2000 GB total
#SBATCH --time=1:00:00                           # total run time limit (HH:MM:SS)
#SBATCH --mail-type=all                          # send email on start, end and fault
#SBATCH --mail-user=${SLURM_MAIL_USER}           # using env variable
# Load environment variables from .env file
if [ -f .env ]; then
    set -a  # automatically export all variables
    source <(grep -v '^#' .env | grep -v '^$')  # skip comments and empty lines
    set +a  # turn off automatic export
fi
# Ensure slurm-logs directory exists
mkdir -p slurm-logs
cd ${PROJECT_WORKDIR}
echo "My SLURM_JOB_ID is $SLURM_JOB_ID."
echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
module purge
module load R/4.4.2
# Print start time
echo "Job started at: $(date)"
# Run R script
Rscript ./src/scripts/reg01/2019_p.R
# Print end time
echo "Job finished at: $(date)"
exit