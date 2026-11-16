#!/bin/bash -l
#SBATCH --job-name=pfa_sem_setup
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=01:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
module purge
module load singularity/4.1.2
set -u
if [[ $# -ne 2 ]]; then
  echo 'Usage: sbatch setup-pfa-semantic-runtime.slurm ABSOLUTE_CONTROLLER.py ABSOLUTE_CONFIG.json' >&2
  exit 2
fi
exec python3 "$1" "$2"
