#!/bin/bash -l
#SBATCH --job-name=pfa_r_setup
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=16G
#SBATCH --time=02:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
module purge
module load singularity/4.1.2
set -u
if [[ $# -ne 3 ]]; then
  echo 'Usage: sbatch setup-pfa-environment.slurm ABSOLUTE_CONTROLLER.py ABSOLUTE_CONFIG.dcf dependencies|package' >&2
  exit 2
fi
exec python3 "$1" "$2" "$3"
