#!/bin/bash -l
#SBATCH --job-name=lkt_ednet_author_report
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=24G
#SBATCH --time=04:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
module purge
module load singularity/4.1.2
set -u
source deployment.env
container_env="LKT_RELEASE_FILE=/work/workflows/simulation/shared/lkt-release.env,R_LIBS_USER=/work/$lkt_library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/run-gru-ednet-author-protocol.R /work /work/study report
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/run-gru-ednet-author-protocol.R /work /work/study check
touch COMPARISON_COMPLETE
