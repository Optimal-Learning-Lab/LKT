#!/bin/bash -l
#SBATCH --job-name=lkt_gru_head_widths
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#SBATCH --mem=24G
#SBATCH --time=08:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
# Use the completed experiment's verified runtime as a read-only dependency.
# Every new checkpoint and log is written to this separate workspace.
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
module purge
module load singularity/4.1.2
set -u
previous=/home/ppavlik/LKT-gru-inference-20261102
test -f "$previous/INFERENCE_COMPLETE"
sha256sum -c bundle.sha256
(cd "$previous" && sha256sum -c container.sha256)
container_env="LKT_GRU_LIBRARY=/reference/library-container,R_LIBS_USER=/reference/library-container,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
# A short real-data check exercises all widths and input versions before the full
# search. Its separate output is never reused as a research checkpoint.
singularity exec --userns --cleanenv --bind "$PWD:/work,$previous:/reference:ro" --pwd /work --env "$container_env" "$previous/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/source/scripts/compare-gru-head-widths.R /work/source /work/smoke-results smoke 4 /reference/inference-results
test -f smoke-results/verification.txt
singularity exec --userns --cleanenv --bind "$PWD:/work,$previous:/reference:ro" --pwd /work --env "$container_env" "$previous/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/source/scripts/compare-gru-head-widths.R /work/source /work/head-width-results run "$SLURM_CPUS_PER_TASK" /reference/inference-results
test -f head-width-results/verification.txt
cp "$previous/container.sha256" container.sha256
cp "$previous/environment.rds" reference_environment.rds
touch HEAD_WIDTH_COMPLETE
