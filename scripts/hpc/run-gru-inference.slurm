#!/bin/bash -l
#SBATCH --job-name=lkt_gru_inference
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#SBATCH --mem=24G
#SBATCH --time=08:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
# Run in a separate prepared workspace with source/, PolicyFactory2/, a checksum-verified
# container and a private library containing LKT 2.0.0.9009. No old benchmark
# checkpoints or previous simulation jobs are modified or reused.
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
test -f INFERENCE_SETUP_COMPLETE
module purge
module load singularity/4.1.2
set -u
sha256sum -c container.sha256
container_env="LKT_GRU_LIBRARY=/work/library-container,R_LIBS_USER=/work/library-container,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1,LKT_REQUIRE_TORCH_TESTS=true"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/source/scripts/compare-gru-inference.R /work/source /work/inference-results run "$SLURM_CPUS_PER_TASK" all
test -f inference-results/verification.txt
touch INFERENCE_COMPLETE
