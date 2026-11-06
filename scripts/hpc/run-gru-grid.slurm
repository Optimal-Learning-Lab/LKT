#!/bin/bash -l
#SBATCH --job-name=lkt_gru_grid
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#SBATCH --mem=24G
#SBATCH --time=08:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
# Run the grid only after the isolated environment has passed setup. The allocated CPUs
# become independent single-threaded R workers.
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
test -f SETUP_COMPLETE
module purge
module load singularity/4.1.2
set -u
# Verify the same container image used by the successful setup job. This keeps all 108
# fresh fits within one execution environment.
sha256sum -c container.sha256
container_sha=$(cut -d ' ' -f 1 container.sha256)
container_env="LKT_GRU_LIBRARY=/work/library-container,R_LIBS_USER=/work/library-container,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1,LKT_GRU_CONTAINER_SHA=$container_sha"
# Run the frozen compact grid with caching disabled for the earlier Windows fits. The
# driver automatically selects by validation and independently verifies the final test
# report.
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/source/scripts/grid-search-gru.R /work/source /work/policy \
  /work/reference /work/results "$SLURM_CPUS_PER_TASK" all compact run /work/reference none
test -f results/verification.txt
# Mark completion only after the driver exits successfully and its verification file
# exists. Submission or partial training is not treated as a completed search.
touch GRID_COMPLETE
