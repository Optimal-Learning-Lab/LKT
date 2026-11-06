#!/bin/bash -l
#SBATCH --job-name=lkt_grid_report
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=01:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
for part in cv{1..5}; do
  for shard in {0..7}; do test -f "FINAL_${part}_${shard}_COMPLETE"; done
done
module purge
module load singularity/4.1.2
set -u
source deployment.env
container_env="LKT_RELEASE_FILE=/work/workflows/simulation/shared/lkt-release.env,R_LIBS_USER=/work/$lkt_library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/compare-gru-training-grid.R /work /work/results report cv1 0
test -f results/reports/verification.txt
touch COMPARISON_COMPLETE
