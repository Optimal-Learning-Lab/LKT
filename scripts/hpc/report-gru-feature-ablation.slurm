#!/bin/bash -l
#SBATCH --job-name=lkt_feat_report
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=02:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
for dataset in cloze assistments; do
  for part in cv{1..5}; do test -f "FINAL_${dataset}_${part}_COMPLETE"; done
done
module purge
module load singularity/4.1.2
set -u
source deployment.env
container_env="LKT_RELEASE_FILE=/work/workflows/simulation/shared/lkt-release.env,R_LIBS_USER=/work/$lkt_library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "/home/ppavlik/LKT-encoder-study-20261103/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/compare-gru-feature-ablation.R /work /work/results report all cv1
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "/home/ppavlik/LKT-encoder-study-20261103/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/compare-gru-feature-ablation.R /work /work/results check all cv1
test -f results/reports/verification.txt
touch COMPARISON_COMPLETE
