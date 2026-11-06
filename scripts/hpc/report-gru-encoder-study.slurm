#!/bin/bash -l
#SBATCH --job-name=lkt_encoder_report
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
module purge
module load singularity/4.1.2
set -u
for part in development cv{1..5}; do test -f "FINAL_${part}_COMPLETE"; done
container_env="LKT_STUDY_LIBRARY=/work/library-current,R_LIBS_USER=/work/library-current,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/run-gru-encoder-study.R /work /work/results /work/release.json report development 0 8
test -f results/verification.txt
touch ENCODER_STUDY_COMPLETE
