#!/bin/bash -l
#SBATCH --job-name=lkt_encoder_followup
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=6
#SBATCH --mem=12G
#SBATCH --time=04:00:00
#SBATCH --array=0-5%2
#SBATCH --output=logs/%x_%A_%a.out
#SBATCH --error=logs/%x_%A_%a.err
# Validation determines the deterministic finalists and parameter-matched controls.
# Gaussian/control followups finish before final choices are frozen and test-scored.
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
module purge
module load singularity/4.1.2
set -u
parts=(development cv1 cv2 cv3 cv4 cv5)
part="${parts[$SLURM_ARRAY_TASK_ID]}"
for shard in {0..7}; do test -f "GRID_${part}_${shard}_COMPLETE"; done
container_env="LKT_STUDY_WORKERS=$SLURM_CPUS_PER_TASK,LKT_STUDY_LIBRARY=/work/library-current,R_LIBS_USER=/work/library-current,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/run-gru-encoder-study.R /work /work/results /work/release.json finalize "$part" 0 8
test -f "results/reports/${part}-verification.txt"
touch "FINAL_${part}_COMPLETE"
