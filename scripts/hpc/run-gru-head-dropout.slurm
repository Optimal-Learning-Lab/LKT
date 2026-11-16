#!/bin/bash -l
#SBATCH --job-name=lkt_head_drop_fit
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=6
#SBATCH --mem=16G
#SBATCH --time=02:00:00
#SBATCH --array=0-9%10
#SBATCH --output=logs/%x_%A_%a.out
#SBATCH --error=logs/%x_%A_%a.err
# Each array task fits six configurations/seeds from one whole-participant fold.
# Six one-thread workers per task allow at most 60 CPUs across ten tasks.
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
test -f SETUP_COMPLETE
mkdir -p results/models results/validation results/training results/reports
module purge
module load singularity/4.1.2
set -u
(cd /home/ppavlik/LKT-encoder-study-20261103 && sha256sum -c container.sha256)
source deployment.env
parts=(cv1 cv2 cv3 cv4 cv5)
part="${parts[$((SLURM_ARRAY_TASK_ID % 5))]}"
shard=$((SLURM_ARRAY_TASK_ID / 5))
container_env="LKT_RELEASE_FILE=/work/workflows/simulation/shared/lkt-release.env,LKT_STUDY_WORKERS=$SLURM_CPUS_PER_TASK,R_LIBS_USER=/work/$lkt_library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "/home/ppavlik/LKT-encoder-study-20261103/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/compare-gru-head-dropout.R /work /work/results fit "$part" "$shard"
touch "FINAL_${part}_${shard}_COMPLETE"
