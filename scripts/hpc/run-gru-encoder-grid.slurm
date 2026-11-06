#!/bin/bash -l
#SBATCH --job-name=lkt_encoder_grid
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=6
#SBATCH --mem=12G
#SBATCH --time=08:00:00
#SBATCH --array=0-47%8
#SBATCH --output=logs/%x_%A_%a.out
#SBATCH --error=logs/%x_%A_%a.err
# Eight shards per partition, each with six one-thread workers. The global array
# cap bounds this study to 48 concurrent CPUs without changing other user jobs.
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
test -f SETUP_COMPLETE
module purge
module load singularity/4.1.2
set -u
sha256sum -c container.sha256
parts=(development cv1 cv2 cv3 cv4 cv5)
part="${parts[$((SLURM_ARRAY_TASK_ID / 8))]}"
shard=$((SLURM_ARRAY_TASK_ID % 8))
container_env="LKT_STUDY_WORKERS=$SLURM_CPUS_PER_TASK,LKT_STUDY_LIBRARY=/work/library-current,R_LIBS_USER=/work/library-current,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/run-gru-encoder-study.R /work /work/results /work/release.json fit "$part" "$shard" 8
touch "GRID_${part}_${shard}_COMPLETE"
