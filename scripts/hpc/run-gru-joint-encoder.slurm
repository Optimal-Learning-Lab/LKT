#!/bin/bash -l
#SBATCH --job-name=lkt_joint_compare
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=6
#SBATCH --mem=12G
#SBATCH --time=02:00:00
#SBATCH --array=0-5%6
#SBATCH --output=logs/%x_%A_%a.out
#SBATCH --error=logs/%x_%A_%a.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
test -f SETUP_COMPLETE
mkdir -p results/models results/validation results/training results/reports
module purge
module load singularity/4.1.2
set -u
sha256sum -c container.sha256
parts=(development cv1 cv2 cv3 cv4 cv5)
part="${parts[$SLURM_ARRAY_TASK_ID]}"
container_env="LKT_RELEASE_FILE=/work/workflows/simulation/shared/lkt-release.env,LKT_STUDY_WORKERS=$SLURM_CPUS_PER_TASK,R_LIBS_USER=/work/candidate-library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/compare-gru-joint-encoder.R /work /work/results fit "$part" /work/candidate/candidate.json /work/candidate-library
touch "FINAL_${part}_COMPLETE"
