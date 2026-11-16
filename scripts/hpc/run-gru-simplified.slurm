#!/bin/bash -l
#SBATCH --job-name=lkt_simple_fit
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=6
#SBATCH --mem=16G
#SBATCH --time=02:00:00
#SBATCH --array=0-4%5
#SBATCH --output=logs/%x_%A_%a.out
#SBATCH --error=logs/%x_%A_%a.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
test -f SETUP_COMPLETE
module purge
module load singularity/4.1.2
set -u
dependencies=/home/ppavlik/LKT-encoder-study-20261103
(cd "$dependencies" && sha256sum -c container.sha256)
parts=(cv1 cv2 cv3 cv4 cv5)
part="${parts[$SLURM_ARRAY_TASK_ID]}"
runtime="R_LIBS_USER=/work/candidate-library,LKT_STUDY_WORKERS=$SLURM_CPUS_PER_TASK,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$runtime" "$dependencies/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/compare-gru-simplified.R /work /work/results fit "$part" /work/candidate/candidate.json /work/candidate-library
touch "FINAL_${part}_COMPLETE"
