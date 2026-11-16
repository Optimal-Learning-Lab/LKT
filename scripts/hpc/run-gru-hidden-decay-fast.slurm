#!/bin/bash -l
#SBATCH --job-name=lkt_fast_fit
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=6G
#SBATCH --time=04:00:00
#SBATCH --array=0-59%60
#SBATCH --output=logs/%x_%A_%a.out
#SBATCH --error=logs/%x_%A_%a.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
test -f SETUP_COMPLETE
module purge
module load singularity/4.1.2
set -u
dependencies=$(python3 -c 'import json; print(json.load(open("remote-inputs.json"))["dependencies_directory"])')
(cd "$dependencies" && sha256sum -c container.sha256)
runtime="R_LIBS_USER=/work/candidate-library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$runtime" "$dependencies/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/compare-gru-hidden-decay-fast.R /work /work/results fit "$SLURM_ARRAY_TASK_ID" /work/candidate/candidate.json /work/candidate-library
touch "FIT_${SLURM_ARRAY_TASK_ID}_COMPLETE"
