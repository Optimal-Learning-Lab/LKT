#!/bin/bash -l
#SBATCH --job-name=lkt_student_fit
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=6G
#SBATCH --time=04:00:00
#SBATCH --array=0-14%15
#SBATCH --output=logs/%x_%A_%a.out
#SBATCH --error=logs/%x_%A_%a.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
test -f STUDENT_SETUP_COMPLETE
module purge
module load singularity/4.1.2
set -u
reference=$(python3 -c 'import json; print(json.load(open("inputs.json"))["remote_reference"])')
candidate_root=$(python3 -c 'import json; print(json.load(open("inputs.json"))["remote_candidate_root"])')
dependencies=$(python3 -c 'import json; print(json.load(open("inputs.json"))["dependencies"])')
runtime="R_LIBS_USER=/candidate-library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
singularity exec --userns --cleanenv --bind "$PWD:/work" --bind "$candidate_root/candidate:/candidate:ro" --bind "$candidate_root/candidate-library:/candidate-library:ro" --pwd /work --env "$runtime" "$dependencies/rocker-r-4.5.3.sif" \
 Rscript --vanilla /work/scripts/compare-gru-plasticity-student.R /work /work/results-constant fit "$SLURM_ARRAY_TASK_ID" /candidate/candidate.json /candidate-library
touch "STUDENT_FIT_${SLURM_ARRAY_TASK_ID}_COMPLETE"
