#!/bin/bash -l
#SBATCH --job-name=lkt_student_report
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=12G
#SBATCH --time=00:30:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
for task in {0..14}; do test -f "STUDENT_FIT_${task}_COMPLETE"; done
module purge
module load singularity/4.1.2
set -u
reference=$(python3 -c 'import json; print(json.load(open("inputs.json"))["remote_reference"])')
candidate_root=$(python3 -c 'import json; print(json.load(open("inputs.json"))["remote_candidate_root"])')
dependencies=$(python3 -c 'import json; print(json.load(open("inputs.json"))["dependencies"])')
runtime="R_LIBS_USER=/candidate-library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
for action in report check; do
 singularity exec --userns --cleanenv --bind "$PWD:/work" --bind "$reference:/reference:ro" --bind "$candidate_root/candidate:/candidate:ro" --bind "$candidate_root/candidate-library:/candidate-library:ro" --pwd /work --env "$runtime" "$dependencies/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/compare-gru-plasticity-student.R /work /work/results-constant "$action" 0 /candidate/candidate.json /candidate-library /reference
done
test -f results-constant/reports/verification.json
touch STUDENT_COMPARISON_COMPLETE
