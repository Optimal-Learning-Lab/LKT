#!/bin/bash -l
#SBATCH --job-name=lkt_single_report
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
for part in cv{1..5}; do for condition in add_item_logitdec add_student_logitdec add_item_ppes; do test -f "FINAL_${part}_${condition}_COMPLETE"; done; done
module purge
module load singularity/4.1.2
set -u
dependencies=/home/ppavlik/LKT-encoder-study-20261103
reference=$(python3 -c 'import json; print(json.load(open("remote-inputs.json"))["reference_directory"])')
runtime_root=$(python3 -c 'import json; print(json.load(open("remote-inputs.json"))["runtime_directory"])')
runtime="R_LIBS_USER=/runtime/candidate-library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
for action in report check; do
 singularity exec --userns --cleanenv --bind "$PWD:/work" --bind "$reference:/reference:ro" --bind "$runtime_root:/runtime:ro" --pwd /work --env "$runtime" "$dependencies/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/compare-gru-single-additions.R /work /work/results "$action" cv1 /runtime/candidate/candidate.json /runtime/candidate-library
done
test -f results/reports/verification.json
touch COMPARISON_COMPLETE
