#!/bin/bash -l
#SBATCH --job-name=lkt_timing_report
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
for part in cv{1..5}; do test -f "FINAL_${part}_COMPLETE"; done
module purge
module load singularity/4.1.2
set -u
dependencies=/home/ppavlik/LKT-encoder-study-20261103
reference=$(python3 -c 'import json; print(json.load(open("remote-inputs.json"))["reference_directory"])')
runtime="R_LIBS_USER=/reference/candidate-library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
for action in report check; do
 singularity exec --userns --cleanenv --bind "$PWD:/work" --bind "$reference:/reference:ro" --pwd /work --env "$runtime" "$dependencies/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/compare-gru-timing-only.R /work /work/results "$action" cv1 /reference/candidate/candidate.json /reference/candidate-library
done
test -f results/reports/verification.json
touch COMPARISON_COMPLETE
