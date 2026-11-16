#!/bin/bash -l
#SBATCH --job-name=lkt_input_report
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
for task in {0..29}; do test -f "FIT_${task}_COMPLETE"; done
module purge
module load singularity/4.1.2
set -u
dependencies=$(python3 -c 'import json; print(json.load(open("remote-inputs.json"))["dependencies_directory"])')
reference=$(python3 -c 'import json; print(json.load(open("remote-inputs.json"))["reference_directory"])')
runtime="R_LIBS_USER=/work/candidate-library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
for action in report; do
  singularity exec --userns --cleanenv --bind "$PWD:/work" --bind "$reference:/reference:ro" --pwd /work --env "$runtime" "$dependencies/rocker-r-4.5.3.sif" \
    Rscript --vanilla /work/scripts/compare-gru-connection-input.R /work /work/results "$action" 0 /work/candidate/candidate.json /work/candidate-library /reference
done
test -f results/reports/verification.json
touch COMPARISON_COMPLETE
