#!/bin/bash -l
#SBATCH --job-name=lkt_simple_report
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=01:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
for part in cv{1..5}; do test -f "FINAL_${part}_COMPLETE"; done
module purge
module load singularity/4.1.2
set -u
dependencies=/home/ppavlik/LKT-encoder-study-20261103
runtime="R_LIBS_USER=/work/candidate-library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
for action in report check; do
  singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$runtime" "$dependencies/rocker-r-4.5.3.sif" \
    Rscript --vanilla /work/scripts/compare-gru-simplified.R /work /work/results "$action" cv1 /work/candidate/candidate.json /work/candidate-library
done
test -f results/reports/verification.txt
touch COMPARISON_COMPLETE
