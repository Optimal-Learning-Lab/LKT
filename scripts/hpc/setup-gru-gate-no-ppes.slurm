#!/bin/bash -l
#SBATCH --job-name=lkt_no_ppes_setup
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=04:00:00
#SBATCH --output=logs/%x_%A_%a.out
#SBATCH --error=logs/%x_%A_%a.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
sha256sum -c bundle.sha256
module purge
module load singularity/4.1.2
set -u
candidate_root=$(python3 -c 'import json;print(json.load(open("inputs.json"))["candidate_root"])')
full=$(python3 -c 'import json;print(json.load(open("inputs.json"))["full"])')
old=$(python3 -c 'import json;print(json.load(open("inputs.json"))["no_ppes"])')
dependencies=$(python3 -c 'import json;print(json.load(open("inputs.json"))["dependencies"])')
runtime="R_LIBS_USER=/candidate-library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
run_action() {
 singularity exec --userns --cleanenv --bind "$PWD:/work" --bind "$candidate_root/candidate:/candidate:ro" --bind "$candidate_root/candidate-library:/candidate-library:ro" --bind "$full:/full:ro" --bind "$old:/old:ro" --pwd /work --env "$runtime" "$dependencies/rocker-r-4.5.3.sif" Rscript --vanilla /work/scripts/compare-gru-gate-no-ppes.R /work /work/results "$1" "$2" /candidate/candidate.json /candidate-library /full /old
}
run_action plan 0
run_action smoke 0
touch SETUP_COMPLETE
