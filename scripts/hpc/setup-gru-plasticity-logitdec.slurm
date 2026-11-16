#!/bin/bash -l
#SBATCH --job-name=lkt_logit_setup
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=12G
#SBATCH --time=00:40:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
sha256sum -c bundle.sha256
python3 scripts/verify-plasticity-logitdec-inputs.py
module purge
module load singularity/4.1.2
set -u
reference=$(python3 -c 'import json; print(json.load(open("inputs.json"))["remote_reference"])')
candidate_root=$(python3 -c 'import json; print(json.load(open("inputs.json"))["remote_candidate_root"])')
dependencies=$(python3 -c 'import json; print(json.load(open("inputs.json"))["dependencies"])')
runtime="R_LIBS_USER=/candidate-library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
for action in plan verify smoke; do
  args=();if [[ "$action" == plan ]]; then args=(/reference); fi
  singularity exec --userns --cleanenv --bind "$PWD:/work" --bind "$reference:/reference:ro" \
    --bind "$candidate_root/candidate:/candidate:ro" --bind "$candidate_root/candidate-library:/candidate-library:ro" \
    --pwd /work --env "$runtime" "$dependencies/rocker-r-4.5.3.sif" \
    Rscript --vanilla /work/scripts/compare-gru-plasticity-logitdec.R /work /work/results "$action" 0 /candidate/candidate.json /candidate-library "${args[@]}"
done
touch SETUP_COMPLETE
