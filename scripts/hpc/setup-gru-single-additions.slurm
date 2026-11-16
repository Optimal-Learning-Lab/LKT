#!/bin/bash -l
#SBATCH --job-name=lkt_single_setup
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=00:30:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
sha256sum -c bundle.sha256
python3 scripts/verify-gru-followup-sources.py
module purge
module load singularity/4.1.2
set -u
dependencies=/home/ppavlik/LKT-encoder-study-20261103
(cd "$dependencies" && sha256sum -c container.sha256)
reference=$(python3 -c 'import json; print(json.load(open("remote-inputs.json"))["reference_directory"])')
runtime_root=$(python3 -c 'import json; print(json.load(open("remote-inputs.json"))["runtime_directory"])')
runtime="R_LIBS_USER=/runtime/candidate-library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
for action in plan verify smoke; do
 args=()
 if [[ "$action" == plan ]]; then args=(/reference/results); fi
 singularity exec --userns --cleanenv --bind "$PWD:/work" --bind "$reference:/reference:ro" --bind "$runtime_root:/runtime:ro" --pwd /work --env "$runtime" "$dependencies/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/compare-gru-single-additions.R /work /work/results "$action" cv1 /runtime/candidate/candidate.json /runtime/candidate-library "${args[@]}"
done
touch SETUP_COMPLETE
