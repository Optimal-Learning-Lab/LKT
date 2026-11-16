#!/bin/bash -l
#SBATCH --job-name=lkt_single_fit
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=3
#SBATCH --mem=12G
#SBATCH --time=01:00:00
#SBATCH --array=0-14%15
#SBATCH --output=logs/%x_%A_%a.out
#SBATCH --error=logs/%x_%A_%a.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
test -f SETUP_COMPLETE
module purge
module load singularity/4.1.2
set -u
dependencies=/home/ppavlik/LKT-encoder-study-20261103
(cd "$dependencies" && sha256sum -c container.sha256)
reference=$(python3 -c 'import json; print(json.load(open("remote-inputs.json"))["reference_directory"])')
runtime_root=$(python3 -c 'import json; print(json.load(open("remote-inputs.json"))["runtime_directory"])')
parts=(cv1 cv2 cv3 cv4 cv5)
part="${parts[$((SLURM_ARRAY_TASK_ID / 3))]}"
conditions=(add_item_logitdec add_student_logitdec add_item_ppes)
condition="${conditions[$((SLURM_ARRAY_TASK_ID % 3))]}"
runtime="R_PARALLEL_PORT=$((14000 + SLURM_ARRAY_TASK_ID)),R_LIBS_USER=/runtime/candidate-library,LKT_STUDY_WORKERS=$SLURM_CPUS_PER_TASK,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
singularity exec --userns --cleanenv --bind "$PWD:/work" --bind "$reference:/reference:ro" --bind "$runtime_root:/runtime:ro" --pwd /work --env "$runtime" "$dependencies/rocker-r-4.5.3.sif" \
 Rscript --vanilla /work/scripts/compare-gru-single-additions.R /work /work/results fit "$part:$condition" /runtime/candidate/candidate.json /runtime/candidate-library
touch "FINAL_${part}_${condition}_COMPLETE"
