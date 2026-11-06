#!/bin/bash -l
#SBATCH --job-name=lkt_ednet_author_fit
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=24G
#SBATCH --time=24:00:00
#SBATCH --array=1-5%3
#SBATCH --output=logs/%x_%A_%a.out
#SBATCH --error=logs/%x_%A_%a.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
test -f SETUP_COMPLETE
module purge
module load singularity/4.1.2
set -u
source deployment.env
sha256sum -c container.sha256
task_id=$(python3 - "$SLURM_ARRAY_TASK_ID" <<'PY'
import csv,re,sys
with open('study/tasks.csv') as f:tasks=list(csv.DictReader(f))
index=int(sys.argv[1])-1
assert 0<=index<len(tasks)
task=tasks[index]['id'];assert re.fullmatch(r'(?:fold[0-4]|holdout)__seed[0-9]+',task)
print(task)
PY
)
container_env="LKT_RELEASE_FILE=/work/workflows/simulation/shared/lkt-release.env,R_LIBS_USER=/work/$lkt_library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/run-gru-ednet-author-protocol.R /work /work/study fit "$task_id"
touch "TASK_${SLURM_ARRAY_TASK_ID}_COMPLETE"
