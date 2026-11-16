#!/bin/bash -l
#SBATCH --job-name=lasso_check_deps
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=8G
#SBATCH --time=01:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
module purge
module load singularity/4.1.2
set -u
[[ $# -eq 1 ]]
CONFIG=$1
mapfile -d '' -t C < <(python3 - "$CONFIG" <<'PY'
import json,sys
c=json.load(open(sys.argv[1]))
for k in ['workspace','container','container_sha256','dependencies']:
 sys.stdout.buffer.write(str(c[k]).encode()+b'\0')
PY
)
[[ ${#C[@]} -eq 4 ]]
W=${C[0]}; SIF=${C[1]}; DEPS=${C[3]}
[[ $(sha256sum "$SIF" | cut -d' ' -f1) == "${C[2]}" ]]
BEFORE=$(du -sb /home/ppavlik /project/ppavlik | awk '{s+=$1} END {printf "%.0f",s}')
[[ "$BEFORE" -lt 9000000000 ]]
SCRATCH=$(mktemp -d "/tmp/lasso-deps.${SLURM_JOB_ID:?}.XXXXXX")
cleanup() { [[ "$SCRATCH" == "/tmp/lasso-deps.${SLURM_JOB_ID}."* ]] && rm -rf -- "$SCRATCH"; }
trap cleanup EXIT
mkdir -p "$SCRATCH/home" "$SCRATCH/cache" "$SCRATCH/tmp"
export SINGULARITY_CACHEDIR="$SCRATCH/cache" SINGULARITY_TMPDIR="$SCRATCH/tmp"
singularity exec --userns --cleanenv --no-home --bind "/project/ppavlik:/project/ppavlik" \
 --bind "$SCRATCH:$SCRATCH" --pwd "$W" \
 --env "HOME=$SCRATCH/home,TMPDIR=$SCRATCH/tmp,R_LIBS=$DEPS,R_LIBS_USER=$DEPS,R_LIBS_SITE=,LC_ALL=C.UTF-8,LANG=C.UTF-8,OMP_NUM_THREADS=1,MKL_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1" \
 "$SIF" /usr/local/lib/R/bin/Rscript --vanilla "$W/scripts/hpc/install-lasso-check-dependencies.R" \
 "$DEPS" "$W/check-dependencies-${SLURM_JOB_ID}.rds"
AFTER=$(du -sb /home/ppavlik /project/ppavlik | awk '{s+=$1} END {printf "%.0f",s}')
[[ "$AFTER" -le 10000000000 ]]
printf 'persistent bytes before=%s after=%s\n' "$BEFORE" "$AFTER"
