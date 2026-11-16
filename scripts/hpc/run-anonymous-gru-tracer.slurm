#!/bin/bash -l
#SBATCH --job-name=lkt_anonymous_tracer
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=24:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
module purge
module load singularity/4.1.2
set -u
[[ $# -eq 2 ]]
CONFIG=$1; EXPECTED=$2
mapfile -d '' -t C < <(python3 - "$CONFIG" "$EXPECTED" <<'PY'
import hashlib,json,sys
from pathlib import Path
p=Path(sys.argv[1]);assert hashlib.sha256(p.read_bytes()).hexdigest()==sys.argv[2]
c=json.loads(p.read_text());assert c['workers']==1 and c['storage_bytes_limit']==10000000000
for k in ['workspace','container','container_sha256','dependencies','python','release_file','root']:
 sys.stdout.buffer.write(str(c[k]).encode()+b'\0')
PY
)
[[ ${#C[@]} -eq 7 ]]
W=${C[0]}; SIF=${C[1]}; DEPS=${C[3]}
[[ $(sha256sum "$SIF" | cut -d' ' -f1) == "${C[2]}" ]]
# Research runs use the explicitly declared, already verified release. Package
# installation/promotion is a separate release action, never a fitting side effect.
SCRATCH=$(mktemp -d "/tmp/anonymous-tracer.${SLURM_JOB_ID:?}.XXXXXX")
cleanup() { [[ "$SCRATCH" == "/tmp/anonymous-tracer.${SLURM_JOB_ID}."* ]] && rm -rf -- "$SCRATCH"; }
trap cleanup EXIT
mkdir -p "$SCRATCH/home" "$SCRATCH/cache" "$SCRATCH/tmp"
export SINGULARITY_CACHEDIR="$SCRATCH/cache" SINGULARITY_TMPDIR="$SCRATCH/tmp"
ENVIRONMENT="TMPDIR=$SCRATCH/tmp,R_LIBS=$DEPS,R_LIBS_USER=$DEPS,R_LIBS_SITE=,LC_ALL=C.UTF-8,LANG=C.UTF-8,LANGUAGE=C,R_TESTS=,OMP_NUM_THREADS=1,MKL_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,TORCH_VERIFY_LOAD=TRUE,LKT_RELEASE_FILE=${C[5]},PYTHONDONTWRITEBYTECODE=1,PYTHONNOUSERSITE=1,TORCH_HOME=$DEPS/torch,TORCH_INSTALL=0,SLURM_JOB_ID=${SLURM_JOB_ID:?}"
singularity exec --userns --cleanenv --no-home --bind "/project/ppavlik:/project/ppavlik" \
 --bind "/home/ppavlik:/tracer-account-home:ro" --bind "$SCRATCH:$SCRATCH" --pwd "$W" \
 --env "$ENVIRONMENT" "$SIF" "${C[4]}" -B "${C[6]}/scripts/run-anonymous-gru-tracer.py" "$CONFIG"
