#!/bin/bash -l
#SBATCH --job-name=lkt_tracer_score
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=00:30:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
module purge
module load singularity/4.1.2
set -u
[[ $# -eq 3 ]]
CONFIG=$1; EXPECTED=$2; CHECK=$3
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
[[ "$CHECK" == "$W/check-"* && -f "$CHECK/VERIFICATION_COMPLETE" ]]
[[ $(sha256sum "$SIF" | cut -d' ' -f1) == "${C[2]}" ]]
python3 "$W/scripts/hpc/deploy-verified-lkt.py" "$CONFIG" "$CHECK"
cp "$W/scripts/lkt_release.R" "$(dirname "${C[5]}")/lkt_release.R"
SCRATCH=$(mktemp -d "/tmp/anonymous-tracer.${SLURM_JOB_ID:?}.XXXXXX")
cleanup() { [[ "$SCRATCH" == "/tmp/anonymous-tracer.${SLURM_JOB_ID}."* ]] && rm -rf -- "$SCRATCH"; }
trap cleanup EXIT
mkdir -p "$SCRATCH/home" "$SCRATCH/cache" "$SCRATCH/tmp"
export SINGULARITY_CACHEDIR="$SCRATCH/cache" SINGULARITY_TMPDIR="$SCRATCH/tmp"
ENVIRONMENT="TMPDIR=$SCRATCH/tmp,R_LIBS=$DEPS,R_LIBS_USER=$DEPS,R_LIBS_SITE=,LC_ALL=C.UTF-8,LANG=C.UTF-8,LANGUAGE=C,R_TESTS=,OMP_NUM_THREADS=1,MKL_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,TORCH_VERIFY_LOAD=TRUE,LKT_RELEASE_FILE=${C[5]},PYTHONDONTWRITEBYTECODE=1,PYTHONNOUSERSITE=1,TORCH_HOME=$DEPS/torch,TORCH_INSTALL=0,SLURM_JOB_ID=${SLURM_JOB_ID:?}"
singularity exec --userns --cleanenv --no-home --bind "/project/ppavlik:/project/ppavlik" \
 --bind "/home/ppavlik:/tracer-account-home:ro" --bind "$SCRATCH:$SCRATCH" --pwd "$W" \
 --env "$ENVIRONMENT" "$SIF" /usr/local/lib/R/bin/Rscript --vanilla "${C[6]}/scripts/benchmark-gru-tracer-scoring.R" "${C[6]}" "$W/results" "$W/scoring-benchmark"
