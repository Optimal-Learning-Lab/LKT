#!/bin/bash -l
#SBATCH --job-name=lkt_candidate_batch
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
CONFIG=$1;EXPECTED=$2;MODE=$3
[[ "$MODE" == capture || "$MODE" == verify ]]
mapfile -d '' -t C < <(python3 - "$CONFIG" "$EXPECTED" "$MODE" <<'PY'
import hashlib,json,sys
from pathlib import Path
p=Path(sys.argv[1]);assert hashlib.sha256(p.read_bytes()).hexdigest()==sys.argv[2]
c=json.loads(p.read_text());assert c['storage_bytes_limit']==10000000000
for k in ['workspace','container','container_sha256','dependencies','root','results']:
 sys.stdout.buffer.write(str(c[k]).encode()+b'\0')
sys.stdout.buffer.write(str(c['baseline_release_file' if sys.argv[3]=='capture' else 'release_file']).encode()+b'\0')
sys.stdout.buffer.write(str(c.get('baseline_directory',c['workspace']+'/baseline')).encode()+b'\0')
sys.stdout.buffer.write(str(c.get('model_file','cv1__anonymous_constant__seed1.rds')).encode()+b'\0')
PY
)
[[ ${#C[@]} -eq 9 ]]
W=${C[0]};SIF=${C[1]};DEPS=${C[3]}
[[ $(sha256sum "$SIF" | cut -d' ' -f1) == "${C[2]}" ]]
SCRATCH=$(mktemp -d "/tmp/candidate-score.${SLURM_JOB_ID:?}.XXXXXX")
cleanup() { [[ "$SCRATCH" == "/tmp/candidate-score.${SLURM_JOB_ID}."* ]] && rm -rf -- "$SCRATCH"; }
trap cleanup EXIT
mkdir -p "$SCRATCH/home" "$SCRATCH/cache" "$SCRATCH/tmp"
export SINGULARITY_CACHEDIR="$SCRATCH/cache" SINGULARITY_TMPDIR="$SCRATCH/tmp"
ENVIRONMENT="TMPDIR=$SCRATCH/tmp,R_LIBS=$DEPS,R_LIBS_USER=$DEPS,R_LIBS_SITE=,LC_ALL=C.UTF-8,LANG=C.UTF-8,LANGUAGE=C,R_TESTS=,OMP_NUM_THREADS=1,MKL_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,TORCH_VERIFY_LOAD=TRUE,LKT_RELEASE_FILE=${C[6]},PYTHONDONTWRITEBYTECODE=1,PYTHONNOUSERSITE=1,TORCH_HOME=$DEPS/torch,TORCH_INSTALL=0"
singularity exec --userns --cleanenv --no-home --bind "/project/ppavlik:/project/ppavlik" \
 --bind "/home/ppavlik:/tracer-account-home:ro" --bind "$SCRATCH:$SCRATCH" --pwd "$W" \
 --env "$ENVIRONMENT" "$SIF" /usr/local/lib/R/bin/Rscript --vanilla \
 "${C[4]}/scripts/benchmark-gru-candidate-batching.R" "${C[4]}" "${C[5]}" "$W/$MODE-${SLURM_JOB_ID}" "$MODE" "${C[7]}" "${C[8]}"
