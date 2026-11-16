#!/bin/bash -l
#SBATCH --job-name=lkt_batch_simulator
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=16G
#SBATCH --time=00:20:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
module purge
module load singularity/4.1.2
set -u
[[ $# -eq 2 || $# -eq 3 ]]
CONFIG=$1;EXPECTED=$2;MODE=${3:-all}
[[ "$MODE" == all || "$MODE" == workflows || "$MODE" == logistic || "$MODE" == schedules ]]
mapfile -d '' -t C < <(python3 - "$CONFIG" "$EXPECTED" <<'PY'
import hashlib,json,sys
from pathlib import Path
p=Path(sys.argv[1]);assert hashlib.sha256(p.read_bytes()).hexdigest()==sys.argv[2]
c=json.loads(p.read_text());assert c['storage_bytes_limit']==10000000000
for k in ['workspace','container','container_sha256','dependencies','release_file','archive']:
 sys.stdout.buffer.write(str(c[k]).encode()+b'\0')
PY
)
[[ ${#C[@]} -eq 6 ]]
W=${C[0]};SIF=${C[1]};DEPS=${C[3]}
[[ $(sha256sum "$SIF" | cut -d' ' -f1) == "${C[2]}" ]]
SCRATCH=$(mktemp -d "/tmp/batch-simulator.${SLURM_JOB_ID:?}.XXXXXX")
cleanup() { [[ "$SCRATCH" == "/tmp/batch-simulator.${SLURM_JOB_ID}."* ]] && rm -rf -- "$SCRATCH"; }
trap cleanup EXIT
mkdir -p "$SCRATCH/home" "$SCRATCH/cache" "$SCRATCH/tmp"
export SINGULARITY_CACHEDIR="$SCRATCH/cache" SINGULARITY_TMPDIR="$SCRATCH/tmp"
ENVIRONMENT="TMPDIR=$SCRATCH/tmp,R_LIBS=$DEPS,R_LIBS_USER=$DEPS,R_LIBS_SITE=,LC_ALL=C.UTF-8,LANG=C.UTF-8,LANGUAGE=C,R_TESTS=,OMP_NUM_THREADS=1,MKL_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,TORCH_VERIFY_LOAD=TRUE,LKT_RELEASE_FILE=${C[4]},PYTHONDONTWRITEBYTECODE=1,PYTHONNOUSERSITE=1,TORCH_HOME=$DEPS/torch,TORCH_INSTALL=0"
run_r() {
 singularity exec --userns --cleanenv --no-home --bind "/project/ppavlik:/project/ppavlik" \
  --bind "$SCRATCH:$SCRATCH" --pwd "$W" --env "$ENVIRONMENT" "$SIF" \
  /usr/local/lib/R/bin/Rscript --vanilla "$@"
}
if [[ "$MODE" == schedules ]]; then
 run_r workflows/simulation/shared/tests/verify_bundle_participant_schedules.R "${C[4]}" "$W/skill_bundle.rds" "$W/schedules-${SLURM_JOB_ID}"
 exit 0
fi
if [[ "$MODE" == all ]]; then
 run_r workflows/simulation/shared/tests/verify_candidate_scoring.R "${C[4]}" "$W/adaptive_bundle.rds" "$W/skill_bundle.rds" "$W/simulator-${SLURM_JOB_ID}"
fi
# The diagnostic workflow fixture explicitly requires a GRU ground-truth role.
if [[ "$MODE" != logistic ]]; then
 run_r workflows/simulation/shared/tests/verify_workflows.R "$W/skill_bundle.rds" "${C[5]}" "$W/workflows-${SLURM_JOB_ID}"
fi
if [[ "$MODE" != workflows ]]; then
 run_r workflows/simulation/shared/tests/verify_logistic_tracer_compatibility.R "${C[4]}" "$W/skill_bundle.rds" "$W/logistic-${SLURM_JOB_ID}"
fi
