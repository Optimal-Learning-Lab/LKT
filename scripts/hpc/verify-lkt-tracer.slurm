#!/bin/bash -l
#SBATCH --job-name=lkt_tracer_check
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=16G
#SBATCH --time=02:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
module purge
module load singularity/4.1.2
set -u
[[ $# -eq 2 ]]
CONFIG=$1
EXPECTED=$2
mapfile -d '' -t C < <(python3 - "$CONFIG" "$EXPECTED" <<'PY'
import hashlib,json,sys
from pathlib import Path
p=Path(sys.argv[1]);assert hashlib.sha256(p.read_bytes()).hexdigest()==sys.argv[2]
c=json.loads(p.read_text());assert c['storage_bytes_limit']==10000000000
for k in ['workspace','container','container_sha256','dependencies','archive','archive_sha256','semantic_test_config']:
 sys.stdout.buffer.write(str(c[k]).encode()+b'\0')
PY
)
[[ ${#C[@]} -eq 7 ]]
W=${C[0]}; SIF=${C[1]}; DEPS=${C[3]}; ARCHIVE=${C[4]}
[[ $(sha256sum "$SIF" | cut -d' ' -f1) == "${C[2]}" ]]
[[ $(sha256sum "$ARCHIVE" | cut -d' ' -f1) == "${C[5]}" ]]
SCRATCH=$(mktemp -d "/tmp/tracer-check.${SLURM_JOB_ID:?}.XXXXXX")
cleanup() { [[ "$SCRATCH" == "/tmp/tracer-check.${SLURM_JOB_ID}."* ]] && rm -rf -- "$SCRATCH"; }
trap cleanup EXIT
mkdir -p "$SCRATCH/home" "$SCRATCH/cache" "$SCRATCH/tmp" "$W/check-${SLURM_JOB_ID}"
export SINGULARITY_CACHEDIR="$SCRATCH/cache" SINGULARITY_TMPDIR="$SCRATCH/tmp"
# The slim research image lacks optional GUI/rendering system dependencies.
# R records the missing Suggests in its check log. All native tests still run; Torch is
# mandatory and the real semantic fixture is explicit, never substituted.
ENVIRONMENT="HOME=$SCRATCH/home,TMPDIR=$SCRATCH/tmp,R_LIBS=$DEPS,R_LIBS_USER=$DEPS,R_LIBS_SITE=,LC_ALL=C.UTF-8,LANG=C.UTF-8,LANGUAGE=C,R_TESTS=,OMP_NUM_THREADS=1,MKL_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,TORCH_VERIFY_LOAD=TRUE,LKT_REQUIRE_TORCH_TESTS=true,LKT_SEMANTIC_TEST_CONFIG=${C[6]},_R_CHECK_FORCE_SUGGESTS_=false,PYTHONDONTWRITEBYTECODE=1,PYTHONNOUSERSITE=1,HF_HUB_OFFLINE=1,TRANSFORMERS_OFFLINE=1,TOKENIZERS_PARALLELISM=false,TORCH_HOME=$DEPS/torch,TORCH_INSTALL=0"
singularity exec --userns --cleanenv --no-home --bind "/project/ppavlik:/project/ppavlik" \
  --bind "$SCRATCH:$SCRATCH" --pwd "$W/check-${SLURM_JOB_ID}" --env "$ENVIRONMENT" \
  "$SIF" /usr/local/lib/R/bin/R CMD check --no-manual --no-vignettes "$ARCHIVE" \
  > "$W/check-${SLURM_JOB_ID}/console.log" 2>&1
grep '^Status:' "$W/check-${SLURM_JOB_ID}/LKT.Rcheck/00check.log"
test -z "$(find "$W/check-${SLURM_JOB_ID}/LKT.Rcheck/tests" -name '*.fail' -print -quit)"
grep -q '^PASS Lasso frame inputs' "$W/check-${SLURM_JOB_ID}/LKT.Rcheck/tests/lasso-feature-selection.Rout"
grep -q '^PASS nonzero constant output plus local item updates' "$W/check-${SLURM_JOB_ID}/LKT.Rcheck/tests/gru-adaptive-embeddings.Rout"
printf '%s\n' "$SLURM_JOB_ID" > "$W/check-${SLURM_JOB_ID}/VERIFICATION_COMPLETE"
