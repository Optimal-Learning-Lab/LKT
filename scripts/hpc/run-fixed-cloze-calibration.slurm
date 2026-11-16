#!/bin/bash -l
#SBATCH --job-name=cloze_fixed_defaults
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=12:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
module purge
module load singularity/4.1.2
set -u
[[ $# -eq 3 ]]
CONFIG=$1;EXPECTED=$2;ACTION=$3
[[ "$ACTION" == control || "$ACTION" == finish ]]
mapfile -d '' -t C < <(python3 - "$CONFIG" "$EXPECTED" <<'PY'
import hashlib,json,sys
from pathlib import Path
p=Path(sys.argv[1]);assert hashlib.sha256(p.read_bytes()).hexdigest()==sys.argv[2]
c=json.loads(p.read_text());assert c['storage_bytes_limit']==10000000000 and c['workers']==1
assert c['new_initial_nonlinear_fits']==1 and c['ground_truth_accuracy_refits']==0
for e in c['files']:
 assert hashlib.sha256(Path(e['path']).read_bytes()).hexdigest()==e['sha256'],e['path']
for key in ['root','project','data','source_accuracy','output','container','container_sha256','dependencies','release_file']:
 sys.stdout.buffer.write(str(c[key]).encode()+b'\0')
PY
)
[[ ${#C[@]} -eq 9 ]]
ROOT=${C[0]};PROJECT=${C[1]};DATA=${C[2]};SOURCE=${C[3]};OUTPUT=${C[4]}
SIF=${C[5]};DEPS=${C[7]};RELEASE=${C[8]}
[[ $(sha256sum "$SIF" | cut -d' ' -f1) == "${C[6]}" ]]
check_storage() {
 python3 - <<'PY'
import subprocess
used=sum(int(line.split()[0]) for line in subprocess.check_output(
 ['du','-sb','/home/ppavlik','/project/ppavlik'],universal_newlines=True).splitlines())
assert used+200000000<10000000000,'Insufficient headroom within combined 10 GB cap'
print('STORAGE_BYTES',used,flush=True)
PY
}
SCRATCH=$(mktemp -d "/tmp/cloze-fixed.${SLURM_JOB_ID:?}.XXXXXX")
cleanup() { [[ "$SCRATCH" == "/tmp/cloze-fixed.${SLURM_JOB_ID}."* ]] && rm -rf -- "$SCRATCH"; }
trap cleanup EXIT
mkdir -p "$SCRATCH/home" "$SCRATCH/cache" "$SCRATCH/tmp"
export SINGULARITY_CACHEDIR="$SCRATCH/cache" SINGULARITY_TMPDIR="$SCRATCH/tmp"
ENVIRONMENT="TMPDIR=$SCRATCH/tmp,R_LIBS=$DEPS,R_LIBS_USER=$DEPS,R_LIBS_SITE=,LC_ALL=C.UTF-8,LANG=C.UTF-8,LANGUAGE=C,R_TESTS=,OMP_NUM_THREADS=1,MKL_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,TORCH_VERIFY_LOAD=TRUE,LKT_RELEASE_FILE=$RELEASE,TORCH_HOME=$DEPS/torch,TORCH_INSTALL=0"
run_r() {
 singularity exec --userns --cleanenv --no-home --bind "/project/ppavlik:/project/ppavlik" \
  --bind "$SCRATCH:$SCRATCH" --pwd "$PROJECT" --env "$ENVIRONMENT" "$SIF" \
  /usr/local/lib/R/bin/Rscript --vanilla "$@"
}
check_storage
run_r "$ROOT/scripts/test-fixed-cloze-calibration.R" "$ROOT"
run_r "$ROOT/scripts/fixed-cloze-calibration.R" "$ROOT" "$PROJECT" "$DATA" "$SOURCE" "$OUTPUT" plan
if [[ "$ACTION" == finish ]]; then [[ -f "$SOURCE" ]]; fi
# Submit finish with scheduler dependencies on the control and source jobs.
# No CPU or memory allocation is held while waiting for source calibration.
run_r "$ROOT/scripts/fixed-cloze-calibration.R" "$ROOT" "$PROJECT" "$DATA" "$SOURCE" "$OUTPUT" "$ACTION"
check_storage
