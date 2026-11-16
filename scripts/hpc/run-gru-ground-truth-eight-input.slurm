#!/bin/bash -l
#SBATCH --job-name=cloze_gt8_gru
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
[[ $# -eq 2 || $# -eq 3 ]]
CONFIG=$1;EXPECTED=$2
FIXED_CALIBRATION=${3:-}
mapfile -d '' -t C < <(python3 - "$CONFIG" "$EXPECTED" "$FIXED_CALIBRATION" <<'PY'
import hashlib,json,sys
from pathlib import Path
p=Path(sys.argv[1]);assert hashlib.sha256(p.read_bytes()).hexdigest()==sys.argv[2]
c=json.loads(p.read_text());assert c['storage_bytes_limit']==10000000000
assert c['workers']==1 and c['full_neural_fits']==1 and not c['simulation_integration']
if c.get('calibration_scope')=='training_only':
 assert sys.argv[3]=='--training-only'
 assert c.get('calibration_store'),'Training-only jobs must declare a shared calibration store'
for e in c['files']:
 assert hashlib.sha256(Path(e['path']).read_bytes()).hexdigest()==e['sha256'],e['path']
for key in ['root','project','reference','control','output','container','container_sha256','dependencies','release_file']:
 sys.stdout.buffer.write(str(c[key]).encode()+b'\0')
sys.stdout.buffer.write(str(c.get('calibration_store','')).encode()+b'\0')
PY
)
[[ ${#C[@]} -eq 10 ]]
ROOT=${C[0]};PROJECT=${C[1]};REFERENCE=${C[2]};CONTROL=${C[3]};OUTPUT=${C[4]}
SIF=${C[5]};DEPS=${C[7]};RELEASE=${C[8]}
CALIBRATION_STORE=${C[9]}
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
SCRATCH=$(mktemp -d "/tmp/cloze-gt8.${SLURM_JOB_ID:?}.XXXXXX")
cleanup() { [[ "$SCRATCH" == "/tmp/cloze-gt8.${SLURM_JOB_ID}."* ]] && rm -rf -- "$SCRATCH"; }
trap cleanup EXIT
mkdir -p "$SCRATCH/home" "$SCRATCH/cache" "$SCRATCH/tmp"
export SINGULARITY_CACHEDIR="$SCRATCH/cache" SINGULARITY_TMPDIR="$SCRATCH/tmp"
ENVIRONMENT="TMPDIR=$SCRATCH/tmp,R_LIBS=$DEPS,R_LIBS_USER=$DEPS,R_LIBS_SITE=,LC_ALL=C.UTF-8,LANG=C.UTF-8,LANGUAGE=C,R_TESTS=,OMP_NUM_THREADS=1,MKL_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,TORCH_VERIFY_LOAD=TRUE,LKT_RELEASE_FILE=$RELEASE,TORCH_HOME=$DEPS/torch,TORCH_INSTALL=0,SLURM_JOB_ID=${SLURM_JOB_ID:?}"
run_r() {
 singularity exec --userns --cleanenv --no-home --bind "/project/ppavlik:/project/ppavlik" \
  --bind "$SCRATCH:$SCRATCH" --pwd "$PROJECT" --env "$ENVIRONMENT" "$SIF" \
  /usr/local/lib/R/bin/Rscript --vanilla "$@"
}
check_storage
run_r "$ROOT/scripts/test-gru-ground-truth-eight-input.R" "$ROOT"
if [[ "$FIXED_CALIBRATION" == --training-only ]]; then
 run_r "$ROOT/scripts/test-cloze-source-calibration-cache.R" "$ROOT"
 ACTIONS=(plan calibrate prepare smoke fit report)
else
 ACTIONS=(plan prepare smoke fit report)
fi
for ACTION in "${ACTIONS[@]}"; do
 check_storage
 EXTRA=()
 if [[ -n "$FIXED_CALIBRATION" ]]; then EXTRA=("$FIXED_CALIBRATION"); fi
 if [[ "$FIXED_CALIBRATION" == --training-only ]]; then EXTRA+=("$CALIBRATION_STORE"); fi
 run_r "$ROOT/scripts/gru-ground-truth-eight-input.R" \
  "$ROOT" "$PROJECT" "$REFERENCE" "$CONTROL" "$OUTPUT" "$ACTION" "${EXTRA[@]}"
done
check_storage
python3 - "$CONFIG" <<'PY'
import hashlib,json,sys
from pathlib import Path
c=json.loads(Path(sys.argv[1]).read_text())
for e in c['files']:
 assert hashlib.sha256(Path(e['path']).read_bytes()).hexdigest()==e['sha256'],e['path']
p=Path(c['output'])
assert json.loads((p/'completed.json').read_text())['passed']
(p/'isolation-verified.json').write_text(json.dumps({'passed':True,
 'input_artifacts_unchanged':True,'comparison_source_unchanged':True,
 'comparison_job_id':c['comparison_job_id'],'no_simulation_integration':True},indent=2)+'\n')
PY
