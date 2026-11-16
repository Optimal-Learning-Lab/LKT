#!/bin/bash -l
#SBATCH --job-name=cloze_tracer6_train
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
[[ $# -eq 2 ]]
CONFIG=$1;EXPECTED=$2
mapfile -d '' -t C < <(python3 - "$CONFIG" "$EXPECTED" <<'PY'
import hashlib,json,sys
from pathlib import Path
p=Path(sys.argv[1]);assert hashlib.sha256(p.read_bytes()).hexdigest()==sys.argv[2]
c=json.loads(p.read_text())
assert c['storage_bytes_limit']==10000000000 and c['workers']==1
assert c['full_neural_fits']==1 and c['numeric_inputs']==6
assert c['calibration_scope']=='training_only' and not c['simulation_integration']
for e in c['files']:
 assert hashlib.sha256(Path(e['path']).read_bytes()).hexdigest()==e['sha256'],e['path']
for key in ['root','reference','control','output','container','container_sha256','dependencies','release_file']:
 sys.stdout.buffer.write(str(c[key]).encode()+b'\0')
PY
)
[[ ${#C[@]} -eq 8 ]]
ROOT=${C[0]};REFERENCE=${C[1]};CONTROL=${C[2]};OUTPUT=${C[3]}
SIF=${C[4]};DEPS=${C[6]};RELEASE=${C[7]}
[[ $(sha256sum "$SIF" | cut -d' ' -f1) == "${C[5]}" ]]
check_storage() {
 python3 - <<'PY'
import subprocess
used=sum(int(line.split()[0]) for line in subprocess.check_output(
 ['du','-sb','/home/ppavlik','/project/ppavlik'],universal_newlines=True).splitlines())
assert used+200000000<10000000000,'Insufficient headroom within combined 10 GB cap'
print('STORAGE_BYTES',used,flush=True)
PY
}
SCRATCH=$(mktemp -d "/tmp/cloze-tracer6.${SLURM_JOB_ID:?}.XXXXXX")
cleanup() { [[ "$SCRATCH" == "/tmp/cloze-tracer6.${SLURM_JOB_ID}."* ]] && rm -rf -- "$SCRATCH"; }
trap cleanup EXIT
mkdir -p "$SCRATCH/home" "$SCRATCH/cache" "$SCRATCH/tmp"
export SINGULARITY_CACHEDIR="$SCRATCH/cache" SINGULARITY_TMPDIR="$SCRATCH/tmp"
ENVIRONMENT="TMPDIR=$SCRATCH/tmp,R_LIBS=$DEPS,R_LIBS_USER=$DEPS,R_LIBS_SITE=,LC_ALL=C.UTF-8,LANG=C.UTF-8,LANGUAGE=C,R_TESTS=,OMP_NUM_THREADS=1,MKL_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,TORCH_VERIFY_LOAD=TRUE,LKT_RELEASE_FILE=$RELEASE,TORCH_HOME=$DEPS/torch,TORCH_INSTALL=0,SLURM_JOB_ID=${SLURM_JOB_ID:?}"
run_r() {
 singularity exec --userns --cleanenv --no-home --bind "/project/ppavlik:/project/ppavlik" \
  --bind "$SCRATCH:$SCRATCH" --pwd "$ROOT" --env "$ENVIRONMENT" "$SIF" \
  /usr/local/lib/R/bin/Rscript --vanilla "$@"
}
check_storage
run_r "$ROOT/scripts/test-gru-tracer-training-only.R" "$ROOT"
for ACTION in plan prepare smoke fit report; do
 check_storage
 run_r "$ROOT/scripts/gru-tracer-training-only.R" "$ROOT" "$REFERENCE" "$CONTROL" "$OUTPUT" "$ACTION"
done
python3 -B "$ROOT/scripts/verify-anonymous-gru-tracer.py" "$ROOT" "$OUTPUT"
check_storage
python3 - "$CONFIG" <<'PY'
import hashlib,json,sys
from pathlib import Path
c=json.loads(Path(sys.argv[1]).read_text())
for e in c['files']:
 assert hashlib.sha256(Path(e['path']).read_bytes()).hexdigest()==e['sha256'],e['path']
p=Path(c['output'])
assert json.loads((p/'completed.json').read_text())['passed']
assert json.loads((p/'reports/independent-audit.json').read_text())['complete']
(p/'isolation-verified.json').write_text(json.dumps({'passed':True,
 'all_pinned_inputs_and_source_unchanged':True,'independent_metrics_passed':True,
 'training_only_calibration':True,'protected_jobs':c['protected_jobs']},indent=2)+'\n')
PY
