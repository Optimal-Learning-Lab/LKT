#!/bin/bash -l
#SBATCH --job-name=cloze_source_reuse
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --time=00:15:00
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
assert c['storage_bytes_limit']==10000000000 and c['scientific_fits']==0
for e in c['files']:
 assert hashlib.sha256(Path(e['path']).read_bytes()).hexdigest()==e['sha256'],e['path']
for key in ['root','project','reference','control','workspace','calibration_store',
 'source_record','source_protocol','container','container_sha256','dependencies','release_file']:
 sys.stdout.buffer.write(str(c[key]).encode()+b'\0')
PY
)
[[ ${#C[@]} -eq 12 ]]
ROOT=${C[0]};PROJECT=${C[1]};REFERENCE=${C[2]};CONTROL=${C[3]};W=${C[4]}
STORE=${C[5]};RECORD=${C[6]};PROTOCOL=${C[7]};SIF=${C[8]};DEPS=${C[10]};RELEASE=${C[11]}
[[ $(sha256sum "$SIF" | cut -d' ' -f1) == "${C[9]}" ]]
SCRATCH=$(mktemp -d "/tmp/cloze-source-reuse.${SLURM_JOB_ID:?}.XXXXXX")
cleanup() { [[ "$SCRATCH" == "/tmp/cloze-source-reuse.${SLURM_JOB_ID}."* ]] && rm -rf -- "$SCRATCH"; }
trap cleanup EXIT
mkdir -p "$SCRATCH/cache" "$SCRATCH/tmp"
export SINGULARITY_CACHEDIR="$SCRATCH/cache" SINGULARITY_TMPDIR="$SCRATCH/tmp"
ENVIRONMENT="TMPDIR=$SCRATCH/tmp,R_LIBS=$DEPS,R_LIBS_USER=$DEPS,R_LIBS_SITE=,LC_ALL=C.UTF-8,LANG=C.UTF-8,LANGUAGE=C,R_TESTS=,OMP_NUM_THREADS=1,MKL_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,TORCH_VERIFY_LOAD=TRUE,LKT_RELEASE_FILE=$RELEASE,TORCH_HOME=$DEPS/torch,TORCH_INSTALL=0,SLURM_JOB_ID=${SLURM_JOB_ID:?}"
run_r() {
 singularity exec --userns --cleanenv --no-home --bind "/project/ppavlik:/project/ppavlik" \
  --bind "$SCRATCH:$SCRATCH" --pwd "$PROJECT" --env "$ENVIRONMENT" "$SIF" \
  /usr/local/lib/R/bin/Rscript --vanilla "$@"
}
run_r "$ROOT/scripts/test-cloze-source-calibration-cache.R" "$ROOT"
run_r "$ROOT/scripts/cloze-source-calibration.R" "$ROOT" "$REFERENCE" "$STORE" import "$RECORD" "$PROTOCOL"
# A repeated ensure must also succeed when both fitting entry points are forbidden.
run_r -e 'root<-commandArgs(TRUE)[1];source(file.path(root,"scripts/load-current-lkt.R"));trace("fit_lkt_config",where=asNamespace("LKT"),tracer=quote(stop("FORBIDDEN FIT")),print=FALSE);trace("LKT",where=asNamespace("LKT"),tracer=quote(stop("FORBIDDEN FIT")),print=FALSE);source(file.path(root,"scripts/cloze-source-calibration.R"))' \
 "$ROOT" "$REFERENCE" "$STORE" ensure
for RUN in a b; do
 run_r "$ROOT/scripts/verify-cloze-source-calibration-reuse.R" \
  "$ROOT" "$PROJECT" "$REFERENCE" "$CONTROL" "$W/LKT-outputs/cache-reuse-$RUN" prepare --training-only "$STORE"
 echo "PASS real-data prepare $RUN with all fitting entry points forbidden"
done
python3 - "$CONFIG" <<'PY'
import hashlib,json,subprocess,sys
from pathlib import Path
c=json.loads(Path(sys.argv[1]).read_text());w=Path(c['workspace'])
for e in c['files']:
 assert hashlib.sha256(Path(e['path']).read_bytes()).hexdigest()==e['sha256'],e['path']
checks=[json.loads((w/'LKT-outputs'/('cache-reuse-'+name)/'prepared.json').read_text()) for name in ['a','b']]
assert all(x['passed'] and x['training_only'] and x['calibration_rows']==35125 for x in checks)
assert checks[0]['source_calibration']==checks[1]['source_calibration']
used=sum(int(s.split()[0]) for s in subprocess.check_output(['du','-sb','/home/ppavlik','/project/ppavlik'],universal_newlines=True).splitlines())
assert used<c['storage_bytes_limit']
(w/'completed.json').write_text(json.dumps({'passed':True,'scientific_fits':0,
 'imported_existing_converged_calibration':True,'cache_hit_with_fit_functions_forbidden':True,
 'two_neural_preparations_with_fit_functions_forbidden':True,'inputs_and_running_job_source_unchanged':True,
 'calibration_store':c['calibration_store'],'storage_bytes':used},indent=2)+'\n')
print('PASS existing calibration published; repeated cache lookup and both new GRU preparations performed zero fitting')
PY
