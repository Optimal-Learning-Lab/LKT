#!/bin/bash -l
#SBATCH --job-name=gru_simulator_pilot
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=04:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
module purge
module load singularity/4.1.2
set -u
[[ $# -eq 2 || $# -eq 3 ]]
CONFIG=$1;EXPECTED=$2;MODE=${3:-run}
[[ "$MODE" == run || "$MODE" == verify || "$MODE" == report || "$MODE" == weekly || "$MODE" == weekly-report ]]
mapfile -d '' -t C < <(python3 - "$CONFIG" "$EXPECTED" "$MODE" <<'PY'
import hashlib,json,subprocess,sys
from pathlib import Path
p=Path(sys.argv[1]);assert hashlib.sha256(p.read_bytes()).hexdigest()==sys.argv[2]
c=json.loads(p.read_text());assert c['storage_bytes_limit']==10000000000 and c['workers']==1
for key,hashkey in [('source_calibration','source_calibration_sha256'),('tracer_fit','tracer_fit_sha256'),('follow_up_config','follow_up_config_sha256')]:
 assert hashlib.sha256(Path(c[key]).read_bytes()).hexdigest()==c[hashkey]
if sys.argv[3]!='verify':
 check=Path(c['verified_check_directory'])
 assert (check/'VERIFICATION_COMPLETE').is_file()
 assert 'ERROR' not in (check/'LKT.Rcheck/00check.log').read_text()
used=sum(int(line.split()[0]) for line in subprocess.check_output(['du','-sb','/home/ppavlik','/project/ppavlik'],universal_newlines=True).splitlines())
assert used<c['storage_bytes_limit'], 'Combined home/project storage limit exceeded'
for k in ['workspace','container','container_sha256','dependencies','release_file','source_calibration','tracer_fit','follow_up_config']:
 sys.stdout.buffer.write(str(c[k]).encode()+b'\0')
resume=c.get('resume_run_directory','')
if sys.argv[3] in ['report','weekly','weekly-report']:
 assert Path(resume).parent==Path(c['workspace']) and (Path(resume)/'pilot/protocol.rds').is_file()
sys.stdout.buffer.write(str(resume).encode()+b'\0')
weekly=c.get('weekly_output_directory','')
if sys.argv[3]=='weekly':
 assert Path(weekly).parent==Path(resume)/'pilot' and not Path(weekly).exists()
elif sys.argv[3]=='weekly-report':
 assert Path(weekly).parent==Path(resume)/'pilot' and (Path(weekly)/'batched/result.rds').is_file()
sys.stdout.buffer.write(str(weekly).encode()+b'\0')
PY
)
[[ ${#C[@]} -eq 10 ]]
W=${C[0]};SIF=${C[1]};DEPS=${C[3]}
[[ $(sha256sum "$SIF" | cut -d' ' -f1) == "${C[2]}" ]]
SCRATCH=$(mktemp -d "/tmp/gru-simulator.${SLURM_JOB_ID:?}.XXXXXX")
cleanup() { [[ "$SCRATCH" == "/tmp/gru-simulator.${SLURM_JOB_ID}."* ]] && rm -rf -- "$SCRATCH"; }
trap cleanup EXIT
mkdir -p "$SCRATCH/home" "$SCRATCH/cache" "$SCRATCH/tmp"
export SINGULARITY_CACHEDIR="$SCRATCH/cache" SINGULARITY_TMPDIR="$SCRATCH/tmp"
ENVIRONMENT="HOME=$SCRATCH/home,TMPDIR=$SCRATCH/tmp,R_LIBS=$DEPS,R_LIBS_USER=$DEPS,R_LIBS_SITE=,LC_ALL=C.UTF-8,LANG=C.UTF-8,LANGUAGE=C,R_TESTS=,OMP_NUM_THREADS=1,MKL_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,TORCH_VERIFY_LOAD=TRUE,LKT_RELEASE_FILE=${C[4]},TORCH_HOME=$DEPS/torch,TORCH_INSTALL=0"
run_r() {
 singularity exec --userns --cleanenv --no-home --bind "/project/ppavlik:/project/ppavlik" \
  --bind "$SCRATCH:$SCRATCH" --pwd "$W" --env "$ENVIRONMENT" "$SIF" \
  /usr/local/lib/R/bin/Rscript --vanilla "$@"
}
OUTPUT="$W/${MODE}-${SLURM_JOB_ID}"
WEEKLY_OUTPUT="$OUTPUT/pilot/weekly-follow-up"
if [[ "$MODE" == weekly || "$MODE" == weekly-report ]]; then
 OUTPUT=${C[8]};WEEKLY_OUTPUT=${C[9]}
 BUNDLE="$OUTPUT/calibration/ppes_cloze_tracer_bundle.rds"
 if [[ "$MODE" == weekly-report ]]; then
  run_r workflows/simulation/shared/runners/run_batched_weekly_test.R "${C[4]}" "$BUNDLE" "$OUTPUT/pilot" "${C[7]}" "$WEEKLY_OUTPUT" report-only
 else
  run_r workflows/simulation/shared/runners/run_batched_weekly_test.R "${C[4]}" "$BUNDLE" "$OUTPUT/pilot" "${C[7]}" "$WEEKLY_OUTPUT"
 fi
elif [[ "$MODE" == report ]]; then
 OUTPUT=${C[8]}
 WEEKLY_OUTPUT="$OUTPUT/pilot/weekly-follow-up"
 BUNDLE="$OUTPUT/calibration/ppes_cloze_tracer_bundle.rds"
 run_r workflows/simulation/shared/runners/run_batched_pilot.R "${C[4]}" "$BUNDLE" "$OUTPUT/pilot" 5 "${C[7]}" report-only
else
run_r workflows/simulation/shared/fitting/prepare_cloze_tracer_bundle.R "${C[4]}" "${C[5]}" "${C[6]}" "$OUTPUT/calibration"
BUNDLE="$OUTPUT/calibration/ppes_cloze_tracer_bundle.rds"
run_r workflows/simulation/shared/tests/verify_cloze_tracer_adapter.R "${C[4]}" "$BUNDLE" "$OUTPUT/adapter-verification"
if [[ "$MODE" == verify ]]; then
 echo 'PASS fitted-model adapter verification only; no simulation or release promotion'
 exit 0
fi
run_r workflows/simulation/shared/runners/run_batched_pilot.R "${C[4]}" "$BUNDLE" "$OUTPUT/pilot" 5 "${C[7]}"
fi
python3 - "$OUTPUT" "$WEEKLY_OUTPUT" <<'PY'
import json,subprocess,sys
from pathlib import Path
p=Path(sys.argv[1])
for name in ['calibration','adapter-verification','pilot']:
 assert json.loads((p/name/'completed.json').read_text())['passed']
assert json.loads((Path(sys.argv[2])/'completed.json').read_text())['passed']
used=sum(int(line.split()[0]) for line in subprocess.check_output(['du','-sb','/home/ppavlik','/project/ppavlik'],universal_newlines=True).splitlines())
assert used<10000000000
(p/'completed.json').write_text(json.dumps({'passed':True,'combined_storage_bytes':used,'scientific_refits':False,'students':5,'weekly_conditions':1,'weekly_output':sys.argv[2]},indent=2)+'\n')
PY
