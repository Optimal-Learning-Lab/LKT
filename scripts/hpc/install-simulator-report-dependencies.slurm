#!/bin/bash -l
#SBATCH --job-name=simulator_report_deps
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=4G
#SBATCH --time=00:20:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
module purge
module load singularity/4.1.2
set -u
[[ $# -eq 2 ]]
CONFIG=$1;EXPECTED=$2
mapfile -d '' -t C < <(python3 - "$CONFIG" "$EXPECTED" <<'PY'
import hashlib,json,subprocess,sys
from pathlib import Path
p=Path(sys.argv[1]);assert hashlib.sha256(p.read_bytes()).hexdigest()==sys.argv[2]
c=json.loads(p.read_text());assert c['storage_bytes_limit']==10000000000
used=sum(int(line.split()[0]) for line in subprocess.check_output(['du','-sb','/home/ppavlik','/project/ppavlik'],universal_newlines=True).splitlines())
assert used<9000000000
for k in ['workspace','container','container_sha256','dependencies']:
 sys.stdout.buffer.write(str(c[k]).encode()+b'\0')
sys.stdout.buffer.write(str(c['report_package_repository']).encode()+b'\0')
PY
)
[[ ${#C[@]} -eq 5 ]]
W=${C[0]};SIF=${C[1]};DEPS=${C[3]}
[[ $(sha256sum "$SIF" | cut -d' ' -f1) == "${C[2]}" ]]
SCRATCH=$(mktemp -d "/tmp/simulator-report-deps.${SLURM_JOB_ID:?}.XXXXXX")
cleanup() { [[ "$SCRATCH" == "/tmp/simulator-report-deps.${SLURM_JOB_ID}."* ]] && rm -rf -- "$SCRATCH"; }
trap cleanup EXIT
mkdir -p "$SCRATCH/cache" "$SCRATCH/tmp"
export SINGULARITY_CACHEDIR="$SCRATCH/cache" SINGULARITY_TMPDIR="$SCRATCH/tmp"
singularity exec --userns --cleanenv --no-home --bind "/project/ppavlik:/project/ppavlik" \
 --bind "$SCRATCH:$SCRATCH" --pwd "$W" \
 --env "TMPDIR=$SCRATCH/tmp,R_LIBS=$DEPS,R_LIBS_USER=$DEPS,R_LIBS_SITE=,LC_ALL=C.UTF-8,LANG=C.UTF-8,OMP_NUM_THREADS=1,MKL_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1" \
 "$SIF" /usr/local/lib/R/bin/Rscript --vanilla scripts/hpc/install-simulator-report-dependencies.R \
 "$DEPS" "$W/report-dependencies-${SLURM_JOB_ID}.json" "${C[4]}"
python3 - <<'PY'
import subprocess
used=sum(int(line.split()[0]) for line in subprocess.check_output(['du','-sb','/home/ppavlik','/project/ppavlik'],universal_newlines=True).splitlines())
assert used<10000000000
print('Combined persistent bytes:',used)
PY
