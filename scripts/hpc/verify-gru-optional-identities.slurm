#!/bin/bash -l
#SBATCH --job-name=lkt_optional_ids
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --time=00:05:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
module purge
module load singularity/4.1.2
set -u
[[ $# -eq 3 ]]
CONFIG=$1;EXPECTED=$2;RELEASE=$3
mapfile -d '' -t C < <(python3 - "$CONFIG" "$EXPECTED" <<'PY'
import hashlib,json,sys
from pathlib import Path
p=Path(sys.argv[1]);assert hashlib.sha256(p.read_bytes()).hexdigest()==sys.argv[2]
c=json.loads(p.read_text());assert c['storage_bytes_limit']==10000000000
for k in ['workspace','container','container_sha256','dependencies']:
 sys.stdout.buffer.write(str(c[k]).encode()+b'\0')
PY
)
[[ ${#C[@]} -eq 4 ]]
W=${C[0]};SIF=${C[1]};DEPS=${C[3]}
[[ $(sha256sum "$SIF" | cut -d' ' -f1) == "${C[2]}" ]]
singularity exec --userns --cleanenv --no-home --bind "/project/ppavlik:/project/ppavlik" --pwd "$W/tests" \
 --env "R_LIBS=$DEPS,R_LIBS_USER=$DEPS,R_LIBS_SITE=,LC_ALL=C.UTF-8,LANG=C.UTF-8,R_TESTS=,TORCH_HOME=$DEPS/torch,TORCH_INSTALL=0,LKT_REQUIRE_TORCH_TESTS=true,OMP_NUM_THREADS=1,MKL_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1" \
 "$SIF" /usr/local/lib/R/bin/Rscript --vanilla - "$RELEASE" <<'RS'
a <- commandArgs(TRUE);source(file.path(dirname(a[1]),'lkt_release.R'));lkt_use_current(a[1])
source('gru-optional-identities.R')
RS
printf '{"passed":true,"job":%s}\n' "$SLURM_JOB_ID" > "$W/optional-identities-${SLURM_JOB_ID}.json"
