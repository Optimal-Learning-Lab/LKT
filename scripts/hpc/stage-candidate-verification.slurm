#!/bin/bash -l
#SBATCH --job-name=lkt_candidate_stage
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --time=00:05:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
module purge
module load singularity/4.1.2
set -u
[[ $# -eq 4 ]]
CONFIG=$1;EXPECTED=$2;CHECK=$3;OUTPUT=$4
mapfile -d '' -t C < <(python3 - "$CONFIG" "$EXPECTED" <<'PY'
import hashlib,json,sys
from pathlib import Path
p=Path(sys.argv[1]);assert hashlib.sha256(p.read_bytes()).hexdigest()==sys.argv[2]
c=json.loads(p.read_text());assert c['storage_bytes_limit']==10000000000
for k in ['workspace','container','container_sha256','dependencies','archive','archive_sha256']:
 sys.stdout.buffer.write(str(c[k]).encode()+b'\0')
PY
)
W=${C[0]};SIF=${C[1]};DEPS=${C[3]}
[[ ${#C[@]} -eq 6 && "$CHECK" == "$W/check-"* && "$OUTPUT" == "$W/candidate-verification-"* ]]
[[ $(sha256sum "$SIF" | cut -d' ' -f1) == "${C[2]}" ]]
[[ $(sha256sum "${C[4]}" | cut -d' ' -f1) == "${C[5]}" ]]
[[ -f "$CHECK/LKT.Rcheck/LKT/DESCRIPTION" ]]
# Copy an explicitly selected, installed test artifact with full file hashes.
# This is a temporary verification deployment, never a release promotion.
singularity exec --userns --cleanenv --no-home --bind "/project/ppavlik:/project/ppavlik" --pwd "$W" \
 --env "R_LIBS=$DEPS,R_LIBS_USER=$DEPS,R_LIBS_SITE=,LC_ALL=C.UTF-8,LANG=C.UTF-8,R_TESTS=" \
 "$SIF" /usr/local/lib/R/bin/Rscript --vanilla scripts/prepare-adaptive-deployment.R \
 "$W" "$CHECK/LKT.Rcheck" "${C[4]}" "$OUTPUT" "$W"
