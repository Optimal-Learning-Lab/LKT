#!/bin/bash -l
#SBATCH --job-name=lkt_checked_deploy
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=1G
#SBATCH --time=00:05:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -euo pipefail
[[ $# -eq 3 ]]
CONFIG=$1;EXPECTED=$2;CHECK=$3
python3 - "$CONFIG" "$EXPECTED" <<'PY'
import hashlib,json,shutil,sys
from pathlib import Path
p=Path(sys.argv[1]);assert hashlib.sha256(p.read_bytes()).hexdigest()==sys.argv[2]
c=json.loads(p.read_text());root=Path(c['root']);archive=Path(c['archive'])
assert root==Path(c['workspace']) and hashlib.sha256(archive.read_bytes()).hexdigest()==c['archive_sha256']
dest=root/archive.name
if dest.exists():
 assert hashlib.sha256(dest.read_bytes()).hexdigest()==c['archive_sha256']
else:
 shutil.copyfile(str(archive),str(dest))
PY
python3 scripts/hpc/deploy-verified-lkt.py "$CONFIG" "$CHECK"
