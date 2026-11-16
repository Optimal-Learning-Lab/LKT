#!/bin/bash -l
#SBATCH --job-name=lkt_error_setup
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=01:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
sha256sum -c bundle.sha256
module purge
module load singularity/4.1.2
set -u
dependencies=$(python3 -c 'import json; print(json.load(open("remote-inputs.json"))["dependencies_directory"])')
(cd "$dependencies" && sha256sum -c container.sha256)
python3 scripts/prepare-gru-simplified-remote.py
mkdir candidate-library
# Reuse dependency packages read-only; LKT itself is installed only in the
# explicit candidate library and every fit validates that exact installation.
archive=$(python3 -c 'import json; print(json.load(open("candidate/candidate-template.json"))["source_archive"])')
runtime="R_LIBS_USER=/work/candidate-library:/dependencies,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1,LKT_REQUIRE_TORCH_TESTS=true"
container="$dependencies/rocker-r-4.5.3.sif"
singularity exec --userns --cleanenv --bind "$PWD:/work" --bind "$dependencies/library-current:/dependencies:ro" --pwd /work/candidate --env "$runtime" "$container" \
  R CMD build --no-build-vignettes --no-manual /work/package-source/LKT
python3 - <<'PY'
import hashlib,json
from pathlib import Path
base=Path('candidate');meta=json.loads((base/'candidate-template.json').read_text())
meta['source_sha256']=hashlib.sha256((base/meta['source_archive']).read_bytes()).hexdigest()
meta['remote_existing_data_reused']=True
(base/'candidate.json').write_text(json.dumps(meta,indent=2)+'\n')
PY
singularity exec --userns --cleanenv --bind "$PWD:/work" --bind "$dependencies/library-current:/dependencies:ro" --pwd /work --env "$runtime" "$container" \
  R CMD INSTALL --library=/work/candidate-library "/work/candidate/$archive"
singularity exec --userns --cleanenv --bind "$PWD:/work" --bind "$dependencies/library-current:/dependencies:ro" --pwd /work --env "$runtime" "$container" \
  Rscript --vanilla -e 'library(LKT,lib.loc="/work/candidate-library"); stopifnot(normalizePath(find.package("LKT"))=="/work/candidate-library/LKT"); setwd("/work/package-source/LKT/tests"); source("gru-error-rate.R")'
reference=$(python3 -c 'import json; print(json.load(open("remote-inputs.json"))["reference_directory"])')
for action in plan smoke; do
  args=(/reference)
  singularity exec --userns --cleanenv --bind "$PWD:/work" --bind "$dependencies/library-current:/dependencies:ro" --bind "$reference:/reference:ro" --pwd /work --env "$runtime" "$container" \
    Rscript --vanilla /work/scripts/compare-gru-output-error-rate.R /work /work/results "$action" 0 /work/candidate/candidate.json /work/candidate-library "${args[@]}"
done
touch SETUP_COMPLETE
