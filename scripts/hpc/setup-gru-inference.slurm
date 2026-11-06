#!/bin/bash -l
#SBATCH --job-name=lkt_inference_setup
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=24G
#SBATCH --time=02:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
# Prepare a separate library using the already verified CPU container dependencies.
# The previous experiment is read only; the new LKT archive is installed exclusively
# in this workspace and tested before the dependent training job can start.
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
module purge
module load singularity/4.1.2
set -u
previous=/home/ppavlik/LKT-gru-grid-20261101
test -d "$previous/library-container"
test ! -e library-container
mkdir library-container
for package in "$previous"/library-container/*; do
  if [[ "$(basename "$package")" != LKT ]]; then
    cp -a "$package" library-container/
  fi
done
cp -a "$previous/tools" tools
cp "$previous/container.sha256" container.sha256
ln -s "$previous/rocker-r-4.5.3.sif" rocker-r-4.5.3.sif
sha256sum -c container.sha256
container_sha=$(cut -d ' ' -f 1 container.sha256)
container_env="LKT_GRU_LIBRARY=/work/library-container,R_LIBS_USER=/work/library-container,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1,LKT_REQUIRE_TORCH_TESTS=true,LKT_GRU_CONTAINER_SHA=$container_sha"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/source/scripts/hpc/setup-gru-environment.R /work
for test in gru-configuration gru-features gru-regression gru-inference; do
  singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
    Rscript --vanilla "/work/source/tests/$test.R"
done
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/source/scripts/compare-gru-inference.R /work/source /work/inference-results plan 12 all
touch INFERENCE_SETUP_COMPLETE
