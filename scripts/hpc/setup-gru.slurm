#!/bin/bash -l
#SBATCH --job-name=lkt_gru_setup
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=24G
#SBATCH --time=02:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
# Allocate setup resources separately from the training job. Any failed installation,
# regression test, or preflight stops setup and prevents its dependent grid job from
# running.
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
# Initialize the site container module before enabling nounset. The site module
# functions read optional unset variables, while later script steps use stricter
# variable checks.
module purge
module load singularity/4.1.2
set -u
export SINGULARITY_CACHEDIR="$PWD/container-cache"
export SINGULARITY_TMPDIR="$PWD/container-tmp"
mkdir -p "$SINGULARITY_CACHEDIR" "$SINGULARITY_TMPDIR"
# Use a versioned R userspace that satisfies Torch's R and glibc requirements. The same
# read-only image will be used for setup and training.
image="$PWD/rocker-r-4.5.3.sif"
if [[ ! -f "$image" ]]; then
  singularity pull "$image" docker://rocker/r-ver:4.5.3
fi
# Record the imported image checksum once and verify it on later runs. A changed image
# must not silently change the execution environment.
if [[ -f container.sha256 ]]; then
  sha256sum -c container.sha256
else
  sha256sum rocker-r-4.5.3.sif > container.sha256
fi
container_sha=$(cut -d ' ' -f 1 container.sha256)
# Provide checksum-pinned CMake for building nloptr inside the private environment. This
# supplies its declared build dependency without changing host system packages.
mkdir -p tools
cmake_archive="tools/cmake-3.31.8-linux-x86_64.tar.gz"
if [[ ! -f "$cmake_archive" ]]; then
  curl -fsSL https://github.com/Kitware/CMake/releases/download/v3.31.8/cmake-3.31.8-linux-x86_64.tar.gz -o "$cmake_archive"
fi
printf '630615d8e98ac33eba7fbe472626dff5c899c85af3c024585ae109166a6909d0  %s\n' "$cmake_archive" | sha256sum -c -
if [[ ! -x tools/cmake-3.31.8-linux-x86_64/bin/cmake ]]; then
  tar -xzf "$cmake_archive" -C tools
fi
# Bind the workspace at a stable path and supply one private R library. Explicit thread
# limits let each future R worker use one allocated CPU.
container_env="LKT_GRU_LIBRARY=/work/library-container,R_LIBS_USER=/work/library-container,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1,LKT_REQUIRE_TORCH_TESTS=true,LKT_GRU_CONTAINER_SHA=$container_sha"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$image" \
  Rscript --vanilla /work/source/scripts/hpc/setup-gru-environment.R /work
# Exercise configuration handling, causal feature construction, and actual neural
# fitting before the search. Successful package loading alone would not verify these
# behaviors.
for test in gru-configuration gru-features gru-regression gru-inference; do
  singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$image" \
    Rscript --vanilla "/work/source/tests/$test.R"
done
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$image" \
  Rscript --vanilla /work/source/scripts/grid-search-gru.R /work/source /work/policy \
  /work/reference /work/results 12 all compact plan /work/reference none
# Create the success marker only after the complete grid plan has been prepared. The
# training launcher checks this marker in addition to Slurm's successful-job dependency.
touch SETUP_COMPLETE
