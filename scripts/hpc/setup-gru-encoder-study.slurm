#!/bin/bash -l
#SBATCH --job-name=lkt_encoder_setup
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=24G
#SBATCH --time=02:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
# Reuse only the verified container and dependencies; install the new maintained
# package archive in this study's explicit deployment library. Never load old LKT.
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
module purge
module load singularity/4.1.2
set -u
previous=/home/ppavlik/LKT-gru-inference-20261102
sha256sum -c bundle.sha256
sha256sum -c archive.sha256
(cd "$previous" && sha256sum -c container.sha256)
mkdir library-current
for package in "$previous"/library-container/*; do
  if [[ "$(basename "$package")" != LKT ]]; then cp -a "$package" library-current/; fi
done
cp "$previous/container.sha256" container.sha256
ln -s "$previous/rocker-r-4.5.3.sif" rocker-r-4.5.3.sif
container_env="LKT_STUDY_LIBRARY=/work/library-current,LKT_STUDY_REFERENCE=/work/reference,R_LIBS_USER=/work/library-current,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1,LKT_REQUIRE_TORCH_TESTS=true"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  R CMD INSTALL --library=/work/library-current /work/LKT_2.0.0.9011.tar.gz
# Native package regressions verify every supported backend, plus encoder learning,
# saved models and retained runtimes, in the Linux deployment that will fit the grid.
for test in tests/*.R; do
  singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
    Rscript --vanilla "/work/$test"
done
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/run-gru-encoder-study.R /work /work/results /work/release.json plan development 0 8
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/run-gru-encoder-study.R /work /work/results /work/release.json smoke development 0 8
test -f results/smoke/verification.txt
touch SETUP_COMPLETE
