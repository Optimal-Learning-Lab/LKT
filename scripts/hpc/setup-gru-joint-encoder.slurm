#!/bin/bash -l
#SBATCH --job-name=lkt_joint_setup
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=16G
#SBATCH --time=01:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
module purge
module load singularity/4.1.2
set -u
sha256sum -c bundle.sha256
sha256sum -c archive.sha256
dependencies=/home/ppavlik/LKT-encoder-study-20261103
(cd "$dependencies" && sha256sum -c container.sha256)
# Explicitly install this validation candidate; never copy an older LKT package.
mkdir candidate-library
for package in "$dependencies"/library-current/*; do
  if [[ "$(basename "$package")" != LKT ]]; then cp -a "$package" candidate-library/; fi
done
cp "$dependencies/container.sha256" container.sha256
ln -s "$dependencies/rocker-r-4.5.3.sif" rocker-r-4.5.3.sif
archive=$(python3 -c 'import json; print(json.load(open("candidate/candidate.json"))["source_archive"])')
container_env="LKT_RELEASE_FILE=/work/workflows/simulation/shared/lkt-release.env,R_LIBS_USER=/work/candidate-library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1,LKT_REQUIRE_TORCH_TESTS=true"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  R CMD INSTALL --library=/work/candidate-library "/work/candidate/$archive"
for test in gru-configuration gru-encoders gru-time-features gru-retrieval gru-retrieval-filter gru-ktst-ideas gru-input-fusion gru-joint-encoder; do
  singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
    Rscript --vanilla "/work/tests/$test.R"
done
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/compare-gru-joint-encoder.R /work /work/results verify development /work/candidate/candidate.json /work/candidate-library
touch SETUP_COMPLETE
