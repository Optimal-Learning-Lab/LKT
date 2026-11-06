#!/bin/bash -l
#SBATCH --job-name=lkt_ednet_author_setup
#SBATCH --partition=acomputeq
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=24G
#SBATCH --time=02:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err
set -eo pipefail
cd "$SLURM_SUBMIT_DIR"
module purge
module load singularity/4.1.2
set -u
sha256sum -c bundle.sha256
source deployment.env
dependencies=/home/ppavlik/LKT-encoder-study-20261103
(cd "$dependencies" && sha256sum -c container.sha256)
mkdir -p "$lkt_library"
for package in "$dependencies"/library-current/*; do
  if [[ "$(basename "$package")" != LKT ]]; then cp -a "$package" "$lkt_library/"; fi
done
cp "$dependencies/container.sha256" container.sha256
ln -s "$dependencies/rocker-r-4.5.3.sif" rocker-r-4.5.3.sif
container_env="LKT_RELEASE_FILE=/work/workflows/simulation/shared/lkt-release.env,R_LIBS_USER=/work/$lkt_library,OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  R CMD INSTALL --library="/work/$lkt_library" "/work/$lkt_source_archive"
singularity exec --userns --cleanenv --bind "$PWD:/work" --pwd /work --env "$container_env" "$PWD/rocker-r-4.5.3.sif" \
  Rscript --vanilla /work/scripts/run-gru-ednet-author-protocol.R /work /work/study verify
touch SETUP_COMPLETE
