# Fresh Big Blue PFA R environment

This maintained setup has two explicit phases: `dependencies`, then `package`.
It installs no model or research fit, submits no further jobs and never searches
old runtime paths. Invoke it only inside Big Blue Slurm using the selected SIF.
R must be exactly 4.5.3. CPU Torch R must be exactly 0.17.0 and its bundled LibTorch
build must be exactly `2.8.0+cpu` and pass its load verification. With
`TORCH_VERIFY_LOAD=TRUE`, setup uses the public default `torch_is_installed()`
call and independently verifies a real CPU float64 tensor operation (squared sum
14 for `[1,2,3]`) and its gradient `[2,4,6]`. It does not force the cached
namespace status to be rewritten or modify Torch namespace bindings.

The [official Torch installation documentation](https://torch.mlverse.org/docs/articles/installation.html)
explains that the CPU repository packages bundle LibTorch/LibLantern.
The [v0.17.0 installer source](https://github.com/mlverse/torch/blob/v0.17.0/R/install.R)
declares LibTorch 2.8.0. This setup reads only the explicit pinned CPU repository
for Torch, installs its R dependencies from CRAN first, then installs the selected
downloaded archive with dependency resolution disabled. Before compiling CRAN
dependencies it inspects the actual CPU archive for `torch/build-version` and
`export(torch_install_path)` in `torch/NAMESPACE`, failing early on a mismatch. It never substitutes the
CRAN Torch release or attempts another runtime download if the CPU bundle fails.
The downloaded Torch archive SHA256 and repository metadata are recorded.

## Dependency-only configuration

Stage the exact current source `DESCRIPTION` as a small file, these setup sources,
and the already built SIF in the selected workspace. Write an unfolded one-record
DCF file with **exactly** the following keys (no comments or blank lines). Paths
are examples; every hash placeholder must be replaced by the actual selected file
hash. The controller rejects placeholders and mismatches before installation.
The container SHA is read from the actual verified remote `runtime/container.sha256`;
no example below asserts its value.

```text
Schema: 1
Workspace: /project/ppavlik/pfa-mechanism-study-20261110
Private-Library: /project/ppavlik/pfa-mechanism-study-20261110/runtime/r-library
Container: /project/ppavlik/pfa-mechanism-study-20261110/runtime/rocker-r-4.5.3.sif
Container-SHA256: REPLACE_WITH_ACTUAL_REMOTE_SIF_SHA256
Description: /project/ppavlik/pfa-mechanism-study-20261110/source/DESCRIPTION
Description-SHA256: REPLACE_WITH_ACTUAL_UPLOADED_DESCRIPTION_SHA256
Setup-R-Script: /project/ppavlik/pfa-mechanism-study-20261110/scripts/hpc/setup-pfa-environment.R
Setup-R-SHA256: REPLACE_WITH_ACTUAL_R_SETUP_SOURCE_SHA256
Setup-Python-Script: /project/ppavlik/pfa-mechanism-study-20261110/scripts/hpc/setup-pfa-environment.py
Setup-Python-SHA256: REPLACE_WITH_ACTUAL_PYTHON_SETUP_SOURCE_SHA256
Rscript-Executable: /usr/local/bin/Rscript
Reports-Directory: /project/ppavlik/pfa-mechanism-study-20261110/runtime/provenance
CRAN-Repository: https://cloud.r-project.org
Install-CPUs: 2
Storage-Roots: /home/ppavlik|/project/ppavlik
Storage-Bytes-Limit: 10000000000
Required-Free-Bytes: 3000000000
Locale: C.UTF-8
```

The private library must initially be absent or empty. Base/recommended R packages
come only from the verified image's `.Library`; every nonstandard requirement
belongs in the selected private library. No site, host or other user libraries are
searched. Actual Depends/Imports declarations are read from the uploaded
DESCRIPTION and supplemented with digest, jsonlite, coro and the pinned Torch
package. Their recursive R dependencies are installed normally from CRAN.
Requested version constraints are checked after installation. Source packages
compile in at most `Install-CPUs` parallel package processes, with one compiler
job each; Slurm must allocate at least that many CPUs.

Stage `logs/` before submitting; Slurm opens its output before the script runs.
For the example workspace, the explicit submission command after review is:

```bash
cd /project/ppavlik/pfa-mechanism-study-20261110
sbatch scripts/hpc/setup-pfa-environment.slurm \
  "$PWD/scripts/hpc/setup-pfa-environment.py" \
  "$PWD/runtime/dependencies.dcf" dependencies
```

This is a command to review, not a submission performed by writing this document.
The wrapper requests two CPUs, 16 GB memory and two hours in `acomputeq`, loading
`singularity/4.1.2`. The Python controller supports the actual cluster Python 3.6.8 API. The controller
runs `Rscript --vanilla` inside the explicit
verified SIF using `--cleanenv --no-home`. It binds the workspace to its own
absolute path and a unique node-local `/tmp/pfa-setup-JOBID-*` directory to its own
path; no old workspace alias or library is used.

CMake 3.31.8 is downloaded into node-local scratch from its official release,
verified against SHA256
`630615d8e98ac33eba7fbe472626dff5c899c85af3c024585ae109166a6909d0`,
and used from that temporary extraction. Downloads, compiler products, R temporary
files, the temporary HOME, Singularity caches and CMake are removed on exit.
The private installed library and small provenance records remain; no backup,
download archive, compiled temporary tree or persistent cache is retained.

`Storage-Roots` must be distinct, nonoverlapping absolute directories and include
the workspace. `du -sx -B1` sums their allocated bytes before installation, every
15 seconds while the owned installer runs, and after completion. The declared
limit cannot exceed ten billion bytes. Setup fails before starting without the
declared free headroom. If installation exceeds the ceiling, only that setup
process group is terminated and no verified completion is reported. An interrupted
or failed private library is not eligible for package installation.

Successful dependency setup writes immutable per-job files:

```text
runtime/provenance/dependencies-JOBID.json
runtime/provenance/dependencies-JOBID.launcher.json
```

The R receipt records R/platform/locale, exact library paths and private package
versions, Rscript/SIF/setup/DESCRIPTION hashes, CMake provenance, pinned Torch
archive provenance, the installed CPU bundle's LibTorch `build-version` metadata,
and load verification plus CPU tensor/autograd results from that same explicitly
selected installation. No optional
internal version-query API or alternate version-detection path is required. The launcher
records storage totals, the exact command, exit status and successful scratch
removal. Keep the receipt SHA256 as explicit input to the package phase.

## Package installation after the new source build

Do not add a candidate until all intended maintained modules are integrated and
the new package archive/version/manifest is frozen. Copy the dependency-phase DCF
and add **exactly** these four fields for the package phase:

```text
Dependency-Receipt: /project/ppavlik/pfa-mechanism-study-20261110/runtime/provenance/dependencies-ACTUAL_JOB_ID.json
Dependency-Receipt-SHA256: REPLACE_WITH_ACTUAL_DEPENDENCY_RECEIPT_SHA256
Candidate-JSON: /project/ppavlik/pfa-mechanism-study-20261110/candidate/candidate.json
Candidate-JSON-SHA256: REPLACE_WITH_ACTUAL_CANDIDATE_JSON_SHA256
```

The candidate uses the maintained release identity structure; its actual fields
must identify the newly verified archive and installation, without embedding a
version or archive path in these generic setup scripts:

```json
{
  "version": "REPLACE_WITH_FROZEN_NEW_VERSION",
  "source_archive": "/project/ppavlik/pfa-mechanism-study-20261110/candidate/REPLACE_WITH_FROZEN_ARCHIVE_NAME.tar.gz",
  "source_sha256": "REPLACE_WITH_ACTUAL_NEW_ARCHIVE_SHA256",
  "manifest_sha256": "REPLACE_WITH_ACTUAL_RESEARCH_SOURCE_MANIFEST_SHA256",
  "library": "/project/ppavlik/pfa-mechanism-study-20261110/runtime/r-library"
}
```

The package phase verifies the explicit dependency receipt, every recorded private
package version, the same SIF and dependency DESCRIPTION, and the candidate JSON
and source archive hashes. It extracts only the archive's DESCRIPTION into
node-local scratch and checks its package/version and dependency declarations.
A version-only DESCRIPTION change is allowed because the dependency contract is
checked independently; the installed package must exactly match the candidate
version and source-manifest hash. It refuses to overwrite an existing LKT
installation. No CRAN dependency resolution occurs during source-package install.
A separate `package-JOBID.json` receipt records the source identity and installed
runtime. It does not promote the central release or establish scientific readiness.

After review and a completed new source build, the package submission uses the
same command with `runtime/package.dcf` and `package`. Actual package/model tests
then run separately inside Slurm, for example with `R_LIBS` and `R_LIBS_USER` set
to the verified private library and the same SIF/locale/thread limits:

```text
Rscript --vanilla tests/gru-latent-denoising.R
Rscript --vanilla tests/gru-bayesian-prototypes.R
```

Add the EAKT tests once their maintained implementation is complete. These are
runtime checks, not research fits; execute them on Big Blue. Local AST/R/Bash
parsing of this setup source does not validate network availability, system build
dependencies, pinned-repository contents, successful installation or Torch loading.
