# One-container GRU mechanism validation

This harness runs the selected maintained package in isolated Rscript child
processes inside one Linux/Slurm container. It submits no jobs, installs nothing,
and launches no research experiment. Tiny synthetic regression fits are part of
the existing tests. All runtime execution belongs on Big Blue.

Before execution, build/install the candidate, refresh the maintained research
manifest to include the harness and analytical probe, and transfer the matching
source/tests. Supply the installed manifest's exact SHA256 explicitly. Source,
test and fixture bytes are checked against that installed manifest before running.
A fresh output directory is required; earlier evidence is never overwritten.

The seven positional arguments are source ROOT, private LIBRARY, expected LKT
version, expected R version, installed research-manifest SHA256, OUTPUT, and either
an explicit real semantic-config JSON path or the literal `PENDING`.

Inside the allocated Slurm job, set explicit verified absolute paths. For the
current prepared runtime, the dependency/candidate library is
`/project/ppavlik/pfa-mechanism-study-20261110/runtime/r-library`, expected R is
`4.5.3`, and expected candidate LKT is `2.0.0.9023`. Use the already verified SIF
path supplied by the operator; the harness does not search for an image or library.

```bash
# Required existing variables: SIF, ROOT, LIBRARY, MANIFEST_SHA256, OUTPUT.
# SIF is the verified rocker-r-4.5.3.sif absolute path; ROOT is matching source.
# Run this command within a real Slurm allocation, not on the login node.
export LC_ALL=C.UTF-8 LANG=C.UTF-8 LANGUAGE=C R_TESTS=
export OMP_NUM_THREADS=1 MKL_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
apptainer exec --cleanenv --bind /project:/project \
  --env LC_ALL=C.UTF-8,LANG=C.UTF-8,LANGUAGE=C,R_TESTS= \
  --env OMP_NUM_THREADS=1,MKL_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1 \
  --env SLURM_JOB_ID="$SLURM_JOB_ID",SLURM_CPUS_PER_TASK="$SLURM_CPUS_PER_TASK" \
  "$SIF" Rscript --vanilla "$ROOT/scripts/validate-gru-mechanisms.R" \
  "$ROOT" "$LIBRARY" 2.0.0.9023 4.5.3 "$MANIFEST_SHA256" "$OUTPUT" PENDING
```

The image must have `C.UTF-8` verified before using the shown locale. The harness
requires Unix, a Slurm job ID/CPU allocation, explicit one-thread limits, exact R
and LKT versions/package location, Torch0.17.0 with real LibTorch, and matching
installed/source manifest bytes. It forces `LKT_REQUIRE_TORCH_TESTS=true` and
private `R_LIBS`, including for tests that launch their own fresh R processes.

The13 native gates are configuration, defaults, saved architecture, tensor
ownership, runtime, GRU regression, constant plasticity, retrieval filtering,
KeenKT latent denoising, Bayesian prototypes, EAKT exercise attributes, EAKT study
preflight rejection tests, and an independent analytical parameter-count probe.
They cover causal feedback, participant isolation, padding/chunks, stable numerical
updates, gradients, saved replay, candidate scoring and baseline/RNG parity. The
probe independently totals encoder, embedding, GRU, retrieval, head and adaptation
parameters: baseline43042, Keen45154, Bayesian43062, EAKT41218+8B. Synthetic fixture
IDs are used; no actual study annotations are manufactured.

`OUTPUT/validation.json` records every exit status, elapsed time and log hash,
source/runtime identity, `native_required_checks_verified`, `semantic_status`, and
`complete`. Logs are one file per regression. All native tests continue after an
individual failure to retain independent diagnoses; any requested test failure
returns nonzero. A native-only request returns zero only when all13 native gates
pass, while explicitly retaining `semantic_status=PENDING...` and `complete=false`.
This qualifies native implementation evidence independently of DKT-Sem readiness.

When the real pinned Python/model runtime is ready, replace `PENDING` with the
explicit `LKT_SEMANTIC_TEST_CONFIG` JSON path in a fresh output directory. The
semantic regression must exit zero and emit its final full-integration PASS marker;
a missing configuration or skipped encoder cannot pass. `complete=true` then
requires all native gates and that actual semantic test. EAKT real annotation
readiness remains a separate study-preflight gate in all modes; this harness
records `eakt_study_annotations_verified=false` even after synthetic EAKT tests pass.

Ordinary package checking is separate evidence. In the same selected container and
private library, root can run `R CMD check --no-manual --no-build-vignettes
--ignore-vignettes PACKAGE_ARCHIVE` in a fresh check directory, with the same locale,
thread limits and `LKT_REQUIRE_TORCH_TESTS=true`. If semantic configuration is absent,
its test currently prints a pending message; an otherwise successful R CMD check
must therefore be reported alongside the harness's semantic-pending status, never
as completed semantic verification. A practical Slurm allocation is one CPU,16GB
memory and a bounded two-hour wall time; actual elapsed time is recorded and does
not authorize any15-fit research condition or release promotion.
