# LKT workspace layout

| Location | Keep here |
| --- | --- |
| `C:/dev/LKT` | The maintained R package, tests, reusable research and analysis scripts in `scripts/`, documentation, and curated figures in `docs/figures/`. |
| `C:/dev/LKT-data` | Bulk research inputs. The official EdNet archives and extracted raw inputs are in `ednet-ktst-base-20261105/downloads/` and `ednet-ktst-base-20261105/raw/`. |
| `C:/dev/LKT-outputs` | One named folder per experiment, including its fitted models, checkpoints, reports, logs, prepared data, frozen protocols/source bundles, submission records, and one-off working scripts/notes. Verification and package-build outputs also belong here. |

The intentional `data/samplelkt.rda` and `data/largerawsample.rda` package assets
stay in the source repository. Existing external datasets stay at their established
locations; this cleanup does not relocate PolicyFactory2's calibration inputs or
datasets referenced by the vignettes. Do not put their copies into the package.

Research scripts accept explicit input/output paths. Where a script supplies a
local default, it derives the sibling `LKT-outputs` directory from the supplied
source root. There are no old-location searches, junctions or symlinks. For example,
pass `C:/dev/LKT-data/ednet-ktst-base-20261105/downloads` as the archive directory
to the EdNet preparation scripts, and a fresh named folder under `LKT-outputs`
as their output. Prepared data already frozen for an experiment stays with it.

Reusable launchers and reporting/plotting functions stay in `scripts/`. Dated
experiment working files stay with their outputs. Generated vignette reports now
live in `C:/dev/LKT-outputs/package-vignettes/_example-reports`. The report writers
use this location; executing a vignette or benchmark may fit models and is a
separate action from locating or reading existing results.

## Frozen GRU design and results

The November 6 reviewed design is documented in
[the curated GRU figure guide](figures/gru-base-model/README.md). Its unfitted
editable specification and current location record remain alongside the figure.
The immutable freeze is `C:/dev/LKT-outputs/gru-defaults-freeze-20261106/`:

- `model.rds`: the reviewed seed-1 fitted checkpoint, SHA-256
  `d2aa459056bf799106dccf8024311444faba58873beaafeb1380304adebe2e22`.
- `prepared-data.rds`, `study.rds`, `settings.*`, `selection.*`, and
  `model-specification.rds`: the frozen data, protocol, settings and identities.
- `README.md`, verification logs, and `prior-figure/`: the original freeze record.

The full study and predictive results are in
`C:/dev/LKT-outputs/ednet-unvamp-matched-20261105/RESULTS.md`, with checkpoints in
`download/study/models/` and tables/reports in `download/study/reports/`. The freeze
retains the reviewed checkpoint directly; it does not select a seed by test score.
The earlier 144-fit factorial study is separately preserved in
`C:/dev/LKT-outputs/gru-ktst-comparison-20261104/RESULTS.md` and `download/results/`.
The local GRU/Mamba study is in `C:/dev/LKT-outputs/sequence-mamba/experiment/`.

From `C:/dev/LKT`, verify the current figure's checkpoint/specification without
training, or regenerate the figure into a fresh review directory:

```powershell
Rscript --vanilla scripts/open-gru-base-fgui.R C:/dev/LKT verify
Rscript --vanilla scripts/draw-gru-retrieval-network.R C:/dev/LKT C:/dev/LKT/docs/figures/gru-base-model/selection.rds C:/dev/LKT-outputs/gru-figure-review
```

The figure generator checks stored hashes and performs independent numerical
and held-out prediction replay. It does not fit a model. Its omitted output
argument intentionally updates the curated figure in the code repository.

## Frozen records and duplicate review

Experiment folder names and their internal result organization were retained.
Older `.codex/benchmarks` runs are directly under `LKT-outputs`, and old HPC
submission/download records are in `LKT-outputs/hpc`. The two shared EdNet input
trees were separated into `LKT-data`; their original archive hashes and download
provenance remain in the experiment records.

Historical protocols, fitted objects, original submissions, source snapshots,
working scripts and reports retain their original bytes. Embedded old paths are
provenance, not executable location configuration. Use maintained scripts with
explicit current paths for new work. Exact replay/resumption of a hash-bound old
study requires its frozen source and declared runtime; changing its recorded paths
or accepting changed source hashes would invalidate that contract. Do not run an
old completion watcher or submission script as a new launcher.

The move manifests and verification logs are in
`C:/dev/LKT-outputs/workspace-cleanup-20261106/`. `moves.csv` maps the old roots to
their real new homes, and `files-before.csv` records every moved file's target,
size and SHA-256. The supplementary vignette manifest covers the report tree.
`duplicates-for-review.csv` identifies identical nonempty files by hash, including
copies inside frozen bundles; none were discarded. The reviewed seed-1 checkpoint
also occurs in the original study and its seed-1 preview. The ignored
`scripts/plot-online-decay-trajectories copy.R` remains in place for review; its
name alone is insufficient evidence to delete it or classify it as disposable.

Before any later move, inspect local research processes, scheduled/completion
watchers, lock ownership and remote job status. Leave an in-use folder where it
is until its jobs and watchers finish. Do not stop or restart jobs for cleanup.

## Package builds and the current release

`.gitignore` and `.Rbuildignore` exclude accidental local research trees.
Stage package inputs outside the maintained source tree before building so R's
Windows copy step cannot pull in research files or live locks:

```powershell
Rscript --vanilla scripts/stage-package-verification.R C:/dev/LKT C:/dev/LKT-outputs/package-build-review
Set-Location C:/dev/LKT-outputs/package-build-review
R CMD build --no-build-vignettes --no-manual LKT
```

Choose a new staging directory each time. Staging rejects an output inside the
source tree, checks that `data/` contains only the two intentional samples, prunes
ignored directories before copying, and verifies every included source hash.
Use ordinary package checks on the staged package/archive as appropriate; check
options that execute examples, vignettes or tests can perform fits.

The single active release definition remains
`C:/dev/PolicyFactory2/workflows/simulation/shared/lkt-release.env`. Maintained
research scripts use `scripts/load-current-lkt.R`; the dependency library
`.codex-r-lib` stays in place. The current root source archive is retained because
the release contract references it. Moving experiment evidence does not promote a
release or make an old candidate library the active installation.
The working-source manifest tracks the relocated research drivers and docs.
The older head-width and encoder figure generators additionally require the entire
working manifest to match the active release. Existing unpublished package changes
already make those identities differ; these generators remain blocked by that
integrity gate. Their paths are updated, but their gate was preserved. The current
frozen-base figure workflow above works through the central released runtime.

## Git contents versus the R source package

Git includes maintained R/C code, tests and fixtures, help pages, vignettes, reusable
research scripts, contributor documentation, and curated figure assets. The two
small GRU configuration RDS files and the exact figure-audit CSV files have explicit
`.gitignore` exceptions. They contain settings, dimensions and aggregate numerical
checks; fitted weights and participant-level research records remain outside Git.

The R source package includes the runtime, tests/fixtures, help, vignettes, required
attribution, and the two intentional sample datasets. `.Rbuildignore` excludes
`scripts/`, `docs/`, agent/operator records, local dependencies, research trees,
compiled local intermediates and generated reports. Downloaded paper PDFs and the
local `policy.md` operator log are ignored by Git; paper citations and source
metadata remain versioned. Keep new curated exceptions exact rather than broadly
allowing research RDS or CSV files.

Text source uses repository-enforced LF line endings so working files, Git blobs
and fresh checkouts have identical manifest hashes. Binary samples, portable RDS
configurations and figure assets are explicitly marked binary in `.gitattributes`.
The policy does not rewrite frozen experiment or active-release artifacts.
