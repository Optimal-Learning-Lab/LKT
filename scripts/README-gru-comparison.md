# GRU versus the configured PPES tracer

For the separate deterministic/Gaussian/KL architectural experiment, see
[the inference comparison guide](README-gru-inference.md). It has its own frozen
protocol and does not replace the PPES comparison or earlier results.

`compare-gru-ppes.R` runs a frozen predictive benchmark against the PPES tracing
specification in PolicyFactory2's `paper_rerun_wald` workflow. It reads that
workflow's nonlinear parameter vector and fits PPES linear coefficients on the
benchmark training participants. It does not change the simulation workflow.

## Current Cloze item and skill structure

New `largerawsample` benchmarks preserve the export's **144 original item labels**
in `KC..Default.` and **36 skill labels** in `KC..Cluster.`. Both raw and augmented
GRUs receive separate item and skill embeddings, each 16 values wide. The matched
GRU continues to receive only the PPES design columns; those features now use the
144 original item identities. The PPES specification is still the simulation's
configured tracer, not the paper's LKT-lasso model.

The source labels preserve feedback-condition distinctions. These counts match
[UNVaMP's Cloze summary](https://educationaldatamining.org/edm2026/proceedings/2026.EDM.full-papers.72/index.html),
but the paper does not document its exact source-column mapping. After removing
unscored `STUDY` events, preprocessing repeatedly removes participants with fewer
than 10 distinct items and items answered by fewer than 100 distinct participants,
until no further rows are removed. Counts use unique identities, not trial counts.
Filtering occurs before the participant split.

The verified bundled-data result is **55,122 scored responses, 478 participants,
144 items, 36 skills; zero scored responses removed by the thresholds**. The
minimums are 36 distinct items per participant and 126 distinct participants per
item. This still does not reproduce the paper's 45,115 responses. The split remains
70/15/15 whole participants, seed 1; matching the paper's reported stratified
50/25/25 design and obtaining its exact assignments are separate outstanding steps.

To reproduce the preprocessing and actual two-table neural encoding audit without
training, give the script a new output directory:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla `
  scripts/audit-gru-cloze-data.R C:/dev/LKT `
  C:/dev/LKT-outputs/gru-cloze-item144-skill36-new-audit
```

The output includes filtering stages, per-item participant counts, per-participant
item counts, item-to-skill mapping, a source-row inclusion ledger, the prepared data,
split assignments, and `verification.txt`. The audit independently reconstructs
the retained rows and tests a multi-pass removal cascade with duplicate responses.

Current comparison protocols use schema 3 and grid protocols use schema 2. New
grids require a new compatible reference benchmark. Earlier results and frozen
source bundles remain unchanged; use their frozen scripts to resume them. The
network figure and November 1 results below describe the **historical 72-identity
model**, not this new two-embedding input structure. No replacement training results
are implied by the preprocessing audit.

## Reading the GRU code

The scripts now have short English comments before the main steps, explaining
why each step exists. For the grid search, read these files in this order:

| File | What it explains |
|---|---|
| [gru-grid-config.R](gru-grid-config.R) | Which hyperparameter combinations we intend to compare. |
| [grid-search-gru.R](grid-search-gru.R) | How data, participants, settings, and source versions are fixed before training. |
| [gru-grid-utils.R](gru-grid-utils.R) | The actual `LKT(..., model = GRUModel(...))` call, one candidate at a time, and its checkpoint. |
| [lkt-model-gru.R](../R/lkt-model-gru.R) | The network, causal forward pass, loss, training loop, and stopping rule. |
| [lkt-gru-input.R](../R/lkt-gru-input.R) | Causal features, training-only vocabularies/scaling, and row partitions. |
| [lkt-gru-prediction.R](../R/lkt-gru-prediction.R) | Prediction before observing an answer, followed by a memory update. |
| [summarize-gru-grid.R](summarize-gru-grid.R) | Validation-only choice, followed by selected-model test scoring. |
| [verify-gru-grid.R](verify-gru-grid.R) | Independent checks of the inputs, choices, probabilities, and reported metrics. |
| [hpc/setup-gru.slurm](hpc/setup-gru.slurm) and [hpc/run-gru-grid.slurm](hpc/run-gru-grid.slurm) | Why environment preparation and the full search are separate jobs. |

The earlier readability-only edit preserved the R expressions, shell commands,
and Slurm resource directives. The current item/skill change alters preprocessing
and neural inputs as described above. Comments also change bytewise source hashes:
exact resumption of an archived experiment must use its frozen source bundle.
The annotated working files do not replace the archived code or results.

## Current chosen network figure (144 items, 36 skills, 128-unit head)

The [current overview](../docs/figures/gru-head128/gru-network-overview.svg)
shows the exact `augmented__head128__seed1` checkpoint inspected in the FGUI:
144 item IDs and 36 skill IDs, with separate learned 16-value embeddings;
five numeric inputs; three GRU layers of 32 units each; and a `69 -> 128 -> 1`
prediction head. This is the user's chosen 128-unit head, not the search's
automatic 256-unit validation winner. The checkpoint restored epoch 24.

The current input vector has `5 + 16 + 16 = 37` coordinates. Prediction uses
the previous top-layer state plus those current inputs (`32 + 37 = 69`).
Only after the current response is observed does the recurrent update consume
the 38-value input `[x(t), y(t)]`. Dropout locations are shown for training;
evaluation disables dropout. There is no separate inference network in this model.

Download the overview as [PDF](../docs/figures/gru-head128/gru-network-overview.pdf)
or [PNG](../docs/figures/gru-head128/gru-network-overview.png). The
[full figure](../docs/figures/gru-head128/gru-network.svg), also in
[PDF](../docs/figures/gru-head128/gru-network.pdf), adds the exact GRU gate equations.

The maintained [generator](draw-gru-network.R) requires the explicit active LKT
2.0.0.9010 installation and release checksum specified in AGENTS.md. It checks
the checkpoint's original protocol and data, both embedding tables, all layer
dimensions, and 31,585 parameter entries. It independently computes the GRU
gates and head output, checks current-answer causality, and replays the saved
predictions for the first held-out participant (up to 120 responses).
See the [audit](../docs/figures/gru-head128/gru-network-audit.txt),
[input coordinates](../docs/figures/gru-head128/gru-network-numeric-inputs.csv),
[weight shapes](../docs/figures/gru-head128/gru-network-weight-shapes.csv), and
[provenance](../docs/figures/gru-head128/gru-network-provenance.txt).

```
Rscript --vanilla scripts/draw-gru-network.R C:/dev/LKT
```

This reads the original historical checkpoint with the maintained implementation;
it does not restore retired installations, retrain, or change fitted artifacts.

## Historical network figure (72 identities)

The [network figure](../docs/figures/gru-network.svg) describes the selected
**augmented, three-layer, 32-unit GRU**. It is also available as a
[PDF](../docs/figures/gru-network.pdf) and
[PNG](../docs/figures/gru-network.png).

Panel A follows one participant at one trial. Five numeric coordinates and a
16-coordinate KC embedding form the 21-coordinate predictor vector. The head
combines that vector with the **previous** top-layer memory, while the observed
answer is appended only for the recurrent update. Each layer retains its own
32-coordinate state. The head is `53 -> 32 -> 1`, with ReLU and dropout after
its hidden layer; both head layers include biases. Exported probabilities apply
sigmoid and clipping to `[0.00001, 0.99999]`, while training loss uses the logits.

Panel B expands one GRU cell into its update gate, reset gate, candidate, and
state mixture. Its reset gate multiplies the complete recurrent candidate
projection, including the recurrent bias, matching the
[Torch GRU equations](https://torch.mlverse.org/docs/reference/nn_gru.html).
Dropout locations are shown for training and are inactive during evaluation.
The retained LKT intercept is included in the input count and becomes zero
after training-mean centering.

The other selected input versions use the same construction with these sizes:

| Input version | Numeric coordinates | Embedding coordinates | Encoded predictors | GRU layers | Head input width |
|---|---:|---:|---:|---:|---:|
| Matched PPES features | 4 | 0 | 4 | 2 | 36 |
| Raw KC/time | 1 | 16 | 17 | 3 | 49 |
| Augmented | 5 | 16 | 21 | 3 | 53 |

The original [accuracy check](../docs/figures/gru-network-audit.txt) and figure
remain historical artifacts. The current generator above targets the chosen
144-item/36-skill model and deliberately rejects the old architecture rather
than silently drawing either model with mismatched labels.

## Historical November 1 protocol

The dataset was **`LKT::largerawsample`**, the larger bundled DataShop
export used by LKT's vignettes. It contains 58,316 original records from 478
participants. Following `prepare_largeraw_sample()` in
`vignettes/lkt-vignette-setup.R`, the benchmark uses `Problem.Name` as
`KC..Default.`, parses `Time`, and retains all 55,122 `CORRECT`/`INCORRECT`
responses. The 3,194 `STUDY` events do not update either model. There are 72
problem/KC identities. The export has no timezone; timestamps use UTC as a
fixed numerical reference, avoiding host-dependent daylight-saving conversion.
An unrecorded local clock change cannot be reconstructed.

`samplelkt` is a separate, much smaller export (1,941 responses from 17
participants), available only by choosing that dataset explicitly. Its practice
clock is cumulative prior response duration because it has no calendar times.
The third choice, `calibration`, reads the upcoming simulation's human
calibration data. These choices have separate output directories and protocols.

## Run the current benchmark

Arguments, in order: LKT repository, PolicyFactory2 repository, output directory,
worker count, dataset, optional comma-separated GRU layer counts (default `1,2`),
and optional reference benchmark directory. The benchmark currently requires LKT 2.0.0.9008 and a
working R Torch runtime. In the current Windows research environment:

```powershell
$env:LC_ALL = 'English_United States.utf8'
$env:R_LIBS = 'C:/dev/LKT/.codex-r-lib'
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla `
  scripts/compare-gru-ppes.R C:/dev/LKT C:/dev/PolicyFactory2 `
  C:/dev/LKT-outputs/gru-vs-ppes-largerawsample-item144-skill36-new-run `
  3 largerawsample
```

For this dataset, participant split seed 1 gives 334 training participants
(38,530 responses), 71 validation participants (8,191 responses), and 73 test
participants (8,401 responses). All scored responses are retained and every
KC is represented in training. Test predictions use prior responses from the
same held-out participant as causal history; their outcomes never update model
weights. The task is prediction for new participants on known problem/KC
identities.

Each GRU input family has one- and two-layer candidates and three training
seeds. The matched family receives exactly the PPES design-matrix columns;
raw uses item and skill embeddings and time gaps; augmented also uses PPES features. The selected
architecture minimizes mean validation log loss separately for each family.
Training allows 100 epochs with validation early stopping, and restores the
best validation checkpoint. Test scores average three separately trained
models' metrics, rather than ensemble probabilities. This is a limited,
prespecified search; reaching the epoch limit is a material qualification.

The script saves the protocol before fitting, checks numeric identity of the
matched predictors, and saves architecture selection before scoring GRU test
predictions. Existing checkpoints can be resumed only when the saved protocol,
input hashes, core model source hashes, and candidate settings match. Preserve
the running script until its process finishes: R may read source incrementally.

## Verify and read

After the fitting process exits successfully:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla `
  scripts/summarize-gru-ppes-phases.R `
  C:/dev/LKT-outputs/gru-vs-ppes-largerawsample-20261101 `
  C:/dev/LKT-outputs/gru-vs-ppes-largerawsample-20261101/largerawsample_source.rds
```

Verification recomputes every pooled metric from saved predictions, checks
source-row labels and participant membership, checks that every selected model
covers all held-out scored rows exactly once, and independently reconstructs
the architecture choices from validation scores. Condition summaries use
literal `Condition.Name.2` labels. Numeric unit IDs are not interpreted as
pretest/posttest phases.

Read `report.html`, `test_summary.csv`, `paired_log_loss_differences.csv`, and
`verification.txt`. The intervals resample whole held-out participants and are
conditional on this split, the frozen PPES nonlinear settings, the chosen
architectures, and the three training seeds. They do not quantify scheduling
outcomes, learning gains, or performance on unseen problem identities.

## Historical three-layer follow-up (use the archived source)

To add three-layer fits without replacing or retraining the original one- and
two-layer fits, use a separate output directory and provide the original run as
the reference:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla `
  scripts/compare-gru-ppes.R C:/dev/LKT C:/dev/PolicyFactory2 `
  C:/dev/LKT-outputs/gru-vs-ppes-largerawsample-3layer-20261101 `
  3 largerawsample 3 `
  C:/dev/LKT-outputs/gru-vs-ppes-largerawsample-20261101
```

This runs nine fits: three input families, three seeds each, with three 32-unit
GRU layers and 0.1 dropout between layers. All other settings match the earlier
run. The reference protocol, dataset and parameter hashes, shared settings,
software and core model source hashes, and participant assignments must match
before fitting starts. The protocol records hashes of the reference results.
This follow-up was requested after viewing the reference test results, so its
reused-test comparisons are explicitly exploratory.

After fitting finishes, run `summarize-gru-ppes-phases.R` on the new output and
its `largerawsample_source.rds`, then compare the verified runs:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla `
  scripts/compare-gru-depths.R `
  C:/dev/LKT-outputs/gru-vs-ppes-largerawsample-20261101 `
  C:/dev/LKT-outputs/gru-vs-ppes-largerawsample-3layer-20261101
```

`depth_comparison.html` shows validation performance at all three tried depths,
the depth selected by validation alone for each input family, test results for
the earlier selected depth and the new depth, and paired participant-bootstrap
intervals. Its checks establish identical splits and paired source rows/seeds,
unchanged PPES predictions, and agreement of paired loss differences with
independently verified pooled scores. The original report stays unchanged.

## Full factorial grid search

`grid-search-gru.R` accepts, in order: repository root, PolicyFactory2 root,
reference benchmark, new output directory, workers, input families (`all` or a
comma-separated subset), profile, action (`plan`/`run`), additional cache
directories separated by semicolons, and cache policy (`exact`/`none`).

The user-selected **compact** profile crosses GRU layers `{1,2,3}`, widths
`{32,64}`, and learning rates `{0.001,0.003}`. That is 12 configurations per
input family, 36 across all three families, and **108 fits with three seeds**.
Dropout stays 0.1 at the input and head and between GRU layers when applicable;
weight decay stays 0.0001. The 32-unit ReLU head, 16-dimensional embeddings,
AdamW, batch size 32, chunk length 100, gradient clipping 5, 100-epoch maximum,
and early-stopping controls retain the earlier settings. The `broader` profile
also crosses shared dropout `{0,0.1}` and weight decay `{0,0.0001}`.

Every candidate and resolved settings list is saved before fitting. Each fit
has an atomic model checkpoint and validation-score file. Configuration choice
minimizes mean validation log loss across all three seeds, separately by input
family. An immutable selection file is saved before the selected configurations
are test-scored. Test scores never rank grid candidates. The independent
verifier checks all candidates' validation predictions, the selection, source
response coverage and chronology, fitting masks, saved test predictions, and
all reported pooled metrics. Its independent AUC calculation uses ranks.

`summarize-gru-grid.R OUTPUT progress` produces a validation-only progress report;
`summarize-gru-grid.R OUTPUT final` requires every fit and freezes/scores the
chosen models. `verify-gru-grid.R OUTPUT` independently verifies the completed
search. `depth-check` is a pipeline verification profile covering only the
already-tried depths at width 32 and learning rate 0.001; it is not the compact
search. The complete pipeline passed on all 27 previously fitted models.

UNVaMP section 3.3.1 reports Bayesian optimization of learning rate, embedding
size, dropout, recurrent width/depth, feature-extractor depth, and decoder depth
for its MLP version. It uses AdamW but does not publish numeric search bounds
or winning settings in the paper. It therefore identifies useful future search
dimensions without supplying the numerical values in our compact grid:
<https://educationaldatamining.org/edm2026/proceedings/2026.EDM.full-papers.72/index.html>.
Its latent-distribution KL penalty is specific to UNVaMP and is not a control in
this standard GRU implementation.

## Big Blue execution

The isolated run workspace is `/home/ppavlik/LKT-gru-grid-20261101`. Its bundle
contains the source, exact dataset snapshot, participant assignments, frozen
PPES checkpoint/parameters, and checksum manifest. Big Blue's native R/glibc
combination cannot load the pinned Torch runtime. Setup therefore uses the
supported `singularity/4.1.2` module and `rocker/r-ver:4.5.3` image as the
deployment environment. It records and verifies the resulting SIF checksum,
installs LKT 2.0.0.9008 and CPU R Torch 0.17.0 into a separate
`library-container` directory, runs all three GRU regression scripts, and
prepares the compact grid protocol. Host R packages and system libraries are
not installation targets. Both jobs use the same verified image, clean
environment, unprivileged `--userns` execution, workspace bind mount at `/work`, and private R library. The
protocol records the container checksum alongside the actual R runtime.
The setup supplies checksum-pinned CMake 3.31.8 in the private workspace for
building LKT's `nloptr` dependency; it does not install host system packages.

`scripts/hpc/setup-gru.slurm` requests four CPUs and 24 GB. The dependent
`scripts/hpc/run-gru-grid.slurm` requests twelve CPUs and 24 GB, uses twelve
single-threaded R workers, and scores/verifies the selected models after all
fits finish. It refits **all 108 models on Linux**, using cache policy `none`,
to keep the grid's execution environment consistent. Windows-trained fits are
not mixed into this grid. The original RDS snapshot is copied byte-for-byte;
the protocol records the actual runtime separately from the reference runtime.

Setup and grid are separate Slurm jobs with an `afterok` dependency. The
`SETUP_COMPLETE` marker requires installation, regression tests, and grid
preflight to succeed. `GRID_COMPLETE` requires the full grid and independent
result verification to succeed. Queued/submitted jobs are distinct from a
completed, verified search. Existing simulation jobs and their libraries are
not modified by these scripts.
