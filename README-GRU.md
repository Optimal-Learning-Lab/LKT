# The three GRU versions

Use these stable names in code, reports and experiment descriptions:
**`baseline_six`**, **`tracer_six`**, and **`ground_truth_eight`**.
They share the maintained LKT GRU implementation. Their differences are the
available inputs and which identity-specific values may carry across students.
The [profile catalog](scripts/gru-variants.json) and
[explicit model constructors](scripts/gru-variants.R) encode that distinction.
They are opt-in research profiles, not changes to ordinary `GRUModel()` defaults.

| Property | `baseline_six` | `tracer_six` | `ground_truth_eight` |
|---|---|---|---|
| Purpose | Predictive reference with a familiar item/skill bank | Knowledge tracing for a new student using observable history | Generate responses conditional on supplied ability/easiness |
| Numeric inputs | Six history/timing values | Same six values | Same six + two LKT trait values |
| Explicit LKT ability/easiness inputs | None | None | Student ability and item easiness |
| Item/skill embeddings learned across training students | Retained for prediction | None retained | None retained |
| Item and skill vectors at a new student | Use fitted shared tables | Reset to zero and learn locally | Reset to zero and learn locally |
| Recurrent memory, retrieval and output adjustments at a new student | Reset | Reset | Reset |
| Shared network weights at evaluation | Fixed | Fixed | Fixed |
| Output learning | Learned constant-rate rule | Same constant-rate rule | Same constant-rate rule |

Student IDs route each student's history. **None of these configurations has a
pretrained student-ID embedding.** The baseline can nonetheless know about familiar
items and skills through its fitted embedding tables. "No intercept inputs" means
no supplied LKT ability/easiness scalars; neural layers still have ordinary biases.

### Construct the named profiles explicitly

```r
root <- 'C:/dev/LKT'
source(file.path(root, 'scripts/load-current-lkt.R'))
source(file.path(root, 'scripts/gru-variants.R'))
baseline <- gru_variant_model('baseline_six', root)
tracer <- gru_variant_model('tracer_six', root)
ground_truth <- gru_variant_model('ground_truth_eight', root)
```

These constructors select model settings only. They do not estimate traits,
calibrate LKT or train a network. Supply the fixed feature recipe and data through
the rerun entry points below. `base_settings` preserves an explicit saved backbone;
`overrides = list(seed = 2L)` allows a new neural seed. Overrides cannot silently
change a family's input, identity or output-plasticity contract.

## Common inputs and network

The six numeric coordinates are three LKT history values and three timing values:

1. Student logitdec.
2. Student-item PPES.
3. Student-item logitdec.
4. Item age, transformed with `log1p`.
5. Mean prior spacing, transformed with `log1p`.
6. An indicator that mean prior spacing is available.

Use the fitted model's `preprocessing$numeric_names` for its actual coordinate
order. The LKT recipe is `components = c('Anon.Student.Id','KC..Default.',
'KC..Default.')`, `features = c('logitdec','ppes','logitdec')`, `interc = FALSE`.
Its nonlinear parameters must already be fixed before neural fitting.
These six numeric coordinates are separate from the two identity channels:
`KC..Default.` identifies items and `KC..Cluster.` supplies skill-group labels.
The current Cloze data has 55,122 responses, 478 students, 144 items and 36 supplied
skill groups. All 144 items and all 36 groups occur in each cv1 split.

The compared backbone has three 32-unit GRU layers, numeric encoding, 16-value
item and skill vectors, item-or-skill filtered retrieval over the last 100 events,
a 128-unit prediction head, and constant-rate output plasticity. It predicts
before observing an answer and then updates the student's local state. Shared
weights learn across training students; held-out replay keeps those weights fixed.
This document describes the six-input constant-output family, not the Lasso,
five-input feature-removal, fatigue, or other architecture experiments.

## 1. `baseline_six`: learned item/skill identities

This is the full predictive reference from which the student-local tracer was
adapted. It has the six numeric inputs, no LKT trait scalars, and ordinary learned
item/skill tables (`embedding_adaptation = 'none'`). Those tables carry information
from training students to held-out students. Recurrent and online output state
still starts fresh for every student.

The matching historical reference is the **constant** condition in
`gru-plasticity-cloze-20261107-v3/download/results`, under the sibling
`LKT-outputs` directory. Its `cv1__constant__seed1.rds` checkpoint has held-out
AUC **0.858034**. The original five-fold/three-seed report gives mean seed-pooled
AUC **0.859248**. Those summaries are different aggregations; do not substitute
one for a single-fold result or describe either as the conditional eight-input fit.

### Rerun a baseline fit with the current release

The historical `compare-gru-plasticity.R` orchestrated a three-condition candidate
study. Its old candidate-library arguments are not the current rerun interface.
Do not restore an obsolete package to use them. Use the public LKT call below for
a new baseline fit, with a new output file and the central release loader:

```r
root <- 'C:/dev/LKT'
source(file.path(root, 'scripts/load-current-lkt.R'))
source(file.path(root, 'scripts/gru-variants.R'))
outputs <- file.path(dirname(root), 'LKT-outputs')
reference <- file.path(outputs,
  'gru-plasticity-cloze-20261107-v3/download/results')
p <- readRDS(file.path(reference, 'study.rds'))
d <- readRDS(file.path(reference, 'data/prepared_data.rds'))
# The reference data also contains a development split: select cv1 explicitly.
d$fold <- readRDS(file.path(reference, 'partitions/cv1.rds'))
model <- gru_variant_model('baseline_six', root,
  base_settings = p$settings$constant,
  overrides = list(seed = 1L, validation_folds = 2L))
destination <- file.path(outputs, 'baseline-six-new-run')
if (dir.exists(destination)) stop('Choose a fresh experiment directory')
dir.create(destination)
fit <- LKT::LKT(data = data.table::as.data.table(data.table::copy(d)),
  usefolds = 1L, components = p$baseline$components,
  features = p$baseline$features, fixedpars = p$baseline$fixedpars,
  interc = FALSE, model = model)
saveRDS(list(variant_id = 'baseline_six', fit = fit),
  file.path(destination, 'baseline_six.rds'))
```

This creates a new scientific fit; it is not a claim of bit-identical retraining
under a changed runtime. The historical fixed history parameters were inherited
from the reference recipe. For a new matched comparison to the recent tracer,
supply the same cached training-only history parameters to both fits and record
that choice, rather than comparing unlike calibrations as a pure architecture test.

## 2. `tracer_six`: fresh identities for every student

The tracer retains shared knowledge of *how to learn* from histories, but discards
all student-local values at each student boundary: item and skill vectors,
recurrent memory, retrieval bank and output adjustments. The reset occurs in
training, validation and testing. Each answered trial updates local state only
after its probability was computed. Shared network weights and the learned
constant output rate are retained across training students and fixed at evaluation.

Item/skill labels still identify repetitions and group membership. Familiar
labels do not supply pretrained vectors. Unknown identities have distinct local
entries. No named-skill discovery is claimed.

Use [gru-tracer-training-only.R](scripts/gru-tracer-training-only.R) with
[its configuration](scripts/gru-tracer-training-only.json). It reuses the six
nonlinear history parameters from an already converged training-only LKT control;
it performs no new nonlinear or Wald calibration. Its stages are:

```text
Rscript scripts/gru-tracer-training-only.R ROOT REFERENCE CONTROL_RECORD OUTPUT plan
Rscript scripts/gru-tracer-training-only.R ROOT REFERENCE CONTROL_RECORD OUTPUT prepare
Rscript scripts/gru-tracer-training-only.R ROOT REFERENCE CONTROL_RECORD OUTPUT smoke
Rscript scripts/gru-tracer-training-only.R ROOT REFERENCE CONTROL_RECORD OUTPUT fit
Rscript scripts/gru-tracer-training-only.R ROOT REFERENCE CONTROL_RECORD OUTPUT report
```

`REFERENCE` is the immutable local-item-and-skill tracer's `results` directory;
`CONTROL_RECORD` is its verified training-only history-calibration record. The
new `OUTPUT` must be a named folder directly under `LKT-outputs`. The current
cluster paths, inputs and result receipts are recorded in
`gru-tracer-training-only-20261113/submission` under local `LKT-outputs`.
The [Slurm runner](scripts/hpc/run-gru-tracer-training-only.slurm) uses an explicit
deployment manifest and its checksum; do not invent new library paths.

The completed training-only cv1/seed1 result is held-out AUC **0.817283**.
The older initial tracer's **0.816312** used the inherited fixed recipe; the later
full-data-history-calibrated **0.816874** comparison is a different protocol.
Use the **0.817283** result for the current training-only tracer reference.
See [the tracer guide](scripts/README-anonymous-gru-tracer.md) for local update
details, and distinguish its historical item-only experiment from the current
two-channel item-and-skill tracer.

## 3. `ground_truth_eight`: supplied ability and item easiness

This is the student-local tracer architecture with two additional scalar inputs:

- `random_intercept`: supplied LKT student ability.
- `kc_intercept`: supplied LKT item easiness (difficulty has the opposite sign).

These values enter the numeric encoder as inputs, so the GRU learns how they
affect responses while its recurrent/local mechanisms capture learning. They are
not fixed logit offsets. Item and skill vectors still reset for each student;
the scalar traits are separate from those vectors. There is no ninth skill
intercept input. A supplied skill-group label and item easiness are distinct.

### Known-trait simulation versus predictive evaluation

The intended simulation use supplies the simulator's chosen student ability and
item difficulty/easiness from the first trial. Those traits need not be inferred
from an unseen student's test answers: they are inputs controlled by the generator.
Conditional fitting asks whether the GRU can model responses **given those traits**.
Whole-student predictive evaluation instead asks how well it predicts a real new
student whose personal ability is unknown. These are different questions.

Two fitted protocols belong to this same model family:

| Protocol | Trait calibration and held-out ability | Recorded cv1/seed1 test AUC | Meaning |
|---|---|---:|---|
| Full-data known-trait conditioning | LKT traits estimated on all calibration rows; personal trait supplied for each student | 0.875239 | Conditional response fit; not an independent end-to-end new-student score |
| Training-only prediction | LKT uses training students only; held-out students receive the training-population ability prior | 0.845095 | New-student prediction under that explicit prior policy |

The 0.875239 result is relevant evidence for modeling responses with supplied
traits, but does not prove accuracy across arbitrary simulated trait ranges or a
fully validated simulator. The 0.845095 result has less personal-ability information
than the intended known-trait generator. Do not label the former leakage-free
new-student prediction, or treat the latter as the only measure of generator quality.
Do not rank the conditional score directly against the baseline's held-out score.

Both remain separate fitted ground-truth **candidates**. PolicyFactory2 now
supports the eight-input supplied-trait model as an opt-in generating role,
including student-local item/skill resets, immutable candidate scoring and the
existing sampling/Wald-latency contract. The
[spacing workflow](../PolicyFactory2/workflows/simulation/ground_truth_spacing/README.md)
reuses the known-trait checkpoint with its original Cloze trait scale and skill
mapping. Its fixed test blocks need no knowledge tracer. This software
integration does not establish validity across arbitrary simulated trait ranges.
The reported neural checkpoints were trained on their GRU training folds, not
refitted on every calibration response. A final all-calibration-row generator fit
at an explicitly selected epoch count would be a separate artifact and step; it
has not been performed by this documentation work. In simulation, supply traits
in the calibrated input units and use easiness = negative difficulty. Do not
silently substitute a population prior when the simulator supplies a personal trait.

### Rerun and reuse calibration

Use [gru-ground-truth-eight-input.R](scripts/gru-ground-truth-eight-input.R).
The predictive mode uses
[gru-ground-truth-training-only.json](scripts/gru-ground-truth-training-only.json):

```text
Rscript scripts/cloze-source-calibration.R ROOT REFERENCE STORE ensure
Rscript scripts/gru-ground-truth-eight-input.R ROOT PROJECT REFERENCE CONTROL_RECORD OUTPUT plan --training-only STORE
Rscript scripts/gru-ground-truth-eight-input.R ROOT PROJECT REFERENCE CONTROL_RECORD OUTPUT prepare --training-only STORE
Rscript scripts/gru-ground-truth-eight-input.R ROOT PROJECT REFERENCE CONTROL_RECORD OUTPUT smoke --training-only STORE
Rscript scripts/gru-ground-truth-eight-input.R ROOT PROJECT REFERENCE CONTROL_RECORD OUTPUT fit --training-only STORE
Rscript scripts/gru-ground-truth-eight-input.R ROOT PROJECT REFERENCE CONTROL_RECORD OUTPUT report --training-only STORE
```

`PROJECT` names the explicit PolicyFactory2 source snapshot supplying coefficient
extraction helpers. `STORE` is shared across GRU experiments, not placed in each
run's output folder. On Big Blue it is
`/project/ppavlik/LKT-outputs/cloze-source-calibrations`. The current cached record
contains the eight fixed PPES/PPEF nonlinear values **and** fitted intercepts.
`ensure` only calibrates when the exact matching artifact is absent; neural
preparation/training never refit LKT. Different GRU settings or neural seeds reuse
it. Different training folds/data/source specifications require a matching record.
See [calibration and verification details](docs/gru-ground-truth-training-only.md).

The known-trait conditioning mode uses
[gru-ground-truth-eight-input.json](scripts/gru-ground-truth-eight-input.json).
Its explicit optional `FIXED_CALIBRATION` directory selects a previously frozen
full-data calibration, avoiding a new nonlinear fit:

```text
Rscript scripts/gru-ground-truth-eight-input.R ROOT PROJECT REFERENCE CONTROL_RECORD OUTPUT ACTION FIXED_CALIBRATION
```

For that mode, `CONTROL_RECORD` must be the `control.rds` in the same verified
full-data fixed-calibration directory. Run the same plan/prepare/smoke/fit/report
actions. Omitting `FIXED_CALIBRATION` intentionally requests the older initial
full-data calibration path, so supply it when reusing fixed values. Do not point
the training-only evaluation at a full-data calibration.

That full-data bundle also supplies its own six history-function parameters.
The original 0.875239 experiment used its recorded training-only history control
alongside full-data traits. Changing the history source creates a different
protocol, even though it is still the `ground_truth_eight` family. To rerun that
original known-trait setup without any source calibration, use its already
prepared data and saved recipe directly:

```r
# After loading the central release and scripts/gru-variants.R as above:
# Run where the complete original artifacts exist; this is their cluster path.
original <- '/project/ppavlik/gru-ground-truth-eight-input-20261113/LKT-outputs/gru-ground-truth-eight-input-20261113'
p <- readRDS(file.path(original, 'protocol.rds'))
d <- readRDS(file.path(original, 'data/eight_input_data.rds'))
model <- gru_variant_model('ground_truth_eight', root, base_settings = p$settings)
destination <- file.path(dirname(root), 'LKT-outputs', 'ground-truth-eight-new-run')
if (dir.exists(destination)) stop('Choose a fresh experiment directory')
dir.create(destination, recursive = TRUE)
fit <- LKT::LKT(data = data.table::as.data.table(data.table::copy(d)),
  usefolds = 1L, components = p$history_recipe$components,
  features = p$history_recipe$features, fixedpars = p$history_parameters,
  interc = FALSE, model = model)
saveRDS(list(variant_id = 'ground_truth_eight', fit = fit,
  source_protocol_sha256 = digest::digest(file = file.path(original, 'protocol.rds'),
    algo = 'sha256')), file.path(destination, 'ground_truth_eight.rds'))
```

The supplied `d` already contains the fixed scalar traits; this call trains only
the GRU. Full-data trait artifacts must retain their conditional interpretation.

## Find the fitted artifacts

Paths below are relative to the sibling `LKT-outputs`, unless marked as cluster paths.
Legacy filenames remain unchanged; the stable family names above identify them.

| Family/result | Reference location |
|---|---|
| Baseline, matched cv1/seed1 | `gru-plasticity-cloze-20261107-v3/download/results/models/cv1__constant__seed1.rds` |
| Baseline five-fold report | `gru-plasticity-cloze-20261107-v3/RESULTS.txt` |
| Current training-only tracer records/metrics | `gru-tracer-training-only-20261113/submission` and `reports` |
| Known-trait ground truth | `gru-ground-truth-eight-input-20261113/reports/metrics.csv` |
| Training-only ground truth | `gru-ground-truth-training-only-20261113-r03/reports/metrics.csv` |
| Fixed source parameter files and zero-fit reuse proof | `cloze-source-calibration-reuse-20261113/reports` |

The current tracer model remains at the cluster path
`/project/ppavlik/gru-tracer-training-only-20261113/LKT-outputs/gru-tracer-training-only-20261113/models/cv1__anonymous_constant__seed1.rds`.
The two ground-truth model files are `models/eight_input_ground_truth.rds` under
their corresponding cluster run's `LKT-outputs/<run-name>` directory. Some local
folders contain reports/receipts only; do not assume a fitted model was downloaded.
Consult each submission record for exact data, protocol and model identities.

## Where the implementation lives

- [lkt-model-gru.R](R/lkt-model-gru.R): shared model constructor and orchestration.
- [lkt-gru-input.R](R/lkt-gru-input.R): inputs, training masks, scaling and vocabularies.
- [lkt-gru-recurrent.R](R/lkt-gru-recurrent.R): shared recurrent computation/training.
- [lkt-gru-adaptive-embeddings.R](R/lkt-gru-adaptive-embeddings.R): student-local item/skill vectors.
- [lkt-gru-plasticity.R](R/lkt-gru-plasticity.R): constant-rate output learning.
- [lkt-gru-prediction.R](R/lkt-gru-prediction.R): causal score/observe behavior and state.
- [lkt-gru-candidate-scoring.R](R/lkt-gru-candidate-scoring.R): read-only batched candidate scoring.

Keep all reusable code in this source tree, use the central release loader, and
write new fits/reports under `LKT-outputs`. Leave saved historical models, source
snapshots, submissions and results unchanged. No new fit, simulator integration,
release promotion or default change is implied by this reference guide.
