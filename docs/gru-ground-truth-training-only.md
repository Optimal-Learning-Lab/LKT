# Training-only evaluation of the eight-input GRU

The model retains its original six history inputs plus the supplied student
ability and item/skill easiness intercepts. Item and skill vectors remain
separate from those scalar inputs and reset to zero at each new student in
training, validation and testing. No new skill-intercept input or layer is added.

Use the explicit seventh `--training-only` argument with
`scripts/gru-ground-truth-eight-input.R`. The original full-calibration experiment
and its artifacts are preserved; this mode uses a new configuration and output
directory. Its configuration is `scripts/gru-ground-truth-training-only.json`.

## Separate calibration and reusable fixed parameters

Training-only source calibration is now a separate `calibrate` action. It looks
up an exact matching saved calibration and fits once only when none exists.
`prepare`, `smoke`, `fit` and `report` never optimize or fit LKT. They require the
saved artifact and load its fixed PPES/PPEF values and existing student/item
coefficients. Even a new GRU output folder or neural seed reuses that artifact.
Missing artifacts fail preparation with instructions to run calibration first.

Pass the shared store as the eighth argument after `--training-only`. Locally the
default is the source root's sibling `LKT-outputs/cloze-source-calibrations`;
cluster launch manifests explicitly declare a shared store outside individual run
folders, such as `/project/ppavlik/LKT-outputs/cloze-source-calibrations`.
The updated launcher calls `plan`, `calibrate`, `prepare`, `smoke`, `fit`, `report`;
on a cache hit the calibration step only validates and loads files.

Each calibration directory is keyed by data/training-row identity, LKT source
specification, source calibration controls/seed and verified fitting runtime.
GRU settings and neural seed are excluded. A changed training fold cannot reuse
another fold's coefficients. Existing corrupt/incompatible artifacts fail rather
than silently trigger re-estimation. A lock prevents concurrent cache misses from
starting duplicate optimizations; completed artifacts publish atomically.

The directory contains `calibration.rds`, `fixed_parameters.json`,
`fixed_parameters.csv` and a checksum receipt. The parameter files explicitly
record the four PPES and four PPEF numeric values; the saved configuration has all
eight nonlinear parameters fixed and zero free nonlinear parameters. The fitted
intercepts are reused too, avoiding a redundant linear refit on unchanged data.

The standalone entry point `scripts/cloze-source-calibration.R ROOT REFERENCE
STORE ensure|verify` handles precomputation/reuse. Its `import RECORD PROTOCOL`
action publishes an already converged training-only result without fitting.
Import verifies the exact data, training mask, source configuration, runtime and
model/protocol hashes. The current running GRU's source snapshot is left intact.

Verification imports the existing converged calibration, then performs repeated
lookup and prepares two different GRU output folders with both LKT fitting entry
points instrumented to stop on any call. No additional scientific fit is run.

The original whole-student cv1 folds remain: 305 training students, 77 validation
students and 96 test students. Only the 35,125 training responses enter the one
initial LKT PPES/PPEF nonlinear and intercept fit. Nonlinear parameters are then
saved as fixed values. There is no Wald fitting in this accuracy experiment.
The six history-feature parameters reuse the immutable training-only control
calibration from the original comparison. Its exact model/protocol hashes and
training row mask are checked; full-data control calibrations are rejected.

Training students receive their training-fitted ability intercepts. Every
validation/test student receives the same fixed unweighted mean of those
training-student intercepts. This is an explicit population-prior policy, not
missing-value substitution. The ability input stays fixed; the GRU's local state
learns from that student's previous observed answers. No prefix is used to fit
a held-out intercept and no responses are removed. All evaluated items must have
training-calibrated easiness; unknown item intercepts fail explicitly.

Skill labels are grouping metadata. There is no population-fitted skill-vector
table in this version. Numeric scaling and categorical vocabularies fit on
training rows only; both learned identity tables must remain zero. Local skill
and item state updates occur only after an answer and never carry across students.
Shared weights train on training students, validation chooses the stopping epoch,
and test students do not select any parameters or epochs.

Before neural fitting, poison all validation/test response labels and perturb
their supplied numeric values; fitted scaling/vocabularies must remain identical.
Trait-attachment tests require held-out ability to equal the training prior and
reject held-out student coefficients. Post-fit checks change future responses
and require all predictions up to those responses to remain unchanged, alongside
the existing reset, renaming, batch/reference, read-only scoring, replay and
save/load checks. The complete-data calibration result AUC 0.8752 remains a
separate conditional result and is not reused as a leakage-free benchmark.

The failed initial training-only run stopped at its nonzero convergence gate
before saving the LKT return; the exact optimizer code/message was therefore lost.
The corrected runner saves `diagnostics/source_calibration.rds` and a concise JSON
report before applying the gate, including the optimizer code/message, objective,
evaluation counts, warnings, controls and exact continuation identity. Failed
returns cannot become calibration models and are not automatically retried.
Converged saved returns can continue preparation without repeating optimization.

The first diagnostic rerun raised the iteration ceiling from 1,000 to 3,000 but
left the nonlinear stopping precision unchanged. That does not address an overly
strict stopping test. The corrected configuration instead sets `source_factrv`
to 10,000,000, R's standard L-BFGS-B precision factor, and restores the 1,000
iteration ceiling. The previous factor of 10,000 required 1,000 times smaller
relative objective reductions before stopping. The linear epsilon stays 1e-12.
The optimizer must still report convergence; nonzero codes are not accepted.
An actual line-search/numerical error must be diagnosed from the saved report
rather than assumed to be resolved by this tolerance change.

Training-only data, no-Wald accuracy calibration and the GRU architecture remain
unchanged. New calibration controls enter the protocol and continuation identity.
A fresh experiment directory preserves the original and diagnostic reruns and the
completed six-input tracer. The documented optimizer control is described in
[R's optim manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/optim.html).
