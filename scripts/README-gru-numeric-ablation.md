# Cloze numeric-input ablation

`compare-gru-numeric-ablation.R` fits the current default GRU with fourteen
numeric-coordinate selections: seven single inputs and seven leave-one-out
selections. Two fresh controls use no numeric inputs and all seven inputs.
Five frozen participant folds and seeds 1-3 give 240 fits. The all-seven
control is checked against the saved simplified-model comparison.

`GRUModel(numeric_feature_cols=NULL)` keeps all constructed numeric inputs;
`character()` removes the numeric encoder; an explicit character vector selects
final column names before training-only scaling. The selection is serialized
and applied during prediction and continuation. No zero placeholder inputs or
separate network implementations are used.

The driver reads the feature order and recipe from the exact reference study.
Timing values are log1p-transformed; the prior-spacing availability flag is a
separate coordinate. Item age includes unscored exposures. Prior spacing uses
strictly earlier exposures and is undefined with fewer than two. The fixed
LKT predictors are student logitdec, student-item logitdec and student-item ppes.
Inherited nonlinear constants are unchanged.

The explicit candidate package is built from the maintained source and is not
an active-release promotion. Scientific inputs, previous results, checkpoints,
and the central release remain unchanged. Output is stored in the sibling
LKT-outputs directory using `docs/workspace-layout.md`.

Arguments:

```
Rscript scripts/compare-gru-numeric-ablation.R ROOT OUTPUT ACTION PART CANDIDATE_JSON CANDIDATE_LIBRARY [REFERENCE]
```

Actions: `plan`, `verify`, `smoke`, `fit`, `report`, `check`.
For `fit`, PART is `cvN:SHARD` with N=1-5 and SHARD=0-7.
Each shard runs six fits with up to six one-thread workers. SLURM setup verifies
hashes and installs the explicit candidate; reporting depends on all fit shards.

`stage-gru-numeric-ablation-code.ps1` creates a code-only bundle. The remote
reference path and a prior frozen remote-input specification are explicit
arguments. No learner rows, folds, predictions, fitted models or package samples
are uploaded. Already-remote inputs and package samples must match all frozen
hashes before setup. Submission IDs and receipts are preserved in the experiment
folder. The scripts never search alternate source, library or data locations.

Training and validation AUCs average the fifteen restored fits per condition.
Held-out AUC pools five outer test folds within seed, then averages the three
seed AUCs. The report includes both contrasts, per-seed changes, cap hits and
parameter counts. Parameter counts differ because inputs are actually removed.
This is an exploratory feature-ablation comparison, with no selection on
held-out outcomes. `verify-gru-numeric-ablation-results.R` independently checks
all 720 phase AUCs and 48 pooled AUCs with pROC, saved masks, minimum-validation-loss
checkpoint selection, input identity, exact folds and fifteen fresh controls.

Parallel worker ports are explicitly assigned by array task ID in the maintained
fit launcher (`R_PARALLEL_PORT=12000 + SLURM_ARRAY_TASK_ID`). R's parallel package
accepts this environment variable before cluster creation. This prevents
simultaneous array jobs from choosing the same random default port. A failed
pre-fit shard can be submitted by its array ID using the corrected launcher;
retain the original submission, append recovery IDs and hashes, and make the
report depend on completion of the original array and successful recovery.
The scientific driver, protocol, package, seeds and completed checkpoints do not
change for an orchestration-only recovery.

The independent auditor checks pooled AUC against the exact held-out replay CSV
probabilities used by the frozen reporter. It also checks every replay vector
against the fit's saved full-history prediction pass, and records both sets of
pooled AUCs in diagnostics. Float32 batching can introduce tiny differences and
change the ordering of near ties; comparing two different probability vectors
is not an independent check of the same AUC. The canonical-vector agreement
criterion remains 1e-12, and all saved phase AUCs are independently checked.
