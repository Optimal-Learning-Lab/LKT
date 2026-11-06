# Cloze training-setting grid

This experiment changes four training settings in the existing released base:

| Setting | Current | Alternative |
|---|---:|---:|
| Learning rate | 0.001 | 0.0001 |
| Dropout | 0.10 | 0.05 |
| Batch size | 32 | 64 |
| AdamW weight decay | 0.0001 | 0.00001 |

The full crossing contains sixteen configurations, including the baseline.
Five existing participant folds and three seeds give 240 fresh fits. No
development-split fits are added. Input, between-GRU-layer and prediction-head
dropout move together; encoder and retrieval dropout remain zero.

All configurations retain the four 16-value input blocks, learned feature
encoders, three 32-unit GRU layers, 128-unit prediction head, one-head retrieval,
same-item OR same-skill retrieval filter, and trainable zero-initialized item
embeddings. The six inherited nonlinear history-feature constants remain fixed.
Each configuration has 46,257 parameters, and seed-matched initial weights are
checked for equality before training. The budget remains 100 epochs with
patience ten and the existing checkpoint restoration rule.

Within each outer fold, select the configuration with the highest validation
AUC averaged over seeds 1, 2 and 3; ties use original grid order. The primary
tuned-procedure result pools its held-out predictions across the five folds
within each seed, then averages the three AUCs. The sixteen fixed-setting CV
AUCs and marginal factor differences are descriptive. Held-out test AUC does
not select a configuration. The data and architecture were already inspected
in earlier experiments, so this nests the current grid selection only.

The driver freezes exact data, fold, reference-control and script checksums in
`study.rds`. Fit workers load the central release explicitly. Saved checkpoints
include actual settings, split masks and fingerprints. Each job replays all six
models' held-out histories, and the final report checks 240 contracts and all
fifteen baseline controls against the preceding experiment. Canonical saved
predictions are used consistently in AUC reports after replay agrees.

`compare-gru-training-grid.R` takes `ROOT OUTPUT ACTION PARTITION SHARD`, with an
additional reference-results directory for `plan`. Actions are `plan`, `verify`,
`fit`, `report`, and `check`. Big Blue runs forty array tasks (five folds by eight
shards), each containing six fits and using six one-thread workers. At most
sixteen array tasks run simultaneously. The setup and final-report jobs have
explicit successful-completion dependencies.

Artifacts and original submission records are under
`C:/dev/LKT-outputs/gru-training-grid-20261105`. The deployment archive contains the
central release descriptor and selected source archive; no package source or
release definition is changed for this experiment. Final reports include
training, validation and held-out CV AUC, selected settings, baseline agreement,
and epoch-cap counts. The lower learning rate or larger batch may need more
epochs; cap counts expose that limitation without changing the budget after
seeing held-out results.

The initial report job for the November 5 experiment failed because an R merge
returned a data frame where row indexing expected a data table. All 240 fits and
40 saved-weight replays had already succeeded. The maintained report calculation
now lives in `gru-training-grid-report.R`, with an explicit table conversion.
`audit-gru-training-grid.R` verifies the original frozen protocol, training
scripts, checkpoints and predictions and writes corrected reports to a separate
directory. It recomputes the report to check agreement. Historical deployment
files and the failed report log stay unchanged. The optional
`bootstrap-gru-training-grid.R` supplies a paired participant interval for the
validation-selected procedure versus baseline and independently checks its six
pooled AUCs against pROC.
