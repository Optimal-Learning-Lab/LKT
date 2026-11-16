# Prediction-head dropout comparison

Vary only `head_dropout` over 0.01, 0.05, 0.10 and 0.20 in the completed Cloze
training-grid baseline. Reuse its exact prepared data, five participant folds,
inner validation assignments, fixed feature coefficients and seeds 1-3.
All 60 fits are fresh. Input and between-GRU-layer dropout remain 0.10;
encoder and retrieval dropout remain zero. The baseline architecture, optimizer,
training budget and validation-log-loss checkpoint rule are unchanged.

The driver `compare-gru-head-dropout.R` accepts `ROOT OUTPUT ACTION PARTITION SHARD`
and a completed training-grid results path for `plan`. Actions are `plan`, `verify`,
`smoke`, `fit`, `report` and `check`. The real benchmark uses ten Slurm array tasks,
each running six one-thread CPU fits, with setup/report success dependencies.
The smoke action fits four tiny synthetic one-epoch models in a separate folder.

The plan freezes data, fold, control, release and script hashes. Verification
checks identical seed-matched initial weights and 46,257 parameters at all rates.
Every saved model is independently replayed on held-out histories. Reporting
checks all 60 fit contracts and all 15 fresh 0.10 controls against the completed
baseline. The shared reporter still supports the original 240-fit grid.

Lead results with each fixed rate's seed-averaged pooled five-fold held-out AUC
and its difference from 0.10. Report training/validation AUC and epoch-cap counts.
A secondary validation-only selected-rate analysis is retained; test AUC never
selects a rate or changes defaults. These data and the architecture have already
informed earlier studies, so conclusions are exploratory.

Artifacts: `C:/dev/LKT-outputs/gru-head-dropout-20261106`. This experiment does not
modify the package architecture, active central release, prior fits or results.
