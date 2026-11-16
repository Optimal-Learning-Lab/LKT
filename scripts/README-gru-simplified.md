# Cloze comparison: simplified GRU versus previous base

Thirty fresh fits compare the previous separate-encoder base with the simplified
alternative: five existing participant outer folds and seeds 1, 2 and 3. Both
conditions reuse the exact prepared Cloze rows, participant validation assignments,
feature terms and six fixed nonlinear constants from the completed training grid.
Fresh baseline predictions must reproduce all fifteen historical controls.

The alternative removes the global intercept, replaces the two numeric encoders
with one `7 -> 32 -> 32` network, supplies the raw binary response and averages raw
retrieval states before one output map. All other settings remain fixed, including
three 32-unit GRUs, 16-value item/skill embeddings, the 128-unit head, one-head
same-item-or-skill retrieval over 100 global events, and zero item initialization.
Parameter counts are 46,257 versus 43,105; there is no parameter matching.

AdamW learning rate 0.001, weight decay 0.0001, batch size 32, BPTT length 100,
input/recurrent/head dropout 0.1, encoder/attention dropout zero, max 100 epochs,
patience 10, min_delta 0.0001 and validation-log-loss checkpoint restoration are
unchanged. No warm start or new hyperparameter search is performed.

Training and validation AUCs average fifteen restored fits per condition. Held-out
AUC pools all five outer folds within each seed, then averages the three seed
AUCs. Each participant is held out once per condition/seed; predictions are not
ensembled. Per-fold and per-seed metrics, epoch counts, parameter counts, baseline
reproduction, held-out saved-weight replay and independent pROC checks are retained.
This remains exploratory evaluation on data used in prior architecture development.

The maintained driver is `compare-gru-simplified.R`, with reporting in
`report-gru-simplified.R` and the existing `gru-encoder-study.R` helpers. The shared
fit helper now accepts the recipe's explicit intercept flag; previous callers
retain their original intercept setting. Every fit fingerprints its frozen
protocol and settings. Candidate installation and scientific scripts/data/roles
are hash-bound. No active release or previous study is changed.

Artifacts: `C:/dev/LKT-outputs/gru-simplified-cloze-20261106/`.
Cluster drivers: `scripts/hpc/{setup,run,report}-gru-simplified.slurm`.
`stage-gru-simplified-code.ps1` stages source text and hashes only;
`prepare-gru-simplified-remote.py` verifies and reuses existing remote inputs.
The driver takes `ROOT OUTPUT ACTION PARTITION CANDIDATE_JSON CANDIDATE_LIBRARY`
and adds `REFERENCE_RESULTS` only for `plan`. Actions are `plan`, `verify`, `smoke`,
`fit`, `report` and `check`. The explicitly selected validation candidate is built
from the maintained package source; it is not promoted into the central release.

## Completed comparison

All thirty fits and all held-out replays completed successfully. The alternative
has training/validation/held-out AUCs 0.866727 / 0.853554 / 0.855983, versus
0.864779 / 0.851560 / 0.854155 for the previous base. The held-out gain is
0.001827516 and is positive in all three seeds. All fifteen fresh controls
reproduced the historical predictions exactly. Local pROC verification checked
ninety phase AUCs, six pooled AUCs and all restored validation-minimum epochs.
The verified report is `C:/dev/LKT-outputs/gru-simplified-cloze-20261106/RESULTS.md`.

Only code and hashes were uploaded after explicit destination authorization;
data and controls already present on Big Blue were reused after exact hash checks.
The maintained Markdown renderer was subsequently corrected to format numeric
columns directly, avoiding premature character-matrix rounding in the final
decimal. Frozen fit records, source bundles, numerical CSV results and the original
remote report remain preserved. Re-audits can supply the explicit frozen study
root to `verify-gru-simplified-results.R` to check those original script hashes.
