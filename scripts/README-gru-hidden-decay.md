# Cloze: stock GRU versus hidden-unit decay

`compare-gru-hidden-decay.R` freezes thirty fresh fits: the current six-input
stock GRU and the same model with hidden-unit DK decay, five unchanged
whole-participant folds and seeds 1-3. All original data, validation/test roles,
LKT feature constants, architecture, retrieval, training controls and validation
checkpoint selection remain matched. Only the decay flag, its explicit clock,
and three learned scalars differ (43,073 versus 43,076 parameters).

Driver arguments: ROOT OUTPUT ACTION TASK CANDIDATE_JSON CANDIDATE_LIBRARY
[REFERENCE]. Actions are plan, verify, smoke, fit, report and check; TASK is a
zero-based row of the thirty-task frozen protocol for fit. Each scheduler task
uses one CPU, avoiding shared parallel socket ports. The two bounded synthetic
one-epoch smoke fits are preflight checks, not research fits.

The stage script takes explicit source-build, reference and output paths and
uploads source text and hashes only. Scientific rows and folds stay on their
existing host and are copied read-only into the new experiment after SHA256
verification. The two intentional package samples are reconstructed from an
explicitly verified existing remote archive and must match the generated build
input's hashes. Candidate installation is explicit and is not an active release.

Preflight checks all ten fold/condition schemas, identical numeric preprocessing
and seed-matched initial shared network weights, and exactly three zero-initialized
extra parameters. Saved fitted models independently replay their held-out histories
through the maintained package, including DK and chunk-boundary handling.

`fits.csv` reports all thirty fits with train/validation/test AUC and log loss,
selected epochs, training caps and times. `parameters.csv` reports raw a/b/c,
baseline hidden decay rate and DK fading rate, and both half-lives in days for
the fifteen decay fits. Baseline hidden half-life is evaluated at DK=0; b is a
signed influence coefficient with no standalone half-life. Actual hidden decay
depends on the unit's current faded DK. Stock values are inapplicable (NA).

Held-out metrics use replayed probabilities, pooled across five folds within
seed and averaged across three seeds. Report/check independently recompute
phase and pooled AUC using pROC, log loss using dbinom, parameter transforms,
checkpoint identities and minimum-validation-loss restoration. No fit, seed or
condition is selected using held-out outcomes. Enabled chronological execution
consumes dropout randomness differently from fused stock execution. This is an
exploratory comparison on previously inspected Cloze data, not a new confirmatory
test. Historical outputs and the central active release are preserved.

The enabled model starts with raw a=b=c=0, corresponding to one-day baseline
hidden-state and DK half-lives. It is not initialized as an exact zero-decay
copy of stock. b=0 removes the initial DK modulation, not the baseline decay.

Scheduler placement can be adjusted for pending tasks without changing model
settings or restarting running fits; retain the original submission and append
the resource-change record. Fit times describe shared cluster execution and
are not isolated hardware throughput measurements. The collector requires all
thirty verified receipts, successful scheduler exits and the final report before
archiving. The independent local auditor binds results to the frozen local plan.
