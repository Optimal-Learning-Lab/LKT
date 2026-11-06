# Three time inputs

The time encoder now accepts recency plus two item-specific timing values:

* **Item age:** current timestamp minus this student's first exposure to this item.
* **Mean prior spacing:** mean of consecutive gaps between this student's previous
  exposures to this item. The current timestamp never enters this mean. With zero
  or one previous exposure, it is undefined and remains `NA`.

An exposure may be a test, drill or study. Compute `LKTGRUTimeFeatures()` on the
complete exposure log **before** selecting scored responses. Each supplied row
counts; no outcome labels are used to compute these values. Timestamp ties follow
original row order. Item keys use `KC..Default.` by default, independently of the
GRU's broader student-history grouping. All timing columns are seconds.

```r
full <- LKTGRUTimeFeatures(exposure_log)
scored <- full[full$Outcome %in% c('CORRECT', 'INCORRECT'), ]
model <- GRUModel(time_col='CF..Time.', feature_encoder='mlp',
  time_feature_cols=c('gru_item_age_seconds','gru_mean_prior_spacing_seconds'),
  time_missing_cols='gru_mean_prior_spacing_seconds',
  encoder_hidden_sizes=32L, encoder_output_size=16L,
  hidden_size=32L, num_layers=3L, head_hidden_sizes=128L)
```

All three time inputs receive `log1p`, then centering and scaling learned on
observed training values only. Undefined spacing stays missing in the data and
audit. An explicit availability indicator distinguishes absent spacing at the
network boundary; only the numerical tensor uses a neutral zero in the missing
coordinate after standardization. The time tower changes from **1 â†’ 32 ReLU â†’ 16**
to **(3 timing values + 1 availability indicator) â†’ 32 ReLU â†’ 16**.
Its downstream representation and all other layer widths stay fixed; the three
extra coordinates add 96 parameters. The supplied timing columns
are required during fitting, scoring and continuation; missing columns fail.

For online use, run the helper on the retained full exposure log plus the candidate
row and supply that row's two timing values. Commit the candidate to that exposure
log only when it actually occurs. Scoring alternative candidates does not create
exposures. The GRU state continues to retain answered recurrent interactions;
the full exposure log is the caller's separate record and also includes studies.

To prepare the existing validation-selected augmented encoder model with these
three time inputs, retaining the historical development participant split:

```text
Rscript scripts/prepare-gru-three-time-inputs.R C:/dev/LKT OUTPUT
```

This writes a new prepared dataset, a loadable FGUI model specification, settings
and a source-row timing audit. It does not fit a model or modify historical records.
Old checkpoints retain their one-input time tower because `time_feature_cols`
defaults to empty. The historical figure describes that fitted model; the new
three-input specification requires a new fit before reporting performance.

## Timing comparison

`compare-gru-time-inputs.R` compares the validation-selected augmented encoder
architecture using recency alone versus all three timing inputs. Both arms train
fresh under the current release with the same three seeds, dropout, optimizer,
early stopping and 128-unit head. There is no new hyperparameter search.

The experiment uses the original development participant split and the five
existing whole-participant outer folds: **36 fits** in total. Validation loss
selects each fit's stopping epoch. Held-out AUC and log loss are reported for
every seed and partition. Cross-validation metrics pool out-of-fold predictions
separately for each seed, then average the three metrics. A paired participant
bootstrap gives loss-change intervals conditional on the fitted models.

The architecture was chosen on previously inspected development data; this is
a fixed-architecture fold comparison, not a repeat of nested architecture
selection. Nonlinear LKT feature parameters remain fixed inherited constants.
Studies contribute only to the new timing inputs, as requested.

The driver delegates all fitting and prediction to the maintained installed LKT.
It verifies release/protocol hashes, preserves historical rows and fold assignments,
replays every test history from saved weights and checks that every response is
scored exactly once per condition and seed across the five outer test folds.
The setup, fit-array and report Slurm scripts are in `scripts/hpc/`; a deployment
artifact `deployment.env` contains values resolved by the shared release loader,
while R still verifies the unchanged frozen central release definition.

The `plan-ablation` action adds two conditions: **recency + item age** and
**recency + prior spacing**. It imports the completed recency-only and all-three-input
controls as immutable scientific records after checking the exact package release,
data, participant folds, model settings and seeds. Only the two new conditions train
again: 36 new fits plus 36 completed comparator fits. Imported weights are replayed
again before the four-condition report is published.

```text
Rscript scripts/compare-gru-time-inputs.R ROOT NEW_OUTPUT plan-ablation development PREPARED_MODEL COMPLETED_COMPARISON_RESULTS
```

The age-only addition has two numerical time inputs and 40,977 parameters.
The spacing-only addition includes its availability indicator, so its time tower
has three numerical inputs and 41,009 parameters. Undefined spacing remains missing in the data. The
augmented LKT/PPES inputs remain present in all conditions; these are ablations of
the explicit time-encoder inputs, not removal of all time-related information.

The current driver uses protocol schema 2. Older completed experiments retain their
original frozen protocols, scripts and outputs; they are consumed explicitly as
verified comparators rather than rerun with a different driver or package.
