# Filtered attention retrieval

Set `retrieval_filter_cols` to identity columns that must match before an earlier
observation can receive attention. Matching uses OR across these columns. Each
history already belongs to one student, so the full rule is the same student AND
any matching identity. The default `character()` preserves unrestricted retrieval.

```r
GRUModel(retrieval_heads=1L,
         retrieval_filter_cols=c('KC..Default.','KC..Cluster.'))
```

The filter applies before softmax within the existing `retrieval_window`. A window
of 100 still means the last 100 completed scored events, rather than the last 100
matches. Use `retrieval_window=NULL` explicitly for an unlimited bank. No matching
event gives exactly the same zero read as an empty bank; no unrelated event is
substituted. The GRU continues to process the complete student history.

Filtering requires nonmissing, nonempty identity columns. Exact UTF-8 identity
strings travel beside the learned retrieval keys and stored hidden states. They
are not encoded through the embedding vocabulary: two unseen item names must stay
distinct even when both receive the unknown embedding. Identities are retained
across backpropagation chunks, student switches, candidate scoring and RDS saves.
They add no trainable weights. The FGUI Retrieval tab exposes the column list.

The stored memory value is still the top GRU hidden state after that past answer.
It summarizes the student's preceding history, including other items and skills.
The filter selects which past observations to consult; it does not make the
stored states item-specific or skill-specific.

## Controlled comparison

`compare-gru-retrieval-filter.R` fits unrestricted, same-item and same-item-OR-same-
skill retrieval. Every arm uses the original first-layer-input architecture, one
32-value attention head, a 100-event window, three 32-unit GRU layers, a 128-unit
prediction head, the augmented encoders and all three timing values. There are no
warm starts or added recurrent input links.

The bundled prepared dataset has 144 items and 36 skills, with exactly one skill
per item. Thus same item OR same skill is equivalent to same skill here. A separate
skill-only arm would duplicate the same experiment and is not fitted.

The existing development split and five outer participant folds, each with its
separate participant validation set, are frozen before fitting. Three conditions
and three seeds on six partitions give 54 fresh fits. Training settings are fixed;
validation log loss selects each restored checkpoint. User-facing reports show
training and held-out AUC. CV AUC pools all five test folds within each seed and
then averages the three seed AUCs; it does not ensemble predictions.

```text
Rscript scripts/compare-gru-retrieval-filter.R ROOT NEW_OUTPUT plan development PREPARED_MODEL REFERENCE_RESULTS
Rscript scripts/compare-gru-retrieval-filter.R ROOT OUTPUT verify development
Rscript scripts/compare-gru-retrieval-filter.R ROOT OUTPUT fit PARTITION
Rscript scripts/compare-gru-retrieval-filter.R ROOT OUTPUT report development
Rscript scripts/compare-gru-retrieval-filter.R ROOT OUTPUT check development
```

The completed input-connection study supplies the prepared-data/participant-fold
contracts and prior unrestricted-control predictions solely for reproducibility
checks. Fitted weights are not imported. Checkpoint hashes and control predictions
are frozen in `data/reference-controls.rds`. Every test history is independently
replayed, and all saved masks, settings, fingerprints and metrics are verified.
The new unrestricted controls must reproduce the previous predictions within the
existing numerical tolerance. Original source models and results remain immutable.

Cluster drivers are `scripts/hpc/{setup,run,report}-gru-retrieval-filter.slurm`.
They use the centrally selected LKT archive and the existing verified container.
The study uses previously inspected data and a fixed architecture; it is not a
nested evaluation of the complete architecture-development process.
