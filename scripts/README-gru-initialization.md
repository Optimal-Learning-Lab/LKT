# Function-preserving initialization of additional GRU input connections

The ordinary first-layer input weights remain randomly initialized. Set
`input_to_all_layers=TRUE, additional_input_init='zero'` to initialize only the
new direct-input blocks in layers 2 and above to zero. All ordinary weights and
the random-number-generator endpoint match the corresponding ordinary stack.
The zero blocks remain trainable.

To expand an already trained ordinary model, also set
`initial_model_file='path/to/fitted-model.rds'`. This file contains the plain fitted
LKT object, not a research checkpoint wrapper. The fit copies every shared tensor
and zeroes only the extra input columns. Architecture, preprocessing, recipe and
training/validation/test row assignments must match. Recomputed preprocessing
summaries allow machine-scale rounding differences, and a full prediction replay
must reproduce the source model within the existing numerical tolerance.

The unmodified starting model is epoch 0 and participates in validation checkpoint
selection before optimization. Training creates a fresh optimizer; it does not
restore the source optimizer state. This is further training after the source
fit, so its total training budget is larger. The fitted model saves the resulting
weights and needs no initialization file for prediction or simulation.

The FGUI Initialization tab exposes both controls. Ordinary random initialization
remains the default, and `initial_model_file=NULL` fits from scratch.

## Controlled comparison

`compare-gru-initialization.R` runs these arms, each with retrieval off and one head:

| Initialization | Ordinary connections | Added input connections |
|---|---|---|
| Ordinary control | Random, then trained | Absent |
| Fresh zero expansion | Random, then trained | Initially zero, then trained |
| Trained zero expansion | Copied from matching trained control | Initially zero, then trained |

Six conditions, three seeds and the existing development plus five participant
cross-validation partitions give 108 fits. All use the augmented encoder model,
three timing values, three 32-unit GRU layers and a 128-unit prediction head.
Training settings and participant assignments match the preceding comparison.
Validation selects the restored epoch; no held-out metric selects an epoch.

```text
Rscript scripts/compare-gru-initialization.R ROOT NEW_OUTPUT plan development PREPARED_MODEL REFERENCE_RESULTS
Rscript scripts/compare-gru-initialization.R ROOT OUTPUT verify development
Rscript scripts/compare-gru-initialization.R ROOT OUTPUT fit PARTITION
Rscript scripts/compare-gru-initialization.R ROOT OUTPUT report development
Rscript scripts/compare-gru-initialization.R ROOT OUTPUT check development
```

The reference is the completed four-way input-connection comparison. Planning
freezes the prepared data, participant partitions and the 36 matching trained
control exports. All source checkpoint and export hashes are recorded. Historical
models and results are not modified. Warm-start paths are relative to the frozen
results directory so the same protocol works on the cluster.

The HPC scripts are `scripts/hpc/{setup,run,report}-gru-initialization.slurm`.
They install the archive selected by the central release descriptor, test it and
verify the frozen study before fitting. Reports independently replay every held-out
history and verify checkpoint contracts, training/validation/test AUC, epoch-zero
equivalence and fresh control reproduction. Cross-validation AUC pools held-out
predictions separately for each seed, then averages the three AUCs.

These are fixed-architecture comparisons on previously inspected data, rather
than a new independent architecture-selection test. The primary user-facing
comparison reports AUC; internal validation diagnostics remain in audit CSVs.
