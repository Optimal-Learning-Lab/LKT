# Direct input connections to every GRU layer

Set `input_to_all_layers=TRUE` to give each GRU layer a direct learned connection
from the original interaction. The default is `FALSE`. The FGUI Architecture tab,
saved configurations, generated calls, fitting and continued prediction share this
control. It is independent of `retrieval_heads`.

For the current augmented encoder model, the original interaction contains 64
encoded predictors and 16 encoded observed-answer values. Layer 1 receives those
80 values. With this option enabled, layers 2 and 3 each receive the preceding
layer's 32 output values plus the same original 80 values. Each upper layer has
separate weights for these additional input connections; the existing GRU biases
are retained. All three gates receive the new connections. This adds
`2 * (3 * 32) * 80 = 15,360` weights.

Between-layer dropout applies to the preceding layer's output. The original
interaction is reused with the input dropout already applied, so it is not
randomly re-encoded at each layer. Each layer retains its own recurrent state.
One-layer models have no upper layer and therefore gain no additional weights.

The current answer enters these update paths only after its prediction. The head
continues to use prior memory and current predictors; when enabled, attention
retrieval continues to supply the prediction head. This option does not implement
Ritter's gated reinstatement or change where attention retrieval is routed.

```r
GRUModel(input_to_all_layers=TRUE, num_layers=3L, hidden_size=32L,
         feature_encoder='mlp', encoder_hidden_sizes=32L,
         encoder_output_size=16L, head_hidden_sizes=128L,
         retrieval_heads=1L)
```

## Controlled comparison

`compare-gru-input-connections.R` crosses the two independent controls:

| Condition | Input to every layer | Retrieval heads | Parameter entries |
|---|---|---:|---:|
| standard | FALSE | 0 | 41,041 |
| retrieval | FALSE | 1 | 46,257 |
| all_layers | TRUE | 0 | 56,401 |
| combined | TRUE | 1 | 61,617 |

All four arms use the augmented model and three time inputs, including the
missing-spacing indicator. Three seeds and the six existing participant partitions
(development plus five cross-validation folds) yield 72 fresh fits. Training
settings are fixed; participant validation loss selects each restored epoch.
This is a fixed-architecture comparison rather than nested architecture selection.

```text
Rscript scripts/compare-gru-input-connections.R ROOT NEW_OUTPUT plan development PREPARED_MODEL REFERENCE_RESULTS
Rscript scripts/compare-gru-input-connections.R ROOT OUTPUT verify development
Rscript scripts/compare-gru-input-connections.R ROOT OUTPUT fit PARTITION
Rscript scripts/compare-gru-input-connections.R ROOT OUTPUT report development
Rscript scripts/compare-gru-input-connections.R ROOT OUTPUT check development
```

The HPC scripts are `scripts/hpc/{setup,run,report}-gru-input-connections.slurm`.
Both controls are fitted fresh under the same centrally selected release. Original
comparison records supply only the prepared-data and participant-split contracts.
Reports replay held-out histories from saved weights and verify every fit before
aggregating AUC and log loss. They compare each addition both alone and in the
presence of the other addition, using paired participant loss intervals.
