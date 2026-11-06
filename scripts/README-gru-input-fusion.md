# Parameter-matched input combination comparison

The user proposed four alternatives to the current augmented encoder base:

1. Sum time and model-history vectors; concatenate item and skill as before.
2. Sum item and skill embeddings; concatenate time and model history as before.
3. Make both pair sums, then add the two results.
4. Make both pair sums, then concatenate the two results.

No extra learned fusion network, projection, gate, padding or normalization is
introduced. Input dropout is applied once after the declared combination.
Response encoding stays separate and is consumed only after prediction.

The base has 46,257 parameter entries. Integer representation widths are selected
before training to minimize the absolute parameter-count difference. Uncombined
blocks keep their 16-value widths; combined blocks use a shared larger width.
Both pair sums use the same width in alternatives 3 and 4. The 16-value response
encoder, 3 by 32-unit GRU, 128-unit prediction head, 32-value one-head retrieval,
optimizer, training budget, seeds and folds are fixed.

| Condition | Time/history widths | Item/skill widths | Combined input | Parameters |
|---|---:|---:|---:|---:|
| Current base | 16 each | 16 each | 64 | 46,257 |
| Time/history sum | 29 each | 16 each | 61 | 46,347 |
| Item/skill sum | 16 each | 25 each | 57 | 46,103 |
| Both sums, added | 40 each | 40 each | 40 | 46,065 |
| Both sums, concatenated | 27 each | 27 each | 54 | 46,425 |

All totals are within 0.42% of the base. Equal parameter count is approximate,
not exact. These are total entries in the parameter tensors, using the established
counting convention including reserved embedding rows. Every tensor is used in
the forward model; no dummy parameters are inserted to fill a budget. Matching
total size does not equate input dimensionality or parameter placement.

The original 144 item and 36 skill identities are retained. Trainable item
embeddings start at zero, skills start random. This experiment does not merge
feedback versions or add shared-content embeddings. The same-item OR same-skill
filter remains within the last 100 global scored events. Retrieval feeds only
prediction and still uses the full prior-state-plus-input cue.

Five conditions times three seeds times six participant partitions give **90
fresh fits**. The six partitions are the prior development split and five outer
participant folds, each with participant-separated validation. The main CV
result pools outer test predictions within each seed, then averages three AUCs.
No probabilities are ensembled. Every held-out history is independently replayed
from saved weights, and 18 fresh controls must reproduce the preceding zero-item
baseline predictions. Training and held-out AUCs use the same restored weights.

The four variants are compared with baseline; alternative 3 is additionally
compared directly with alternative 4. Width selection uses parameter counts
only. Epoch selection retains the previous validation criterion. Earlier
architecture development on these data makes the evaluation exploratory rather
than nested over the entire research process. Fixed engineered-feature constants
remain inherited; study events affect timing but do not update recurrent answers.

The public controls are `encoder_output_sizes` (named widths for time, history,
response), `numeric_fusion`, `categorical_fusion` and `input_fusion`. Default
concatenation preserves earlier checkpoints. Addition requires compatible widths,
MLP feature encoding, direct prediction and a full retrieval cue; incompatible
configurations fail explicitly. These controls round-trip through FGUI.

```text
Rscript scripts/compare-gru-input-fusion.R ROOT NEW_OUTPUT plan development PREPARED_MODEL REFERENCE_RESULTS
Rscript scripts/compare-gru-input-fusion.R ROOT OUTPUT verify development
Rscript scripts/compare-gru-input-fusion.R ROOT OUTPUT fit PARTITION
Rscript scripts/compare-gru-input-fusion.R ROOT OUTPUT report development
Rscript scripts/compare-gru-input-fusion.R ROOT OUTPUT check development
```

Drivers load the single central release through `scripts/load-current-lkt.R`.
Original fitted models, data, results and submission records remain unchanged.
