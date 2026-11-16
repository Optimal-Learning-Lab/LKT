# Simplified GRU alternative

[PNG](gru-simplified-network.png) | [Zoomable SVG](gru-simplified-network.svg) |
[PDF](gru-simplified-network.pdf) | [Previous design](../gru-base-model/README.md)

This is the November 6, 2026 structural alternative to the reviewed default GRU.
It is implemented in the maintained package source as explicit settings. Neither
`GRUModel()` defaults nor the centrally installed release are changed. The previous
figure, specification, fitted checkpoints and historical results are preserved.
The new figure is generic: it loads no dataset or fitted checkpoint and shows
architecture widths rather than sample-specific vocabulary sizes or performance.

## Structure and comparison

| Component | Previous reviewed design | Simplified alternative |
| --- | --- | --- |
| Numeric inputs for the illustrated feature recipe | Four timing coordinates and four history coordinates, including a constant intercept | Same four timing coordinates and three history features; no intercept |
| Numeric encoder | Separate `4 -> 32 -> 16` time and history networks | One `7 -> 32 -> 32` network, with ReLU in the hidden layer |
| Item and skill-combination embeddings | Separate trainable 16-value tables | Same |
| Current predictor vector | 64 values | Same: 32 numeric + 16 item + 16 skill |
| Observed response | `1 -> 32 -> 16` response network | One raw binary value |
| First GRU layer input | 80 values | 65 values |
| GRU memory | Three layers of 32 units | Same |
| Retrieval query/key | Shared `96 -> 32` map | Same |
| Retrieval values/output | `32 -> 32` value map, weighted mean, `32 -> 32` output map | Weighted mean of raw 32-value states, then one `32 -> 32` output map |
| Prediction head | `96 -> 128 -> 1` | Same |
| Dropout | Input, between-layer and head 0.1; encoder and attention zero | Same |

The combined encoder keeps the previous total numeric output width of 32, avoiding
an additional representation bottleneck. The seven inputs are recency, item age,
mean prior spacing, spacing availability, student logitdec, student-item logitdec
and student-item PPES. Three scalar history terms and their fixed nonlinear
coefficients are supplied by the caller's LKT recipe. Different recipes can have
different numeric input widths; the maintained implementation derives those
widths from the saved preprocessing schema.

Removing the intercept means `interc=FALSE` in `LKT()` or `intercept=FALSE` in an
`LKTModelConfig`. Network biases remain. Time transforms, availability handling,
training-only scaling/vocabularies, causal answer routing, filtering over the last
100 global completed scored events, exact zero reads for empty eligible sets and
chunk/state continuation retain their existing definitions. Item tables start at
zero and train; skill-combination tables start random. Reserved rows stay zero.

## Parameter labels

Connectors show trainable **weights**. Receiving boxes show **biases** separately.
Fixed arithmetic, concatenation, dropout, copying and state storage have zero
trainable parameters. Each of the three GRUs has 96 input biases and 96 recurrent
biases, for 192 per layer. Each weight matrix includes all three GRU gates.

The query and key use one shared map: 3,072 weights plus 32 biases, counted once.
`I` and `S` denote numbers of learned item and skill-combination categories. The
embedding connections therefore show `16 x I` and `16 x S` trainable entries.
Each table also stores a reserved zero row, excluded from those trainable counts.
[connection-parameters.csv](connection-parameters.csv) records numerical counts
for the fixed-size network connections.
Its `copies` column counts three recurrent matrices and one shared Q/K matrix.

The fixed-size part shrinks from 43,345 to 40,193 entries, saving **3,152**:

- 2,032 from the response encoder and its extra GRU input columns.
- 1,056 from one redundant retrieval projection.
- 64 from replacing the two numeric encoders and removing the intercept input.

Total learned entries are `40,193 + 16(I + S)`. Including the two reserved rows,
the stored tensor-entry count is `40,193 + 16(I + S + 2)`. The generator checks both
the previous and alternative formulas with multiple arbitrary vocabulary sizes.

## Functional meaning and validation

For binary responses with no encoder dropout, the old response network has only
two possible outputs: `f(y) = f(0) + y * (f(1) - f(0))`. Its contribution can be
absorbed into the first GRU input weights and biases. One-head attention with no
attention dropout also permits merging its value and output affine maps, retaining
the zero-memory mask. Tests convert random nonzero-bias networks and compare full
logits, states, attention and continuation-bank contents at `1e-12` tolerance in
float64. Different attention and state widths are included in that check.

Combining the numeric encoders changes the function class and has not been fitted
as a research comparison. The previous separate encoders are preserved for that
future comparison. Equivalent parameterizations also need not retrain identically
because optimization and weight decay operate on different parameters.

`tests/gru-simplified.R` additionally fits one tiny synthetic contract case and
checks the seven-input schema, all key tensor shapes, gradients, saved-weight
prediction replay, future-answer causality, immutable scoring, chunk continuation
and save/load. This smoke fit is not evidence of improved predictive performance.

## Use and reproduce

[The maintained alternative helper](../../../scripts/gru-simplified-alternative.R)
provides `gru_simplified_alternative()` for an adapter and
`gru_simplified_config(terms)` for an intercept-free configuration. Its explicit
settings are:

```r
GRUModel(numeric_encoder = 'combined',
         encoder_output_sizes = c(numeric = 32L),
         response_encoding = 'binary',
         retrieval_value_mode = 'raw')
# Supply to LKT(..., interc = FALSE), or use gru_simplified_config(terms).
```

These controls round-trip through the GRU settings editor. Incompatible combined
encoder widths, multiple-head raw retrieval and attention dropout with raw
retrieval are rejected explicitly. The older modes retain their existing names,
tensor shapes and allocation order.

[architecture-settings.rds](architecture-settings.rds) records resolved unfitted
architecture settings. It does not contain data, a feature recipe or fitted
weights. The [drawing script](../../../scripts/draw-gru-simplified-network.R)
requires a runtime containing these controls. With a verified central release:

```text
Rscript --vanilla scripts/draw-gru-simplified-network.R C:/dev/LKT C:/dev/LKT/docs/figures/gru-simplified-alternative
```

An optional third argument explicitly selects the isolated candidate library
produced for verification; no alternate installation is searched automatically.
The current figure was generated from that candidate. Verification-build inputs,
package-check logs and the candidate runtime are in
`C:/dev/LKT-outputs/gru-simplified-alternative-20261106/verification/`.
[provenance-and-verification.txt](provenance-and-verification.txt) records source
hashes, runtime identity and counting conventions. Text extents are checked before
export; [layout-check.csv](layout-check.csv) stores the geometry checks. PNG, SVG
and PDF are generated from the same drawing code.

## Completed verification

The hash-verified staged source built and installed successfully in the isolated
candidate library. All **28 regression scripts passed** under ordinary `R CMD
check` with `LKT_REQUIRE_TORCH_TESTS=true`; examples, vignette rebuilding and the
PDF manual were excluded. Final status: **0 errors, 1 warning, 1 note**. The
warning records dependencies built with R 4.5.2 while this runtime is R 4.5.1;
the note identifies an existing unqualified `tail()` in the joint-encoder branch.
The initial attempt stopped before tests because the inherited `C.UTF-8` locale
is unavailable on this Windows R. The completed check used the valid `C` locale;
both check records remain preserved.

The completed log is
`C:/dev/LKT-outputs/gru-simplified-alternative-20261106/verification/check-C/LKT.Rcheck/00check.log`.
The candidate is a verification artifact, not the active release. No research
benchmark, HPC job or release promotion was performed. Existing default figure
files are unchanged. The PNG was visually checked; SVG, PDF and PNG box text
extents passed automatic bounds checks.
