# Default GRU network

[Connected SVG](gru-default-network.svg) | [PNG](gru-default-network.png) |
[PDF](gru-default-network.pdf)

The simplified architecture was adopted as the GRU default on November 6, 2026
after its matched Cloze comparison. `GRUModel()` and the GRU settings editor use
these constructor defaults. The model configurator omits the global intercept
for GRU/Sequence unless explicitly overridden; its GUI offers a model-dependent
intercept default. `LKT()` itself continues to select LibLinear by default.

## Architecture

- One combined numeric MLP: 32-unit ReLU hidden layer and 32-value linear output.
  The illustrated recipe supplies seven inputs: recency, item age, mean prior
  spacing, spacing availability, student logitdec, student-item logitdec and
  student-item PPES. Other author-specified feature recipes have their own input
  widths, derived from the saved preprocessing schema.
- Separate trainable 16-value item and skill-combination embeddings. Items start
  at zero; skills start random. Reserved rows remain zero.
- Concatenate 32 numeric, 16 item and 16 skill values into `x(t)` of width 64;
  input dropout is 0.1.
- Predict before observing the answer. After prediction, concatenate the raw
  binary answer with `x(t)`, giving 65 values to the first GRU layer. There is no
  learned response encoder.
- Three GRU layers with 32 units each, first-layer-only original input and
  between-layer dropout 0.1.
- One 32-wide retrieval head with shared query/key map from the 96-value
  prior-state-plus-input cue. Match raw item OR skill-combination ID within the
  last 100 global completed events. Softmax only over eligible past events.
  Average raw post-answer top-layer states and apply one `32 -> 32` output map.
  An empty eligible set gives an exactly zero read.
- Concatenate that 32-value retrieval read with `x(t)` and predict through
  `96 -> 128 -> 1`, with ReLU, head dropout 0.1, sigmoid and clipping.
- Cache the pre-answer event; write the bank only after observing the answer.
  All three states and the bank carry forward. Training-only scaling/vocabularies,
  timing transforms and missing-spacing availability handling are unchanged.

Prepare additional timing columns with `LKTGRUTimeFeatures()` on the full exposure
log. LKT feature terms and nonlinear coefficients are explicit author inputs, not
hardcoded universal coefficients. The default global intercept is absent; network
biases remain. Explicit settings can override the defaults.

## Connected figure and counts

The figure contains no dataset-specific vocabulary count or fitted checkpoint.
Weight counts are on learned arrows; biases are listed at receiving outputs.
Unlabeled arrows pass information. Dashed arrows carry state/bank data into the
next trial; crossings without dots are not junctions. Numeric outputs and both
embeddings share the concatenation box. Attention arithmetic and the weighted
mean share one calculation box. The predictor branches and feedback loops remain
explicit.

For the illustrated seven-input recipe, fixed-size learned entries total 40,193,
plus `16(I + S)` learned embedding entries. `I` and `S` are learned item and
skill-combination category counts. Two reserved zero rows add 32 stored entries.
[Architecture settings](architecture-settings.rds),
[connection counts](connection-parameters.csv),
[connectivity audit](connectivity-check.json),
[layout checks](layout-check.csv) and
[provenance](provenance-and-verification.txt) accompany the figure.

Regenerate through the centrally selected release:

```text
Rscript --vanilla scripts/draw-gru-simplified-network.R C:/dev/LKT C:/dev/LKT/docs/figures/gru-default-model default
```

## Previous version and comparison

The [previous default figure and original specification](../gru-base-model/README.md)
remain in place, along with the [development connected alternative](../gru-simplified-consolidated/README.md).
The previous architecture remains available in the same maintained package:

```r
previous <- GRUModel(numeric_encoder = 'separate',
                     encoder_output_sizes = integer(),
                     response_encoding = 'mlp',
                     retrieval_value_mode = 'projected')
# Use interc = TRUE to reproduce its original global-intercept recipe.
```

`scripts/gru-previous-default.R` provides that explicit adapter. Previous saved
schema-1 models/specifications missing these selectors are decoded as the
historical separate/projected layout, with strict weight-shape validation. Saved
intercept choices are preserved. No historical weights or predictions are changed.

The immutable comparison artifacts are in
`C:/dev/LKT-outputs/gru-simplified-cloze-20261106/`. Its
[verified report](../../../../LKT-outputs/gru-simplified-cloze-20261106/RESULTS.md)
records thirty fresh fits using the same five folds, seeds and training settings:

| Model | Training AUC | Validation AUC | Held-out CV AUC |
| --- | ---: | ---: | ---: |
| Previous base | 0.864779 | 0.851560 | 0.854155 |
| New default | 0.866727 | 0.853554 | 0.855983 |

All fifteen fresh old controls reproduced their previous probabilities exactly;
all thirty held-out replays and independent AUC checks passed. This exploratory
combined-change comparison does not isolate the contribution of each change.
