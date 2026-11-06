# Default GRU design

The argument defaults of `GRUModel()` describe the GRU design selected on
November 6, 2026. Explicit R arguments override individual defaults. Selecting
GRU in the settings editor uses the same constructor defaults. LibLinear remains
the overall default of `LKT()` and the model configurator.

[PNG](gru-base-network.png), [SVG](gru-base-network.svg), [PDF](gru-base-network.pdf).
The figure describes the design and contains no predictive-performance results.

## Structure

- Three GRU layers, each with 32 units; the original predictor/answer vector enters
  layer 1 only. Between-layer dropout is 0.1.
- Separate time, history-feature and response feedforward encoders: a 32-unit ReLU
  hidden layer followed by a 16-value linear output. Encoder dropout is zero.
- Item and skill-combination embeddings each have 16 values. Item tables start
  at zero and remain trainable; skill tables start random. Reserved rows stay zero.
- Concatenate the two predictor encodings and embeddings into 64 values. Input
  dropout is 0.1. After observing the response, its 16 encoded values produce the
  80-value interaction input to the first GRU layer.
- One retrieval head of width 32, with shared query/key projection, no learned
  attention-decay bias and no attention dropout. Queries and keys use the previous
  top GRU state plus all current predictors: 96 values. Retrieval values are
  completed post-answer top-layer states.
- Search the last 100 global completed events, allowing an earlier event when
  its raw item ID OR raw skill-combination ID matches. Normalize attention only
  over eligible events. An empty eligible set returns an exactly zero read.
- Concatenate the 32-value retrieval read and 64 current predictor values, then
  predict through a 96 -> 128 -> 1 head with ReLU and 0.1 head dropout. The sigmoid
  converts the final score to a correctness probability.
- Update memory only after observing the answer. Retrieval feeds prediction;
  all three recurrent states and the completed-event bank support continuation.
- Separate latent inference, Gaussian sampling, temporal KL, upper-layer direct
  input, sum fusion and a joint input encoder are disabled by default. Existing
  controls remain available as explicit settings in their owning interfaces.

The timing defaults consume recency, item age and mean prior spacing, plus an
availability flag for undefined spacing. Prepare the additional timing columns
with `LKTGRUTimeFeatures()` on the full exposure log before selecting scored rows.
LKT feature terms and their fixed nonlinear parameters remain explicit inputs
to LKT; the saved specification includes the exact terms used in this checkpoint.
Training participants alone determine vocabulary and scaling.

## Exact illustrated checkpoint

The reviewed EdNet seed-1 checkpoint and its data are preserved with hashes in
[the freeze artifact](../../../../LKT-outputs/gru-defaults-freeze-20261106/selection.json).
Its vocabularies contain 9,134 question IDs and 1,607 skill-combination IDs. The
embeddings therefore have 9,135 and 1,608 rows, including the reserved rows. There
are 185 individual skill tags in the dataset; these are not the 1,607 combination
categories used by the model. The illustrated instance has 215,233 parameter
entries. Vocabulary sizes and parameter counts change with the training dataset;
the default encoder, recurrent, retrieval and head widths stay as specified.

[model-specification.rds](model-specification.rds) is the unfitted editable
specification. [selection.json](selection.json) and [selection.rds](selection.rds)
identify the exact saved checkpoint, data, fitting protocol and hashes. They are
provenance records, not another source of constructor defaults. The reviewed seed
was retained directly; no selection among the remaining seeds was performed.

## Verification and reproduction

[The drawing script](../../../scripts/draw-gru-retrieval-network.R) checks the
stored weight dimensions and reproduces encoders, embeddings, all GRU layers,
attention and prediction with ordinary R arrays over 103 events. It checks
initial zero item embeddings, learned trained embeddings, the exact-ID filter,
empty matches, the 100-event window, answer causality, chunk continuation and
120 real held-out predictions. All differences are below 0.000001. Evidence:
[accuracy-check.csv](accuracy-check.csv), [weight-shapes.csv](weight-shapes.csv),
[provenance-and-verification.txt](provenance-and-verification.txt).

```text
Rscript scripts/draw-gru-retrieval-network.R C:/dev/LKT
Rscript scripts/open-gru-base-fgui.R C:/dev/LKT verify
Rscript scripts/open-gru-base-fgui.R C:/dev/LKT open
```

Opening the editor does not fit a model. Historical studies, checkpoints and the
earlier diagram are preserved; the earlier diagram is in the freeze artifact's
prior-figure directory. The central release descriptor remains the authority for
the active installed package. Source changes to defaults are checked in an
isolated verification build before any release promotion.
