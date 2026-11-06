# Augmented GRU with one-head retrieval

This is the one-head model selected after the completed retrieval comparison.
It includes recency, item age, and mean prior item spacing with an availability
indicator; learned time/history/response encoders; item/skill embeddings; three
32-unit GRU layers; and a 128-unit prediction head. Total parameter entries: 46,257.

The figure follows the order of one trial:

1. Encode the available predictors into `x(t)`, a 64-value vector.
2. Form a 96-value query input from previous top-layer memory and `x(t)`. One
   32-value attention head reads the participant's previous 100 completed events.
3. Combine 32 retrieved values with `x(t)` and predict through `96 -> 128 -> 1`.
4. Observe the answer, encode it into 16 values, and update all three GRU layers.
   Append the pre-answer event record and post-answer top state for later retrieval.

The bank stores raw event records and GRU states. Shared query/key projections map
event records from 96 to 32 values; value and output projections each map 32 to 32.
All projections include biases. The retrieved memory replaces the direct previous
state in the prediction head. The GRU consumes the current predictors and encoded
answer. Attention decay and attention dropout are zero in this configuration.

The representative checkpoint is development seed 3, epoch 27, selected from the
three one-head seeds by validation log loss. The drawing does not select a seed
using test performance. Its weights and exact source hashes are recorded in
[provenance-and-verification.txt](provenance-and-verification.txt).

[The drawing script](../../../scripts/draw-gru-retrieval-network.R) checks saved
tensor dimensions and independently reproduces the full network with ordinary R
arrays across 103 events. It verifies attention masking, the rolling 100-event
window, continuation across a chunk boundary, and causal prediction. It also
replays 120 real held-out responses. Maximum differences are recorded in
[accuracy-check.csv](accuracy-check.csv); tensor dimensions are in
[weight-shapes.csv](weight-shapes.csv).

This is a historical figure. Its drawing-script hash is recorded in the
provenance file. The maintained drawing script now generates the
[current base model](../gru-base-model/README.md), which adds the retrieval filter
and zero item initialization. The original diagrams and verification records in
this directory are retained unchanged.
