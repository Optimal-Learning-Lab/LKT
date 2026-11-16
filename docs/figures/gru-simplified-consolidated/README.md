# Connected, consolidated view of the GRU alternative

[Zoomable SVG](gru-consolidated-network.svg) | [PNG](gru-consolidated-network.png) |
[PDF](gru-consolidated-network.pdf) |
[Original detailed view](../gru-simplified-alternative/gru-simplified-network.svg)

This is the same simplified GRU alternative with its network connections intact.
The revision merges boxes that describe parts of one calculation, while retaining
arrows that pass information from one calculation to another. Such arrows have
no numeric parameter label. Learned connections retain their weight counts;
biases are listed with the receiving outputs.

## Consolidation and explicit connections

- Numeric output, item embedding and skill-combination embedding are described
  inside the `32 + 16 + 16` concatenation box. Their incoming learned connections
  and the input-dropout order remain explicit.
- Shared Q/K outputs, scaled dot products, masked softmax and weighted averaging
  are one attention-calculation box. It has separate incoming query, key and raw
  value arrows. The bank box includes the exact-ID eligibility filter.
- Retrieval output, empty-match masking and concatenation with the current
  predictors share the prediction-input box. The probability box includes the
  final logit, sigmoid and clipping.
- Each GRU box includes its outgoing between-layer dropout where applicable.
  The arrows that carry states to storage remain visible.

The predictor bus explicitly branches to retrieval, the prediction head and the
completed-interaction input. Raw IDs reach the eligibility filter. The pre-answer
event and its IDs are cached until the post-answer bank write. Dashed feedback
arrows carry the retained bank, all three previous GRU states and the previous top
state into the next trial. Crossings without dots are not junctions. Ordinary
unlabeled arrows pass information; they do not indicate additional learned maps.

This corrects the preceding consolidated layout, which substituted repeated
variable names for several actual information-flow connections. The original
detailed figure is preserved. This changes the diagram only: model code,
architecture, tensor dimensions and parameter counts are unchanged.

## Verification

The renderer checks the architecture and parameter-count formulas, and SVG, PDF
and PNG text extents. The PNG is visually inspected. The declared drawing graph
is also audited in [connectivity-check.json](connectivity-check.json): all nodes
belong to one connected network, current predictors explicitly reach retrieval,
prediction and memory update, and the current answer can affect predictions only
through next-trial memory feedback. No current-answer path reaches the current
probability after next-trial edges are excluded.

[Architecture settings](architecture-settings.rds) and
[connection counts](connection-parameters.csv) match the detailed figure.
[layout-check.csv](layout-check.csv) records text bounds;
[provenance-and-verification.txt](provenance-and-verification.txt) records the
drawing sources and verified runtime. No model fitting is performed.

The [renderer](../../../scripts/draw-gru-simplified-network.R) and its
[connected layout](../../../scripts/gru-consolidated-layout.R) are maintained in
the source tree. Reproduce with the explicit verification runtime:

```text
Rscript --vanilla scripts/draw-gru-simplified-network.R C:/dev/LKT C:/dev/LKT/docs/figures/gru-simplified-consolidated C:/dev/LKT-outputs/gru-simplified-alternative-20261106/verification/candidate-library consolidated
```
