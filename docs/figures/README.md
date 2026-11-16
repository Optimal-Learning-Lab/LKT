# LKT network diagrams

The current figure shows the **six-input default GRU design** adopted on
November 7, 2026: item age, prior mean spacing, spacing availability and three
LKT history predictors. The previous-response gap is omitted. The architecture
retains the November 6 simplifications: one combined numeric encoder,
raw binary answers, filtered one-head retrieval with a single output map,
zero-initialized trainable item embeddings, three 32-unit GRU layers and a
128-unit head. The connected figure groups parts of one calculation and preserves
all information-flow arrows. LibLinear stays LKT's overall default backend.

- [Current connected default figure (SVG)](gru-default-model/gru-default-network.svg)
- [PNG](gru-default-model/gru-default-network.png) | [PDF](gru-default-model/gru-default-network.pdf)
- [Default architecture, previous settings and comparison reference](gru-default-model/README.md)

The [preceding seven-input default and provenance](gru-default-seven-input-20261106/README.md)
remain preserved in full.

The preceding separate-encoder default and its original checkpoint/specification
remain preserved for reference:

- [Previous default diagram](gru-base-model/gru-base-network.png)
- [Zoomable SVG](gru-base-model/gru-base-network.svg)
- [PDF](gru-base-model/gru-base-network.pdf)
- [Exact base specification and verification](gru-base-model/README.md)
- [Editable FGUI specification](gru-base-model/model-specification.rds)

The development-stage [simplified alternative](gru-simplified-alternative/README.md) has its own
[parameter-labeled diagram](gru-simplified-alternative/gru-simplified-network.svg),
with a combined seven-input numeric encoder, raw binary responses, a single
retrieval output map and no intercept predictor. The current default shares that architecture and drops its previous-response
gap input; these development figures and their original provenance remain preserved.

The same alternative also has a [consolidated view](gru-simplified-consolidated/README.md)
that groups parts of the same calculation, preserves information-flow arrows,
and puts numeric parameter labels only on learned links.
[SVG](gru-simplified-consolidated/gru-consolidated-network.svg) |
[PNG](gru-simplified-consolidated/gru-consolidated-network.png) |
[PDF](gru-simplified-consolidated/gru-consolidated-network.pdf).

Earlier figures describe the models used at those stages:

- [Earlier unrestricted one-head retrieval](gru-retrieval-one-head/gru-one-head-retrieval-network.png)
- [Augmented GRU with learned encoders and recency only](gru-encoder-augmented/gru-encoder-network.png)
- [Augmented GRU with a 128-unit head](gru-head128/gru-network.png)
- [Original GRU](gru-network.png)
