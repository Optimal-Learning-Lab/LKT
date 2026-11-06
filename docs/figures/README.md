# LKT network diagrams

The current figure shows the **default GRU design** selected on November 6,
2026: separate feature encoders, filtered one-head retrieval, zero-initialized
trainable item embeddings, three 32-unit GRU layers and a 128-unit prediction
head. The figure shows structure only, with no predictive-performance results.
These are GRUModel argument defaults; LibLinear stays LKT's overall default.

- [View the current diagram](gru-base-model/gru-base-network.png)
- [Zoomable SVG](gru-base-model/gru-base-network.svg)
- [PDF](gru-base-model/gru-base-network.pdf)
- [Exact base specification and verification](gru-base-model/README.md)
- [Editable FGUI specification](gru-base-model/model-specification.rds)

Earlier figures describe the models used at those stages:

- [Earlier unrestricted one-head retrieval](gru-retrieval-one-head/gru-one-head-retrieval-network.png)
- [Augmented GRU with learned encoders and recency only](gru-encoder-augmented/gru-encoder-network.png)
- [Augmented GRU with a 128-unit head](gru-head128/gru-network.png)
- [Original GRU](gru-network.png)
