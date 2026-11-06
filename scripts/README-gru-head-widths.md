# Prediction-head width comparison

Our chosen working configuration, as of November 2, 2026, is **one 128-unit
ReLU head hidden layer followed by one output unit**, for all three input
families. The GRU remains three layers of 32 units. The user chose 128 to keep
the model smaller given the modest additional validation improvement at 256.

The completed search's automatic validation winner was 256 in each family;
that result and its frozen selection records remain unchanged. The 128-unit
checkpoints already exist for all three seeds in every family, so this choice
requires no retraining. The search procedure below documents the original
experiment rather than overriding the chosen working width.

The [exact chosen network diagram](../docs/figures/gru-head128/gru-network-overview.svg)
shows both learned embeddings, all numeric inputs, three GRU layers, the
128-unit head, and the predict-before-update timing. The
[full PDF](../docs/figures/gru-head128/gru-network.pdf) also expands the GRU gates.

This search changes only the hidden width of the plain GRU prediction head:
32, 64, 128 and 256 units, with one ReLU hidden layer and one output unit.
All three input families (matched engineered features, raw item/skill/time,
and augmented) use seeds 1, 2 and 3, giving 36 fresh fits. The recurrent
backbone remains three layers of 32 units; dropout and all training controls
come from the completed 144-item/36-skill inference experiment.

For each family, select the width with the lowest **mean validation log loss
across the three seeds**. An exact tie selects the smaller width. Validation
also determines early stopping for each individual fit. Scores average
individual model metrics; they do not average probabilities into an ensemble.

The runner requires a verified, completed inference experiment as its reference.
It preserves its exact prepared rows and whole-participant split and checks
frozen source/data hashes. No previous model checkpoint is reused. The 79/80/92
capacity-control widths are historical references, not additional candidates.

Set `LKT_GRU_LIBRARY` to a library containing LKT 2.0.0.9009 and Torch 0.17.0.

```
Rscript scripts/compare-gru-head-widths.R ROOT OUTPUT plan 12 REFERENCE
Rscript scripts/compare-gru-head-widths.R ROOT OUTPUT run 12 REFERENCE
```

`smoke` instead uses the first 12 scored responses per participant and one
epoch/seed, exercising all 12 family/width combinations. Use a separate output
folder. It checks the pipeline, not predictive performance.

After all fits finish, the summary independently recomputes every validation
score, freezes `selected_widths.csv`, and only then scores the chosen widths
and the width-32 baseline on test responses. It replays test predictions from
saved weights and reports paired participant bootstrap intervals. All candidate
validation scores, checkpoints, training histories, a readable HTML report and
verification evidence are retained.

The split was already inspected in earlier experiments. These are exploratory
held-out comparisons, not cross-validation or fresh confirmatory evidence.
The three seeds quantify initialization variability, not participant splits.

After downloading, verify the results against the independently frozen local plan
and render the validation curve using the standard plotting script:

```
Rscript scripts/verify-gru-head-widths.R RESULTS LOCAL_PLAN
python scripts/plot-gru-head-widths.py RESULTS
```

The plot shows mean validation loss and one initialization-seed standard deviation.
It saves PNG/PDF files and embeds the PNG in the local HTML report.
