# KTST-inspired comparisons within the GRU

This study borrows three ideas from Neubauer, Rudolph and Brefeld (2026),
*Principled Transformers for Predictive Performance in Knowledge Tracing*:
https://jedm.educationaldatamining.org/index.php/JEDM/article/view/961
It does not replace the GRU with their complete transformer architecture.

`retrieval_cue_cols` selects categorical representations for query/key matching.
An empty vector preserves the original cue: prior GRU state plus all current
predictors. Selecting the item and skill columns excludes the GRU state and the
numeric time/history towers from matching. All predictors still enter the GRU
and prediction head. Retrieved values remain the top GRU states after past answers.
For the selected model, cue width changes from 96 to 32, reducing the query
projection by 2,048 parameters. This is not a parameter-count-matched comparison.

`zero_init_embedding_cols` identifies trainable embedding tables that start at
zero. Here only the item table is zeroed; skill embeddings remain random. Ordinary
tables are allocated before zeroing, preserving other initial weights and RNG
draws. Saved trained weights replace initialization when loading a model for
prediction. This option does not zero recurrent input matrices or freeze embeddings.

`retrieval_decay='exponential'` subtracts a learned positive slope times the number
of intervening scored responses from attention scores, before softmax. The slope
uses softplus and starts at `1/256`, the released ALiBi slope for one head. Distances
are interaction counts, not elapsed seconds or counts of eligible matches.
`retrieval_decay_lr_multiplier=20` gives only that parameter a higher learning
rate. Other weights keep the original rate. Plateau scheduling applies its existing
factor and scalar minimum to both groups, and both actual rates are recorded.

These controls appear in FGUI and portable configurations. Empty cue/zero-init
column lists and multiplier 1 preserve the existing public defaults.

## Frozen comparison

`compare-gru-ktst-ideas.R` crosses all three binary changes: baseline, question
cue, decay, zero item embeddings, each two-way combination and all three together.
Eight conditions, three seeds and six participant partitions give 144 fresh fits.
Every condition retains the selected same-item-OR-same-skill filter, one head,
100-event window, original three-layer 32-unit GRU, augmented encoders, three
timing values, and 128-unit prediction head. There are no warm starts.

The existing five outer participant folds and independent participant validation
assignments are reused, plus the original development split. Training settings
remain fixed. Validation log loss selects each restored epoch, as in the preceding
GRU studies. User-facing results report training and held-out AUC. CV AUC pools
the five test folds separately per seed and then averages the three AUCs.

```text
Rscript scripts/compare-gru-ktst-ideas.R ROOT NEW_OUTPUT plan development PREPARED_MODEL REFERENCE_RESULTS
Rscript scripts/compare-gru-ktst-ideas.R ROOT OUTPUT verify development
Rscript scripts/compare-gru-ktst-ideas.R ROOT OUTPUT fit PARTITION
Rscript scripts/compare-gru-ktst-ideas.R ROOT OUTPUT report development
Rscript scripts/compare-gru-ktst-ideas.R ROOT OUTPUT check development
```

The reference is the completed retrieval-filter comparison. Only its data/fold
contracts and prior control predictions are imported; every model is fitted fresh.
The new baseline must reproduce all 18 prior skill-filtered controls. All test
histories are independently replayed from saved weights. Source models and results
remain immutable. Drivers use the central LKT release selector.

The authors' released AS2009 learned-ALiBi configuration uses ordinary rate
`0.00005` and `theta_lr=0.001`, a ratio of 20. We retain our ordinary rate `0.001`,
so the corresponding initial decay rate is `0.02`. Sources:
https://github.com/kainbr/kt_set_transformers/blob/development/pkg/config/reproduce/ktst/ablation_assist2009/reproduce_20.yaml
https://github.com/kainbr/kt_set_transformers/blob/development/pkg/model/utils/attentions/LearnableALiBiMonotonicAttention.py

These are fixed-architecture, exploratory comparisons on previously inspected data.
They are not nested evaluation of all earlier architecture choices, nor a full
reproduction of KTST's embeddings, contextualized question sequence or optimization.
