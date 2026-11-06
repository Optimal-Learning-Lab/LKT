# Configurable attention retrieval

The selected one-head model is illustrated in the
[current network diagram](../docs/figures/gru-retrieval-one-head/gru-one-head-retrieval-network.png),
with [model details and numerical verification](../docs/figures/gru-retrieval-one-head/README.md).

Set `retrieval_heads=0` to keep the existing GRU. Set it to `1` for one-head
retrieval, or a larger number for multiple heads. All controls are in the FGUI's
**Retrieval** tab and survive saved configurations and generated R calls.

```r
GRUModel(
  # Keep the chosen embeddings, encoders, GRU and prediction head here.
  hidden_size=32L, num_layers=3L, head_hidden_sizes=128L,
  feature_encoder='mlp', encoder_hidden_sizes=32L, encoder_output_size=16L,
  retrieval_heads=1L, retrieval_size=32L, retrieval_window=100L,
  retrieval_fusion='replace', retrieval_decay='none'
)
```

| Control | Default | Meaning |
|---|---|---|
| `retrieval_heads` | 0 | Attention heads; zero turns retrieval off. |
| `retrieval_size` | 32 | Total attention width, divided evenly across heads. |
| `retrieval_window` | 100 | Maximum completed interactions retained; `NULL` means all. |
| `retrieval_dropout` | 0 | Training dropout on attention weights. |
| `retrieval_fusion` | `replace` | Replace the normal memory read, or `concat` both reads. |
| `retrieval_decay` | `none` | Plain attention, `exponential` position penalty, or `akt` score decay. |
| `retrieval_decay_init` | 0.1 | Initial positive decay rate per head. |
| `retrieval_share_qk` | `TRUE` | Share query and key projection weights. |
| `retrieval_bias` | `TRUE` | Include biases in the retrieval projections. |

Changing the number of heads at a fixed `retrieval_size` divides the same total
width differently. It does not multiply the projection width. The size must be
divisible by the head count. The output projection always returns `hidden_size`
values, keeping the prediction-head input width unchanged in `replace` mode.

## What is retrieved

The query combines the previous top GRU state with the current encoded inputs.
Each stored key describes a previous event using its pre-answer GRU state and
encoded inputs. Its value is the top GRU state after that event's observed answer.
Consequently, retrieval can choose among contextualized past memories while the
GRU continues to update normally. There are no additional attention encoders.

Each head learns query/key/value projections, applies scaled dot-product attention,
and combines the earlier values. The heads are concatenated and projected back
to the GRU's hidden width. An empty history yields exactly zero retrieved memory,
including when output projections have biases.

In the ordinary or encoder-only GRU, `replace` substitutes the retrieved vector for
the last hidden state in the prediction read; `concat` supplies both. With a separate
deterministic or Gaussian inference network, the same choice applies to the memory
input of that inference network. Recent-interaction evidence and current predictors
keep their existing routes, and retrieved memory never replaces the GRU update.

This is an AKT-inspired read mechanism added to LKT's GRU, not an implementation
of the complete AKT architecture. The optional `akt` decay follows the context-aware
attention-score method in [Ghosh, Heffernan and Lan (2020)](https://arxiv.org/abs/2007.12324)
and their [MIT-licensed code](https://github.com/arghosh/AKT).

## Decay and causality

`none` uses ordinary attention. `exponential` subtracts a learned positive rate times
interaction distance from each score. `akt` multiplies each score by an exponential
decay based on preliminary attention mass and interaction distance; the derived
distance is detached as in AKT. These distances are sequence positions, not seconds.
The existing recency, item-age and prior-spacing inputs remain available unchanged.

Current and future responses are masked before normalization. Only completed earlier
events can be retrieved. The rolling memory crosses backpropagation chunk boundaries;
its maximum length is controlled independently by `retrieval_window`. Bank tensors
are detached at training chunk boundaries together with the recurrent state.
The window limits which events can be retrieved, not information already carried
inside their GRU states. Unscored studies do not create answer-updated memories;
their contribution to the explicit timing features remains unchanged.

`LKTGRUState` stores portable ordinary R arrays for each history's bank.
`LKTGRUStep` commits it only after an observed answer. Pending trials and
`LKTGRUScore` candidate branches do not advance it, and different students/histories
never share a bank. Save the fitted model and state with `saveRDS`; create a fresh
`LKTGRURuntime` in each process.

## Prepare an existing specification

```text
Rscript scripts/prepare-gru-retrieval.R ROOT SOURCE_SPEC NEW_OUTPUT 1
```

This creates an unfitted, editable one-head specification using the existing model's
other settings. Opening it in FGUI exposes all nine retrieval controls. Old fitted
weights cannot stand in for a retrieval fit. Zero-head historical checkpoints keep
their original network and probabilities when loaded through the current package.

## Controlled performance comparison

`compare-gru-retrieval.R` trains fresh `off`, `one_head`, and `two_heads` arms.
It preserves the selected augmented encoder model, three GRU layers of 32 units,
128-unit prediction head, and all three time inputs, including the missing-spacing
indicator. Total attention width is 32 in both retrieval arms; the window is 100
completed scored events, fusion is `replace`, and attention decay is disabled.
The parameter counts are 41,041 without retrieval and 46,257 with either head count.

```text
Rscript scripts/compare-gru-retrieval.R ROOT NEW_OUTPUT plan development PREPARED_MODEL REFERENCE_RESULTS
Rscript scripts/compare-gru-retrieval.R ROOT OUTPUT verify development
Rscript scripts/compare-gru-retrieval.R ROOT OUTPUT fit PARTITION
Rscript scripts/compare-gru-retrieval.R ROOT OUTPUT report development
Rscript scripts/compare-gru-retrieval.R ROOT OUTPUT check development
```

The reference supplies data hashes and participant assignments, not fitted models.
Run `fit` once for each of `development`, `cv1`, ..., `cv5`. Three seeds per arm
produce 54 fits. The existing participant validation sets select the restored epoch
by log loss. Five-fold AUC pools out-of-fold predictions separately for each seed,
then averages the three AUCs. Paired log-loss intervals resample participants and
condition on these fitted models; they do not include uncertainty from refitting.
This evaluates a previously selected architecture rather than nested architecture
selection, and does not constitute a retrieval hyperparameter search.

The Big Blue setup, array and dependent reporting jobs are in
`scripts/hpc/{setup,run,report}-gru-retrieval.slurm`. The protocol freezes the central
release, research scripts, prepared data, and partitions. Each partition independently
replays held-out histories from saved weights before publishing its scores. The final
check verifies every fit's settings, training/validation/test rows, checkpoint
fingerprint, parameter count, metrics, and complete out-of-fold response coverage.
