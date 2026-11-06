# Selectable sequence memory

`SequenceModel(unit='gru'|'mamba', ..., layer_dropout=.1, unit_options=list())`
selects memory architecture while retaining LKT's feature construction, training,
head, retrieval and optional inference. `hidden_size` is the readable memory width,
not every internal state dimension. `GRUModel()` and `LKTGRU*` retain their existing
contracts, defaults, weight names and initialization order.

```r
model <- SequenceModel('mamba', hidden_size=32L, num_layers=3L,
  layer_dropout=.1, head_hidden_sizes=128L,
  unit_options=list(d_state=16L, d_conv=4L, expand=2L))
# Supply this adapter as LKT(..., model=model).
# Portable configurations: backend='Sequence', sequence_options=list(...).
```

Architecture selection is currently available through the R APIs. The model
builder retains its existing backend choices and rejects Sequence configurations
explicitly, including when loading one from disk. GUI architecture selection and
simulator integration are deferred.

## State contract

`LKTSequenceState`, `LKTSequenceRuntime`, `LKTSequenceScore` and `LKTSequenceStep`
provide continuation. Score branches every candidate independently without
committing observations. Step predicts before observing an answer and advances
only completed responses. Pending rows must be completed before advancing their
history. `predict_lkt` supports both architectures and retained state. Save models
and state with saveRDS; recreate the process-local runtime after loading.

Adapters own construction, initialization, readable memory and advancement.
Advancement returns post-answer representations and final opaque state. Shared
prediction shifts those outputs by one trial. Retrieval stores representations,
not internal tensors. State layouts declare the batch axis of every tensor leaf;
generic operations subset active histories, detach chunks, clone and pack/unpack
R arrays. GRU declares axis 2. Mamba declares axis 1 for convolution/SSM state and
last output. A future LSTM adapter can declare hidden/cell state separately,
without modifying shared loops. A test-only adapter with two evolving memories
on different batch axes exercises the real shared fitting, chunk detachment,
active-batch removal, retrieval/inference, independent scoring and serialized
continuation paths without modifying those loops.

Shared helpers retain historical `lkt_gru_` names where existing contracts use
them; state indexing lives in the adapter. GRU retains `gru.*` weights; Mamba uses
`sequence.*`. Old saved models are explicitly read through the GRU adapter.

### Internal adapter entry points

All tensors use the model's configured device and floating-point dtype. The
interaction has shape `[batch, time, encoded_predictor_width + encoded_response_width]`.
Its response portion encodes the completed binary response; without a learned
response tower that portion is one raw binary value. Readable memory has the common
`hidden_size` width, regardless of the unit's internal dimensions.

| Entry point | Contract |
| --- | --- |
| `lkt_sequence_build(input_size, settings)` | Construct the trainable unit. The returned module accepts `(interaction, state)` and returns `list(outputs, final_state)`. Outputs have shape `[batch, time, hidden_size]` and represent memory **after** each completed interaction. |
| `lkt_sequence_initial(settings, batch, like)` | Return the complete initial state, using `like` for dtype and device. |
| `lkt_sequence_read(state, settings)` | Return current readable memory with shape `[batch, 1, hidden_size]`, including for an empty history. |
| `lkt_sequence_advance(net, interaction, state, settings)` | Invoke the selected unit, retaining the same output/state contract for a single step or chronological chunk. |
| `lkt_sequence_layout(settings)` | Declare the batch axis for each tensor leaf. A numeric leaf is a one-based axis; lists mirror state structure and names exactly. |
| `lkt_sequence_state_map(state, settings, operation, indices=NULL)` | Apply `subset`, `detach`, `clone`, `pack`, or `unpack` to every leaf using that layout. A null state remains null. |

`subset` preserves the requested order and retains dimensions. `detach` ends a
training chunk's gradient graph while retaining values. `clone` gives independent
tensor storage. `pack` copies detached CPU values into ordinary R arrays;
`unpack` reconstructs tensors on the configured device with the configured dtype.
A unit must return all continuation data in its state rather than retain hidden
mutable caches in the module. Eval-mode advancement must agree across full
sequences, chunks and single steps, including from a nonzero initial state.

The fitting and replay loops remove finished batch members and detach chunks
through these operations. They do not inspect state fields, tensor ranks or
architecture layers. To add another unit, extend construction, options,
initialization, reading and layout at the adapter boundary. An LSTM can put hidden
and cell tensors in its state; it does not require another fitting or continuation
pipeline. The test adapter in `tests/sequence-unit-contract.R` exercises this
boundary with two evolving memories on different batch axes.

The shared prediction path reads memory before the current answer. Sequence
outputs are shifted by one completed interaction when constructing predictions;
retrieval banks contain those completed output vectors. Dropout can make training
trajectories depend on chunking, so equivalence checks use evaluation mode.

### Public APIs and saved configuration

New models use `model_name='Sequence'`, with one canonical settings representation:
common controls at the top level, `unit`, `layer_dropout`, and architecture
controls in `unit_options`. Portable configurations use `backend='Sequence'` and
`sequence_options` containing the public constructor arguments. Configuration
export generates a `SequenceModel(...)` call with the same settings.

Existing `backend='GRU'` configurations, `GRUModel()` constructor arguments and
saved GRU models remain valid. Explicit translation maps `gru_dropout` to common
`layer_dropout` and the remaining GRU-specific controls to `unit_options` when
creating a new Sequence model. Historical readers retain their GRU format rather
than rewriting saved artifacts. `LKTGRU*` wrappers retain their original fitted-model
validation and return classes; the generic APIs accept either fitted backend.

```r
runtime <- LKTSequenceRuntime(fit)
state <- LKTSequenceState(fit)
completed <- LKTSequenceStep(fit, state, completed_rows, runtime)
probabilities <- LKTSequenceScore(fit, completed$state, candidate_rows, runtime)
saveRDS(list(fit=fit, state=completed$state), "continuation.rds")
# In another process, load that object and construct a new runtime.
```

Score treats candidate rows as independent branches, ignores their answers and
does not commit state. Step commits only observed answers. An unanswered final
trial is retained as pending and must be answered before that history advances.
State and runtime identity checks reject objects belonging to a different fitted
model. Runtimes also reject use from another process. Public calls restore caller
R/Torch RNG state and Torch thread settings. Save fitted models and portable
states; process-local runtimes are recreated after loading.

## Native Mamba-1

The reference revision and attribution are in `inst/sequence-reference.json`.
The implementation includes projections, causal depthwise convolution, SiLU,
selective SSM, gating and output projection. LKT adds the learned input projection,
pre-RMSNorm residual stack and between-layer dropout. Unit options control state
width, convolution, expansion, delta rank/range/floor, epsilon and biases. A_log
and skip parameters have no weight decay. GRU warm-start expansion and direct
input to every layer are not Mamba options and are rejected.

The differentiable native reference scan processes state updates chronologically
with O(T) work. It is not a fused CUDA implementation and
requires no Python runtime. NumPy generates independent fixed-weight fixtures and
finite-difference gradients only. Test full/chunk/single-step inference, nonzero
state, save/load, independent candidates, causal answers and holdout isolation.

Build/test the candidate explicitly in `C:/dev/LKT-outputs/sequence-mamba/library`, using
existing `.codex-r-lib` Torch dependencies and ordinary user-library dependencies.
This temporary test build is not an active release. The central selector remains
unchanged. See the experiment guide for provenance checks and execution.

The independent fixture generator is `scripts/sequence-mamba-reference.py` and
its retained fixture is `tests/fixtures/sequence-mamba.json`. It computes NumPy
outputs, nonzero intermediate convolution/SSM states, and finite-difference
gradients for inputs and every parameter. NumPy and Python are verification tools
only; package fitting and inference execute entirely in R/Torch.
