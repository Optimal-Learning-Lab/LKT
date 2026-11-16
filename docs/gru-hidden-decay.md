# GRU hidden-unit decay and signed DK

Enable explicitly with `GRUModel(hidden_decay=TRUE,
hidden_decay_time_col='CF..Time.')`. The timestamp uses `time_units` and does
not add a numeric time predictor. The feature is also available for
`SequenceModel(unit='gru')`; other sequence units reject it. Disabled and older
saved models retain the ordinary GRU path.

Each history owns `h` and `dk` tensors of shape layers x batch x hidden units.
Both start at zero. Histories use `history_group_cols` (student ID by default).
The decay clock retains only the last committed timestamp. The model learns
exactly three additional scalars, `hidden_decay.a`, `.b`, and `.c`, shared across
layers, units and histories, all initialized to zero.

The default `hidden_decay_parameterization='amplitude'` uses a direct
nonnegative amplitude a, initialized to exactly zero. The ordinary optimizer
updates a and then projects it onto [0, infinity). Its one-sided derivative at
zero allows learning a positive rate; no threshold appears in the forward
equations. b and c also start at zero. b and c can have zero gradients until a
and then b become active. A trained a of zero is a valid stock-equivalent result.

For an elapsed interval d in days, a prior post-response activation h, and its
signed history D, the eventwise update is:

1. Fade history: F = exp(-softplus(c) d) D.
2. Compute the nonnegative rate: r = a exp(b F).
3. Decay: H = exp(-r d) h; record decay changes: K = F + (H - h).
4. Predict before observing the current response, using H and the existing
   encoder/head/retrieval/inference path.
5. After observing the response, run the existing GRU update from H to J.
   Commit h = J and D = K + (J - H), plus the timestamp.

`softplus(x) = log(1 + exp(x))`, evaluated stably. The baseline hidden half-life
at F=0 is log(2)/a (infinite at a=0); the DK half-life is
log(2)/softplus(c). Both use days. b has no independent half-life. Positive and negative h
both move toward zero; signed D does not need clipping or an absolute value.
The learned sign of b determines the direction of D's influence. Old
contributions fade, while new response and decay changes can reinforce or
cancel them. The interval's decay change affects subsequent rates, never a
recalculation of the same interval. This is an eventwise recurrence, not a
continuous-time differential equation; inserting committed intermediate
events can change its result.

Training differentiates through h and D within each existing truncated
backpropagation chunk, then detaches both while retaining their values.
Both restart with each new history traversal. The ordinary loss and optimizer
learn the starting network and three scalars. During testing all learned
parameters remain fixed, while per-history h and D evolve. This mechanism is
separate from optimizer weight decay.

Only real observations advance state. Candidate scoring branches temporarily;
an unanswered trial retains the existing pending-row rule and commits no
hidden/DK/time changes. Completing that trial recomputes from committed state
and commits the interval once. First observations have elapsed interval zero.
Missing, nonfinite or backwards timestamps are errors; tied times are allowed.

Existing missing-timing neutral inputs, availability indicators, response
handling, padding exclusion and retrieval are preserved. Stored retrieval
entries and historical inference context are not decayed. With the default
replacement retrieval, past entries can preserve information even as live
hidden units decay; correctness probabilities need not fall with elapsed time.
Amplitude models execute the chronological recurrence in a compiled TorchScript
scan, passing live GRU parameters to retain autograd. Encoding, the prediction
head, retrieval and inference operate over the existing whole chunk. Compilation
failure is an error. The scan runs GRUs one interaction at a time, so training
dropout draws can differ from the fused disabled path. Evaluation is deterministic
and agrees across full sequences, chunks and continuation. At a=0, shared-weight
evaluation matches the stock model exactly within floating-point precision.

For historical compatibility, `hidden_decay_parameterization='softplus'` keeps
the earlier rate r=softplus(a+b F) and its original execution path. Enabled saved
models without the parameterization field select this legacy definition on load.
Its three raw zeros imply one-day baseline hidden and DK half-lives, not zero
decay. Historical fitted values and predictions must not be reinterpreted using
the new amplitude equation. Disabled saved models retain the stock path.
