# Student-local GRU embeddings

`optional_adaptive_cols=character()` retains strict categorical inputs by default.
An explicitly optional adaptive column may be absent or NA: it supplies a zero
vector, creates no local entry, is excluded from local gradient clipping/updates,
and never matches retrieval by that missing label. Known or novel supplied labels
continue to receive distinct student-local vectors. The same implementation is
used for training, replay, ordinary continuation and batched scoring.
`LKTGRUOptionalIdentities(fit, "KC..Cluster.")` derives this input policy for an
already-adaptive fitted GRU without changing any weights, preprocessing, numeric
coordinates or original fitted artifact. Its new continuation identity prevents
accidentally reusing state from the strict parent model. This supports item banks
with unmarked skills while retaining the skill channel for labels supplied later;
it does not invent skill memberships or a shared unknown-skill identity.

`GRUModel(embedding_adaptation="per_student")` opts into a new training and
continuation capability. The default `"none"` retains ordinary GRU behavior,
including historical saved models that lack these settings.

`adaptive_embedding_cols` selects categorical tables (default `KC..Default.`).
Their fitted tables remain zero and are never used for identity lookup. Each
student instead owns a dictionary of vectors, initialized at zero when first
observed. Unseen strings remain distinct. Repeated candidate scoring allocates
no state and ignores supplied answers.

The model predicts before the answer. An observed answer advances recurrent
memory with the original current inputs and applies one SGD update to the
current item's local vector, with `embedding_learning_rate=0.01` by default.
The binary loss uses the configured positive/trial weights and gradient clipping.
There is no momentum or weight decay on local vectors. Shared weights do not
change during continuation. Adaptive state is plain R data and can be saved.

Training shuffles whole-student episodes, resets local vectors at each episode,
and retains them across truncated sequence chunks. It trains shared weights on
causal pre-answer losses using the existing optimizer. Local updates are first
order: gradients do not pass through the SGD update. Evaluation reuses that
update with dropout disabled. It predicts each validation/test answer before
using that answer to adapt to the next trial.

The supported architecture is the GRU with ordinary inference and its existing
encoder/retrieval modules. Optional `plasticity="constant"` retains the existing
student-local output adjustment in addition to local item vectors. Its shared
nonnegative output rate is learned across training students. Both local output
adjustments and item vectors reset for each student, carry across that student's
sequence chunks, and update only after the answer. The embedding gradient uses
the same pre-answer logit, including the prior output adjustment. Padded events
cannot change either local update state. Training and evaluation share this path.
Experimental inference, decay, fatigue, other plasticity modes,
warm starts, and non-GRU sequence units are explicitly rejected in this mode.
History grouping must be by student alone; no student identity embedding is
allowed. Ordinary models retain their existing supported combinations.

For simulator use, the tracer consumes item identity and observable response/time
history only. It does not infer named skills or a discrete membership table.
Ground truth may retain pretrained item/skill vectors and an explicitly supplied
mapping. The first simulator calibration is item-only because its input has no
separate skill mapping. See PolicyFactory2's new
`workflows/simulation/shared/examples/single_subject_config_gru_adaptive.R` example.
