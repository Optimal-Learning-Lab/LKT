# Independent batched GRU candidate scoring

`LKTGRUScore(fit, state, candidates, runtime, method="batched")` opts into a
read-only scoring path. `method="reference"` remains the default. Models,
training, defaults, serialized fits/states, and `LKTGRUStep()` are unchanged.
`candidate_batch_size=256L` bounds the neural query batch and can be set explicitly.

Each candidate branches from the same committed history. Candidates are batch
members, never sequence steps. Their supplied answers are ignored. Scoring does
not allocate unseen identity entries, advance recurrent/retrieval state, create
pending trials, update output weights, or build an embedding-gradient graph.
Unknown item and skill strings retain their existing distinct local lookup and
retrieval-matching semantics. Use `LKTGRUStep()` once after observing the chosen
item's answer; both embedding SGD and constant output learning remain there.

The scorer converts each history's committed state once, prepares retrieval
key/value projections once, and reuses them across all query chunks. Attention
math, encoders and prediction-head parameters are shared with the ordinary
network. It skips recurrent transitions and next-context construction altogether.
Temporary derived tensors belong to the scoring call, never the fitted model,
worker runtime or serialized student state. There is no persistent cache to
invalidate or recover.

Identical effective queries (numeric inputs, actual categorical vectors, and
retrieval eligibility) are evaluated once and scattered back to their original
positions. This preserves exact ties when dense arithmetic otherwise gives a
batch's last row a slightly different rounding result. Hashes index these
temporary queries, and actual input equality is checked before reuse. No identity
entries are merged: separate unfamiliar labels still receive separate updates
when their respective answered trials are observed. No probability rounding is
introduced, and the simulator's exact-tie selection rule is unchanged.

Supported models are GRUs with ordinary inference, with/without encoders and
retrieval, and none/constant output plasticity. Student-local item and skill
embeddings are supported. Experimental state/inference mechanisms fail explicitly
when batched scoring is requested; selecting reference scoring retains their
existing implementation. There is no automatic method substitution.

Engineered candidate inputs currently support additive `logitdec`, `ppes` and `lineafm`
terms, fixed nonlinear parameters, no interactions or identity intercepts. This
covers the six-input Cloze tracer. The existing feature kernels evaluate disjoint
component histories, reusing identical queries instead of rebuilding every
student-history feature for every item. Unsupported recipes fail explicitly.
Frozen numeric column selection/scaling and raw categorical encoders are retained.

`LKTGRUCandidateTimeFeatures(history, candidates)` computes independent item-age
and mean-prior-spacing queries from the complete committed exposure log. It
includes study exposures when supplied in that log. Preparing timing values from
response rows alone has the same exposure limitation as the ordinary helper.
No candidate is counted as another candidate's earlier exposure.

The shared PolicyFactory2 adapter exposes
`pf_with_candidate_scoring(bundle$tracer, batch_size=256L)`. This adds an explicit
versioned performance extension to that role and its checkpoint identity; old
roles/bundles retain their representation. The shared trial loop uses one batch
call only for an opted-in session. This does not enable the separate Cloze tracer
in simulator configurations or change the simulator's skill-mapping contract.

Validation uses native package tests and saved-model benchmarks through
`scripts/benchmark-gru-candidate-batching.R` and its Slurm wrapper. Baselines are
captured with the declared prior release, not reconstructed using candidate code.
Checks include old-release scores, candidate permutation/chunks, supplied-answer
independence, selected-trial prediction, serialization/runtime recreation,
unseen labels, state/RNG/shared-weight isolation, and unchanged ordinary paths.
Scientific data, logs, fitted models and benchmark reports stay in LKT-outputs.
