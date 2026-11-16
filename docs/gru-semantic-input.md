# Frozen semantic completed-response input

`GRUModel(semantic_input = NULL)` preserves the existing model and RNG behavior.
An explicit `semantic_input` list enables the adjudicated DKT-Sem input component.
It encodes the authentic question, completed answer and KC description separately
with frozen `sentence-transformers/all-mpnet-base-v2`, revision
`e8c3b32edf5434bc2275fc9bab85f82640a19130`. Concatenate three 768-vectors,
apply trainable `Linear(2304,32)` and tanh, and append to the completed recurrent
input. The fixed five-input reference changes 65 to 97 recurrent coordinates,
adds 73,760 projection and 3,072 recurrent weights, and totals 119,874 parameters.
The current prediction uses the previous state and original inputs. Neither the
current answer nor its semantic vector enters its own prediction or retrieval cue.

The list requires `question_col`, `response_col`, `kc_col`, `kc_strip_number`,
`python`, `helper`, `helper_sha256`, `model_dir`, `model_id`, `revision`, and
`manifest_sha256`. Bind the Python executable and maintained packaged helper
`python/lkt_semantic_encode.py` explicitly. The helper is called with
`--model-dir DIR --manifest-sha256 SHA --request INPUT_JSON --output OUTPUT_JSON`.
Request is `{ "texts": [ ... ] }`; response contains N-by-768 embeddings plus
model ID, revision, manifest SHA, embedding dimension and exact package versions.
The helper validates its model/tokenizer manifest and runtime, loads only local
artifacts, and never downloads or substitutes another encoder. R checks the helper
and manifest SHA256 before execution. `jsonlite` and `digest` are optional required
packages for this mode. Paths may be explicitly rebound after relocation only to
the same hash-bound artifacts; no directory or library search occurs.

Bind the study fields to exact `Problem.Name`, `Input`, and `KC..Cluster.`.
With `kc_strip_number=TRUE`, remove only the leading numeric KC identifier and its
separator; preserve the remaining statement. Preserve answer strings literally,
including empty strings, spaces, misspellings and timeouts. Reject NA completed
text. Do not construct feedback, canonical answers, imagined tutor turns or
worked solutions. An unanswered candidate does not require or encode its response.
Pending completion may supply a new response string, but other trial identity
fields must still match. Candidate scoring never commits a semantic update.

Only text embeddings are cached, never student predictions. Saved preprocessing
contains exact UTF-8 texts, finite 768-vectors, a text hash and encoder metadata;
projection/recurrent weights and settings are saved normally. Process-local
runtimes extend their cache for unseen completed answers using the same pinned
local helper. A historical-answer-only lookup is insufficient. Runtime creation
validates the pinned encoder even when all requested strings are cached. Save
models/states, not runtime environments.

For shared preprocessing, optional `cache_file` and `cache_sha256` bind one RDS
snapshot. Internal `lkt_gru_semantic_prepare_cache(texts, semantic_input)` returns
that snapshot for a caller to save. An optional `encoder_result` argument imports
an already generated helper response with its exact ordered unique texts, avoiding
repeat encoding. It verifies dimensions and metadata against the local helper.
A saved fit embeds the cache, so replay does not require this external cache file.
A bad supplied file/hash fails; there is no cache-discovery workaround.

Ordinary GRU, existing feature encoders/retrieval and none/constant output adaptation
are supported. Variational inference, hidden decay, gate/connection fatigue,
student-adaptive embeddings, other output-plasticity modes, all-layer input,
warm starts and non-GRU units are rejected explicitly. No benchmark settings or
central release are promoted by this optional extension.

`tests/gru-semantic-input.R` accepts an explicit JSON settings list through
`LKT_SEMANTIC_TEST_CONFIG`. It checks real local encoding and unseen-answer replay,
causal independence of current/future answers, projection gradients, chunking,
saved models, pending trials, exact parameter counts and strict hash failures.
Without that explicit test configuration it reports the missing integration-test
binding rather than choosing an encoder or Python installation.
