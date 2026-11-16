# Exercise attribute representation (EAKT without GAT)

`GRUModel(exercise_attributes=bindings, ...)` replaces only the first, 16-wide,
zero-initialized item embedding. The separate existing KC16 input, numeric inputs,
three-layer GRU, retrieval, head, and none/constant output adaptation retain their
existing implementations. The mode rejects Bayesian prototypes, latent denoising,
semantic response input, adaptive embeddings, other experimental decay/fatigue,
Mamba, and warm starts rather than silently stacking untested conditions.

The implementation follows the frozen transfer in
`C:/dev/LKT-outputs/pfa-mechanism-review-20261109/reviews/pfa-90bae1222bc1.json`
and `execution/eakt-preparation/implementation-contract.json`. The original score
remains 7. The paper is Guo et al., *Mitigating Cold-Start Problems in Knowledge
Tracing with Large Language Models: An Attribute-aware Approach*, CIKM 2024,
pp.727-736, https://doi.org/10.1145/3627673.3679664. The selected mechanism derives
from Algorithm1, five-rank standardization, and the EAKT w/o GAT ablation. The fixed
GPT-4o snapshot, statistics-domain prompts, eight-wide tables, mean pooling and
zero projection are disclosed transfer choices. This module neither calls an LLM
nor creates a real annotation artifact. The actual study condition remains blocked
until the frozen annotation pipeline and content validation are complete.

For known item i, the module looks up an eight-vector for its difficulty rank, an
eight-vector for its expected-time rank, and eight-vectors for its nonempty set of
required abilities. It averages the ability vectors, concatenates difficulty,
ability and time to24 coordinates, and applies one affine24-to-16 projection. There
is no activation, graph adjustment, extra KC input, extra objective, empirical
latency covariate, or residual free item lookup. Attribute tables initialize with
Xavier uniform values; projection weights and bias initialize exactly zero.
Initially every real item's vector is exactly zero. The projection receives a
first-step gradient while table gradients are initially zero; the tables learn
after projection updates. No artificial gradient path changes these equations.

The baseline's reserved zero padding row is retained as one1x16 padding-only
parameter. The144 ordinary item lookup rows are removed. Padded events use the
retained row through `nnf_embedding(..., padding_idx=0)`, which suppresses its
gradient. In Torch0.17.0, R row index1 is converted to native index0, while the
functional padding argument is passed through unchanged. Valid items always use
the attribute projection. It is not an unused
parameter added to obtain a desired count. Parameter accounting is therefore:

- Remove144x16=2304 actual item lookup scalars; preserve the existing16 padding
  scalars and separate KC table including its own existing padding row.
- Add5x8 difficulty +5x8 time +Bx8 abilities +24x16 projection weights +16 biases,
  or480+8B new scalars.
- The frozen43042-scalar baseline becomes41218+8B under the package's existing
  stored-parameter counting convention. As before, this convention includes the
  padding rows even though padding-index gradients are suppressed.

The original full-size item table is initialized in its original position, then
only its zero padding row is retained. This consumes the original baseline random
draws before shared modules initialize, preserving every common initial weight.
Attribute modules initialize after all shared modules. Their own initialization
consumes additional random draws; comparisons in training must explicitly align
forward RNG states. Disabled mode allocates no attribute modules or extra draws.
No dummy parameters or equality-dependent prediction branch is used.

The `exercise_attributes` binding is a named list containing every following field;
there are no implicit paths, metadata defaults or online annotation:

| Binding | Meaning |
| --- | --- |
| `artifact_file`, `artifact_sha256` | Explicit validated-item JSON and exact lowercase SHA256 of its bytes |
| `provenance_file`, `provenance_sha256` | Explicit provenance sidecar JSON and exact lowercase SHA256 |
| `item_col`, `kc_col` | First and second categorical columns, both16-wide |
| `question_col`, `answers_col` | Authentic source question and recorded accepted-answer columns, required for preprocessing validation |
| `expected_item_count`, `expected_kc_count` | Positive fixed catalog counts |
| `source_data_sha256` | Frozen source identity |
| `vocabulary_prompt_sha256`, `solutions_prompt_sha256`, `attributes_prompt_sha256` | Exact frozen prompt byte hashes |
| `purpose` | `study`, or explicitly `synthetic_test` for regression fixtures |

The existing GUI accepts these as ordinary `name|value` lines; the two count fields
are parsed numerically. The library validates the explicitly declared item/KC
counts, source hash, prompt hashes, coverage and provenance consistently for every
catalog. A legitimate new dataset uses `purpose='study'` with its own declared
bindings. Identical texts never merge item or KC identities. `synthetic_test` marks
regression fixtures and must agree with the sidecar; changing only a declared
purpose cannot reclassify the bound provenance.

The specific PFA/Cloze experiment additionally uses
`scripts/verify-pfa-exercise-attributes.R`, invoked by `pfa-gru-study.R` before plan,
fit or report. That study helper owns the144/36 catalog sizes,72/18 distinct text
counts,129/15 complete accepted-answer string multiplicities, numeric-prefix-only
KC statements, frozen prepared-data hash and three approved prompt hashes. It
hash-binds the concrete preparation `annotation-plan.json` and
`implementation-contract.json` from explicit SPEC fields, requires completed
annotation/access readiness, verifies actual source rows and every accepted-answer
string, and freezes B and41218+8B. Synthetic/toy artifacts cannot pass this study
gate. These experiment constraints are deliberately absent from generic model code.

The primary artifact retains the handoff's exact
`validated-item-artifact.schema.json` fields: `model`, `vocabulary`, `items`,
`source_data_sha256`, `content_validation_complete`, and `frozen_utc`. Only
`gpt-4o-2024-08-06` is accepted. Vocabulary rows contain unique `ability_id`, `name`,
and `definition`. Each item contains `item_id`, `kc_id`, finite difficulty0..1,
finite expected_seconds0..100, nonempty unique `ability_ids`, and integer
`difficulty_rank`/`time_rank` in1..5. Item rows use stable ascending ID order.
Ranks are recomputed globally as `1+floor(5*(r-1)/N)` after ordering the continuous
estimate and then item ID. Missing rows, duplicates, unknown abilities, bad ranks,
wrong hashes and unvalidated artifacts cause explicit errors before network use.

The separate provenance sidecar is required because the short primary schema has
no fields for the provenance that the review also requires. It contains exactly:

- `schema:1`, `purpose`, `model`, `temperature:0`, `seed_supported` and `seed`
  (`1` if supported, JSON null otherwise), `source_data_sha256`, `artifact_sha256`.
- `prompts`: `vocabulary`, `solutions`, `attributes`, each with exact `text` and
  `sha256`; text bytes and external binding hashes must agree.
- `catalog`: stable item-ID order, one record per item, with `item_id`, `question`,
  `kc_id`, `kc_statement`, and a nonempty array `accepted_answers`. Preserve every
  distinct recorded answer string, including every pipe-delimited alternative.
  Generic source preprocessing checks every supplied answer string belongs to the
  complete catalog; the PFA study preflight also verifies exact equality of full
  source answer-string sets.
- `reviews`: same order, with `item_id`, validated `proposed_answer`, `explanation`,
  `accepted_answer_match` selecting an actual accepted alternative,
  `content_validation_complete:true`, and `corrections`. Each correction contains
  nonempty `before`, `after`, and `content_reason`. Content validation is an explicit
  human/content-review assertion; this validator does not certify semantic truth.
- `raw_outputs`: one vocabulary result plus one solution and one attribute result
  per stored KC. Records contain exactly `stage`, `group_id` (`ALL` for vocabulary),
  exact `model`, `response_id`, `input`, `input_sha256`, `output`, `output_sha256`.
  Input/output are retained JSON strings whose exact UTF8 bytes must hash correctly.
  Parsed output uses the handoff response schemas (`abilities`, `solutions`, or
  `attributes` respectively); changed final content needs a correction record.

Every raw `input` has `items`, each containing only item ID, question, KC ID and KC
statement matching the catalog. Attribute inputs additionally contain the frozen
`vocabulary` and validated `solutions` (item ID, proposed answer, explanation).
Answer keys are retained for content validation and never enter generated-solution
or attribute request inputs. Extra participant/outcome/timing/fold fields are
rejected. Exact stage/group coverage, input identity and all content hashes are
validated; no request is made by this package.

The complete bound raw artifact and sidecar, vocabulary, item-to-KC mapping, ranks,
and ability mask persist inside preprocessing. Every new runtime revalidates them
and verifies metadata buffers against saved weights. Saved inference does not need
the original file paths to exist. Category IDs for the item input come from the
immutable full catalog, enabling known annotated questions absent from fitting
participants. Prediction requires the current item ID and matching stored KC; an
unknown item, missing metadata, changed KC mapping, or incomplete saved artifact
fails explicitly even if ordinary categorical unknown handling is set to zero.
Scoring and unanswered continuation retain the package's causal feedback rules.

`tests/gru-exercise-attributes.R` and its explicitly synthetic fixture helper cover
strict validation, preserved duplicate-text identities and answer alternatives,
mean pooling, exact zero initialization, projection-then-table gradients, padding
output/gradient, common-weight and forward RNG parity, disabled behavior,
known-catalog new questions, full frozen parameter counts, fitting, scoring and
saved continuation after source files are removed.
`scripts/test-pfa-exercise-attributes.R ROOT` verifies study-specific rejection of
synthetic substitution and absent/changed preparation bindings. All model execution belongs on
Big Blue. These checks do not perform the independent15-fit experiment or establish
a predictive improvement.
