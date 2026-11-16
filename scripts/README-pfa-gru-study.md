# PFA mechanism study execution contract

The maintained runner prepares one shared control or one independent mechanism,
with exactly five frozen whole-participant folds and seeds 1, 2 and 3 (15 fits).
The prior selected architecture, training settings, partition hashes, checkpoint
selection and replay tolerance remain fixed. No mechanism stacking or tuning grid
is introduced by runtime configuration.

## New Big Blue protocols

New plans use schema 2 and a new, nonexistent output directory. Do not rewrite or
resume a schema-1 local protocol with the changed runners. Old protocols, results
and weights remain historical evidence; their original five hash-bound scripts
and runtime must be preserved before maintained runner updates. A Windows run is
not a qualified Big Blue control or candidate completion.

`spec.runtime_provenance_file` selects the runtime explicitly. Its actual SHA-256
must equal readiness provenance, the RDS protocol and JSON protocol. The launcher
uses this frozen path; it does not assume a study-wide runtime filename. The
runtime must contain all existing candidate/R/platform/locale/package/library/
executable provenance, plus:

```json
{
  "schema": 2,
  "execution_host": "Big Blue",
  "scheduler": "slurm",
  "runtime_policy": {
    "maximum_concurrent_research_fits": 3,
    "allocated_cpus": 3,
    "torch_threads_per_fit": 1,
    "device": "cpu",
    "dtype": "float32",
    "deterministic": true,
    "environment": {
      "LC_ALL": "C.UTF-8",
      "LANG": "C.UTF-8",
      "LANGUAGE": "C",
      "R_TESTS": "",
      "OMP_NUM_THREADS": "1",
      "MKL_NUM_THREADS": "1",
      "OPENBLAS_NUM_THREADS": "1"
    }
  }
}
```

The example locale must be verified in the actual Linux container before freezing
provenance. No locale is selected automatically. All seven environment keys are
required, with the three thread limits fixed at `1` and `R_TESTS` empty. The
launcher applies them before starting R; direct plan/report commands must receive
the same environment. R checks their exact values and its actual `Sys.getlocale()`,
platform, versions and library paths against the runtime record.

Worker count is an explicit integer from 1 through 15, bounded by the declared
integer `allocated_cpus`; all scientific per-fit settings remain fixed. Every R
action and the launcher require `SLURM_CPUS_PER_TASK` at least as large as the
declared allocation. A shell on a login node does not satisfy the execution gate.
Readiness must be generated from checks executed inside the intended Slurm/container
runtime. This contract does not itself establish host identity, dependency
availability, package compatibility or historical-control training equivalence.

## Explicit score-seven user selection

The general mechanism threshold remains scores 8 through 10. A score-seven
mechanism additionally requires `spec.user_selection_file` and
`spec.user_selection_sha256`, pointing to the exact approved selection manifest.
The file must have `selected_papers`, with exactly one matching `paper_id` entry
whose `original_score` is 7, `mechanism_id` equals `spec.mechanism_family`, and
`review_sha256` equals the actual bytes of `spec.original_review_file`. A missing,
changed, ambiguous or mismatched selection fails. No additional score-seven paper
is admitted by changing the general threshold.

The verified manifest path, hash and matching entry are frozen as
`protocol.user_selection` in both protocol representations and rechecked on every
fit/report invocation. The specification itself remains hash-bound. Explicit user
selection authorizes queue membership; implementation, parameter count, readiness
and compatible shared controls remain separately required before planning fits.

## Invocation and preservation

Invoke `pfa-gru-study.R` with the existing eight arguments and the launcher with
its existing explicit arguments. The launcher still starts `Rscript --vanilla`,
checks the exact Rscript executable hash, verifies all six current runner hashes and
preserves each attempt and launch. Completed batches cannot be relaunched. Runtime
configuration does not create protocols, qualify controls, install packages or
submit Slurm jobs.

Scientific runtime checks, package/torch checks, replay and all research fitting
belong on Big Blue. Local verification of these orchestration edits is restricted
to source inspection and syntax parsing.


## EAKT preparation preflight

For `paper_id='pfa-90bae1222bc1'` or any condition enabling `exercise_attributes`,
SPEC additionally requires these explicit paired preparation bindings:

```json
{
  "contract_file": "/explicit/study/execution/eakt-preparation/implementation-contract.json",
  "contract_sha256": "<actual lowercase SHA256>",
  "annotation_plan_file": "/explicit/study/execution/eakt-preparation/annotation-plan.json",
  "annotation_plan_sha256": "<actual lowercase SHA256>"
}
```

These are required paths and hashes, not runnable defaults. The helper
`verify-pfa-exercise-attributes.R` is the sixth frozen script in every new protocol.
The existing original-review and user-selection bindings remain required.

Before writing a new plan or entering fit/report execution, the helper verifies
both preparation files and their immutable review identity; exact snapshot/access
and completed annotation readiness;144 item IDs/36 KC IDs without text deduplication;
72 question strings/18 KC statements;129/15 items with one/two complete recorded
answer strings; fixed source and approved prompt hashes;73 planned preparation
requests; and the frozen B and41218+8B count. It checks actual source bytes and rows,
not only declared counts. A synthetic marker, toy catalog, changed file, missing
artifact hash or unverified annotation plan fails explicitly. The verification
record is frozen in both protocol representations and rechecked for each action.
The current handoff remains blocked while real annotations and exact-model access
are unavailable; implementation tests do not establish this preflight as passed.

The model package remains reusable: its generic attribute loader validates declared
counts/hash bindings and provenance/catalog consistency for other legitimate
studies without knowing Cloze-specific identities. Only this research helper owns
the constraints above. No historical protocol or result is rewritten by adding the
new helper. Run the standalone preflight regression on Big Blue as
`Rscript --vanilla scripts/test-pfa-exercise-attributes.R ROOT` in the selected
candidate environment; it performs no fits.
