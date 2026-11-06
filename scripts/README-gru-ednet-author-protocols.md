# Existing base GRU under two published EdNet evaluation procedures

This follow-up keeps the centrally released base model fixed. No neural source,
release, hyperparameter search or previously fitted model is changed. New fits
use the existing base features, coefficients, optimizer, validation-log-loss
checkpoint selection, 100-step chunks and 100-event retrieval. These are
data/evaluation comparisons, not reproductions of either paper's architecture,
hyperparameter optimization or history limits.

## KTST / Principled Transformers

Reuse the exact author-cleaned 5,000-student sample and assignments retained in
the earlier EdNet experiment (4,687 accepted students). The fixed test students
remain fixed. The five other student folds rotate as validation; this is not
five outer test folds. Read the five published `set_dense` / `q_mean_c` EdNet
configurations at commit `e6331a6f45e597f286d1272fbb96e7bb67bcaa52`: validation
folds 0--4 use seeds 90, 56, 96, 50 and 95 respectively. Fit the unchanged base
fresh for all five pairs. Aggregate individual model test AUCs with arithmetic
mean and population SD, matching the released table notebook. Also record sample
SD and a conditional whole-test-student bootstrap interval. Do not pool repeated
test responses as independent observations or ensemble model probabilities.

## UNVaMP procedural comparison

The selected best-matching preparation uses:

```powershell
python -B scripts/prepare-ednet-unvamp.py ARCHIVES OUTPUT --sampling-frame qualified-students --threshold-scope sample --cap-order after-filtering
```

Select 20,000 students with at least ten distinct validly answered questions in
their full raw histories. Remove missing responses and session/question duplicates,
join tagged item metadata, then iterate the ten-distinct-item student and
100-distinct-student item filters on the sampled full histories. Only after that
fixed point, retain the earliest 500 remaining responses per student. Do not
reapply either threshold to truncated histories. Item eligibility before the cap
is saved and hashed in `item-eligibility-before-cap.csv`. Preparation choices were
selected by comparing aggregate counts to the published dataset, not by predictive
scores. This is a count-matched procedural approximation, not recovery of the
unreleased author sample. Creating the dataset does not launch model fitting.

The preparation command supports two explicit sampling frames. The original
completed batch used the default `--sampling-frame all-students`. The follow-up
uses `--sampling-frame qualified-students`, interpreting the reported 20,000 as
students already having at least ten distinct validly answered questions.
Qualification is measured on the full raw history, before the 500-response cap,
metadata exclusion and iterative item/student filtering. The seed-2 random
ordering is unchanged: scan it until 20,000 qualifiers are accepted. This is a
uniform sample among qualifiers; rejected short histories do not consume sample
slots. `sampling-audit.csv` records the examined prefix, qualification counts and
acceptance decisions. This interpretation is user-selected, not a confirmed
description of the unpublished author code. Preserve the original study and
use a new study folder for each sampling frame.

The revised data-preparation interpretation additionally uses
`--sampling-frame qualified-students --threshold-scope kt1`. Both the ten-item
student threshold and the 100-distinct-student item threshold are iterated to a
fixed point on full KT1, using valid answers and tagged item metadata, before
selecting 20,000 eligible students. The chronological 500-response cap is then
applied to globally eligible interactions. Neither threshold is recomputed within
the sample or after truncation. `global-eligible-students.csv`,
`global-item-counts.csv` and the full-population filtering trace record the
population used to establish eligibility. This is a preparation/counting
revision only; it does not authorize or launch new fits. Historical sample-scope
preparations and fitted models remain unchanged. The steps below describe the
original `--threshold-scope sample` procedure.

The paper describes a fixed 60/20/20 split, not cross-validation. Its code,
sample seed and original assignments are unavailable. The new pipeline follows
the published cleaning as closely as possible, with explicit prespecified
choices for the omitted details:

- Sample 20,000 KT1 students uniformly using a seeded random ordering (seed 2).
- Remove missing responses. Use official `solving_id` as session identity and
  retain the earliest valid student/session/question response; preserve later
  sessions. Timestamp ties retain original file order.
- Keep the earliest 500 cleaned responses per student, then iteratively retain
  students with at least ten distinct items and items with at least 100 distinct
  students until neither set changes. Required item/skill metadata must exist.
- Split whole students with independent PCG64 seed 2: floor(60%) training,
  floor(20%) validation, remainder testing. Three fresh neural seeds 1--3 use
  this same split. This does not claim identical original sampled students.

For both procedures, skill combinations remain the existing base's categorical
skill input. Training rows alone determine vocabularies and scaling; held-out-only
identities receive the base's explicit zero encoding. Timing uses presentation
timestamps and a practice clock formed only from completed prior response
durations. Primary next-response targets exclude each student's first retained
response; all-response results are also retained, since UNVaMP's exact target-mask
implementation is unavailable. Sample/filter ordering, counts at every filtering
pass, archives, source hashes and participant assignments are retained in provenance.

## Execution and checks

`prepare-ednet-unvamp.py` reads the existing official archives directly; it does
not extract another full raw tree. `run-gru-ednet-author-protocol.R` freezes and
checks tasks/data/partition/release/script hashes, fits through maintained LKT,
and verifies portable held-out replay before accepting a model. It independently
matches restored training AUC to the trainer's pROC AUC. The double-count shared
metrics helper handles large EdNet class counts without integer overflow.

`test-ednet-unvamp.py` verifies session semantics, chronological cap, iterative
unique-pair thresholds and split determinism. `test-ednet-author-protocol.R`
verifies published fold/seed selection, whole-student roles and weighted
participant bootstrap against independent literal-row pROC calculations.

Big Blue uses the existing checked CPU container/dependencies and installs the
unchanged centrally selected release archive in each isolated study bundle.
Setup validates every partition before a bounded Slurm array starts. Both arrays
were initially limited to three concurrent fits; after a node-resource check,
KTST concurrency was raised to five. UNVaMP remains at three.
Report/check jobs require every saved model and successful replay receipt before
computing the 2,000-draw shared whole-test-student bootstrap. Completed earlier
EdNet results and the separate Cloze training grid remain untouched.

Published sources: [KTST code](https://github.com/kainbr/kt_set_transformers),
[UNVaMP methods](https://educationaldatamining.org/edm2026/proceedings/2026.EDM.full-papers.72/index.html),
[official KT1 schema](https://github.com/riiid/ednet).
