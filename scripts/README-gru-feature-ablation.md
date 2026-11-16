# LKT/time feature benefit: Cloze versus ASSISTments 2017

Compare the existing full GRU with a model that removes both the engineered LKT
inputs and elapsed-time inputs. Item/skill embeddings, observed answer history,
response encoding, three 32-unit GRU layers, filtered retrieval and the 128-unit
head remain. Head, input and between-layer dropout stay at 0.10. Chronological
ordering remains; no elapsed-time, LKT or raw numeric values enter the ablated
model. Input-dependent parameter counts are reported rather than matched.

Use five whole-participant outer folds and seeds 1-3: 60 fresh fits. Cloze reuses
the exact prepared data and assignments from the completed head-dropout study.
ASSISTments uses the same partition algorithm/seed schedule on its own students.
Training-only vocabularies/scaling and validation-log-loss checkpoint restoration
are unchanged. Unknown categories use zero encoding on both datasets; all 15 fresh
full Cloze controls must reproduce the earlier predictions.

The feature recipe and its six nonlinear coefficients are inherited from the
Cloze baseline and held fixed on both datasets. This tests the current fixed
recipe, not separately optimized features for each dataset. Earlier inspection
of Cloze influenced the architecture; conclusions are exploratory.

## ASSISTments preparation

Raw 2017 competition file from the public archive indexed by EduData:
https://github.com/bigdata-ustc/EduData/blob/master/EduData/DataSet/download_data/download_data.py

Publisher and field dictionary:
https://sites.google.com/view/assistmentsdatamining/dataset

Keep original, non-scaffold, non-hint responses with zero prior hint count and no
first-response help request. Exclude student-item pairs whose earliest action
was assisted. Keep items with one observed skill among these cleaned records.
Iterate >=100 distinct students per item and >=10 distinct items per student.
This retains 157,124 responses, 1,643 students, 533 items and 76 skills. These
counts differ from UNVaMP's; this is an explicit procedural approximation.
The raw file uses `action_num` for ordering. `startTime` is pre-response UNIX
seconds; `endTime-startTime` contributes only to shifted prior-duration history.
Compute item age/prior spacing on the full original-problem exposure log before
scored-row filtering, including assisted encounters. Demographic, aggregate
performance and affect columns are not exported or used as predictors.

## Estimand and checks

For each dataset, feature benefit is pooled held-out AUC(full) minus
AUC(no LKT/time). Pool five outer folds within seed, then average seeds. The main
interaction is Cloze benefit minus ASSISTments benefit. Positive values support
the user's hypothesis of a larger benefit on Cloze. Also retain log loss, Brier,
training/validation AUC, epochs, timing and parameter counts.

Use 2,000 whole-participant bootstrap draws paired across conditions/seeds within
dataset and independent between datasets. Intervals condition on fitted models.
Validate weighted AUC against pROC and twenty literal actual-data bootstrap draws.
Smoke tests must establish finite weights/predictions, saved-weight replay and
exact invariance of ablated predictions when timing values change but order stays.

Scripts: `prepare-gru-assistments.R`, `compare-gru-feature-ablation.R`,
`report-gru-feature-ablation.R`, `test-gru-feature-ablation.R`, and the three
feature-ablation Slurm drivers under `scripts/hpc/`. Fitting and replay delegate
to the existing maintained study helpers and centrally selected LKT archive.

Artifacts: `C:/dev/LKT-outputs/gru-feature-ablation-20261106`.
Raw inputs: `C:/dev/LKT-data/assistments2017`.
The final frozen plan is in `frozen-plan/`; earlier preflight records are retained
in `local-plan/`. No package defaults or central release are changed.
