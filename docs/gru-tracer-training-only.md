# Six-input Cloze tracer with training-only history calibration

This separate experiment corrects the upstream calibration scope of the recent
full-Cloze comparison. It retains the same whole-student cv1 split, seed 1,
three 32-unit GRU layers, six numeric inputs, supplied item/skill labels,
student-local embedding adaptation and learned constant output learning rate.
It adds no ability/easiness inputs and changes no package or simulator defaults.

The six numeric inputs are student logitdec, student-item PPES, student-item
logitdec, item age, mean prior spacing and spacing availability. The six nonlinear
parameters of the three history functions come from the existing training-only
LKT control calibration: 35,125 responses from 305 students. The saved calibration
and its protocol are pinned by hash. The runner verifies the declared training
rows and recomputes the training-only likelihood and optimizer objective from
saved predictions. It never reruns nonlinear optimization or fits Wald latency.

Validation uses 8,895 responses from 77 different students; test uses 11,102
responses from another 96 students. Validation selects the epoch under the
existing 100-epoch cap and patience 10. Test results do not select parameters or
epochs. This is one partition and one seed, rather than a completed five-fold study.

Before fitting, the actual dataset undergoes a held-out answer/timing perturbation
check. Training feature values, scaling and vocabularies must stay unchanged;
training features must also equal features computed with held-out students removed.
Native fixture checks reject mixed-student folds and full-data objective records.

The same per-student resets and causal learning apply during training and
evaluation. A one-epoch smoke check precedes the single full fit. Runtime checks
cover prediction before an answer, future-answer isolation, immutable candidate
scoring, batching/reordering, identity renaming, student resets, zero shared item
and skill tables, unchanged shared weights, chunking and saved-state reload.
An independent Python calculation checks all three AUC/log-loss values and
disjoint student assignments after fitting.

Maintained entry points are `scripts/gru-tracer-training-only.R` and
`scripts/hpc/run-gru-tracer-training-only.slurm`. All algorithms remain in the
centrally selected LKT release. The job uses one CPU, 16 GB RAM and a 12-hour
limit within the existing combined 10 GB storage cap. It reuses existing cluster
inputs in place and has no effect on the running eight-input or simulation jobs.
Experiment records and results belong under the sibling `LKT-outputs` directory.
