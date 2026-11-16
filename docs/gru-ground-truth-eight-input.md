# Eight-input Cloze GRU ground truth

This is a separate research fit, not a simulator/default change. It uses the
current verified LKT release and calls its existing numeric-input, recurrent,
retrieval, student-local embedding and constant output-plasticity mechanisms.
No network implementation is copied into the research scripts.

`scripts/gru-ground-truth-eight-input.json` defines the experiment. The original
six inputs remain: item age, mean prior spacing, spacing availability, student
logitdec, student-item logitdec and student-item PPES. Two raw numeric columns
are added: `random_intercept` (student LKT intercept) and `kc_intercept` (item
LKT intercept/easiness). A positive item intercept means easier; its negative
is reported as difficulty. Training-row standardization is saved with the model.
These values pass through the neural encoder, prediction and memory-update
paths. They are not added as fixed output offsets and are not optimized as local
state. The original 3 x 32 GRU, 128-unit prediction layer, retrieval, clipping,
optimizers, seed 1 and validation stopping are inherited unchanged. The numeric
encoder's input width grows by two, so this requires a new fit.

Student and item intercepts come directly from one complete Cloze LKT accuracy
fit: global intercept, student/item intercepts, student-item PPES and PPEF.
All eight nonlinear source parameters and coefficients are estimated on all
55,122 responses from 478 students and 144 items. No first-20-event ability
estimation, prefix removal or omitted-reference imputation is introduced.
Treatment-coded zero reference effects use the shared verified coefficient
extractor. Missing or ambiguous traits fail. The six GRU history-transform
parameters come from the already completed Cloze training-only logistic
control calibration. No Podsie parameters are imported and no Wald refit is
needed for this accuracy experiment.

Every student starts with zero recurrent memory, empty retrieval, zero output
adjustments and fresh item/skill vectors in both training and evaluation. Shared
weights and the constant output learning rate learn across training students;
online replay freezes the shared weights. The supplied baseline traits are
available from the first trial and remain fixed while local state learns.

All 55,122 rows and original whole-student GRU folds are retained (305 training,
77 validation, 96 test students). Validation loss selects the epoch, with the
original 100-epoch maximum and patience 10. Reported AUC/log loss are conditional
on full-Cloze LKT calibration, including traits estimated from validation/test
responses. The student folds exclude those students from GRU weight training;
they do not make this an independent test of the complete trait-estimation
pipeline. Reports explicitly retain that distinction.

The command has six positional arguments:

```text
Rscript --vanilla scripts/gru-ground-truth-eight-input.R ROOT SHARED_PROJECT REFERENCE CONTROL_RECORD OUTPUT ACTION
```

`REFERENCE` is the immutable local-item/skill Cloze study directory.
`CONTROL_RECORD` is the immutable completed Cloze logistic-control record.
`SHARED_PROJECT` supplies the maintained coefficient extractor; its files are
read only. Select the current release through `LKT_RELEASE_FILE` or the normal
central loader. `OUTPUT` must be a named experiment under `LKT-outputs`.

Actions are `plan` (read-only), `prepare` (full LKT trait calibration and exact
trait joins), `smoke` (bounded one-epoch architecture/runtime check), `fit`
(one full GRU fit and report) and `report` (saved-model checks and reporting).
Continuations require identical protocol/source/input identities. Outputs are
immutable fitted records, trait tables, training history, predictions, AUC/log
loss tables, HTML/text reports and runtime/isolation receipts. Runtime checks
cover read-only candidate scoring, batching, renaming, chunked save/load,
fresh-student resets, local item/skill learning and unchanged shared weights.
A cold-start trait grid reports learned ability/easiness sensitivity without
assuming monotonicity.

The dedicated SLURM launcher stages code only, reuses declared artifacts already
on Big Blue, enforces the existing combined 10 GB cap and verifies all input and
protected comparison-source hashes again at completion. It neither changes nor
depends on completion of the running two-arm comparison. This fitted GRU is
intended for ground truth, but simulator role validation/integration is a future
explicit step; it is not silently packaged as a tracer.
