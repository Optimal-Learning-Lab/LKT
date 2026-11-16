# Separate six-input GRU tracer with student-local learning

Canonical family: **`tracer_six`**. See [the three-version GRU guide](../README-GRU.md)
for the baseline/ground-truth distinction and the current training-only rerun.

## Supplied skill labels

The explicit deployment setting `local_skill_labels=true` retains the original
item and skill input channels and the original item-or-skill retrieval matching.
Both tables are adaptive: every student starts with fresh zero item vectors and
fresh zero skill vectors. A supplied label identifies which vector to update; it
does not provide a pretrained vector. An answer can therefore change predictions
for another item sharing the marked skill, within that same student's history.
Neither vector table carries learned values into the next student. The same rules
apply during training, validation and testing. The original six-input architecture
and parameter dimensions are retained in this variant. The earlier item-only
experiment is preserved separately and is not relabeled as the skill-label version.

## Initial item-only experiment

This experiment starts from the reviewed six-input constant-output GRU, not the
Lasso experiment. No simulator configuration or model default is changed.

The three fixed history features are student logitdec, student-item PPES and
student-item logitdec, with the original nonlinear parameters. The other three
numeric coordinates are item age, mean prior spacing and spacing availability.
The original feature transformations, numeric encoder, three 32-unit GRU layers,
retrieval, 128-unit prediction head, optimizer and validation stopping are retained.

The previous categorical inputs were item and supplied skill embeddings. There
was no student identity embedding. The separate tracer removes the supplied skill
lookup and skill-based retrieval filter. Item labels address a student-local
dictionary and an item-equality retrieval filter, without encoding pretrained
knowledge of those labels. Student labels select histories, not a trained table.

For each student, in every training epoch and every evaluation replay:

1. Start with zero recurrent memory, empty retrieval, zero output adjustments
   and zero vectors for item identities.
2. Predict before observing the current answer.
3. Advance memory/retrieval and the existing constant-rate output adjustment
   using the observed trial's original inputs.
4. Apply one local item-vector SGD update from that pre-answer prediction loss,
   at rate 0.01 with the existing gradient clipping. Local updates are first order.
5. Retain local state across chunks for that student; discard it at a new student.

Shared encoders, recurrent weights, prediction weights and the nonnegative
constant output rate learn across training students. Evaluation freezes those
shared weights but continues the same local post-answer updates. Fitted item
tables remain zero and are not used to look up identity-specific knowledge.
Two completely new items with identical observable histories therefore have the
same initial prediction. This model cannot know an unfamiliar item's difficulty
before observing evidence about it.

`anonymous-gru-tracer-study.R ROOT OUTPUT REFERENCE ACTION` supports plan, smoke,
fit and report. REFERENCE explicitly names the immutable six-input constant-rate
study, including prepared rows and the original partition. The initial test is
cv1/seed1 only, with validation selecting the epoch under the original 100-epoch
cap and patience 10. It is not complete five-fold/three-seed cross-validation.

The runner requires Big Blue Slurm, the official release loader, declared runtime
hashes and the combined home/project 10 GB cap. Native verification precedes the
small pipeline smoke and scientific fit. Fits/predictions are separate artifacts;
original protocols and fitted models are preserved. The independent Python
auditor checks all three phase AUC/log-loss results and student separation. Runtime
checks cover saved-prediction replay, fresh student/item relabeling, chunking and
equal initial predictions for unfamiliar items.
