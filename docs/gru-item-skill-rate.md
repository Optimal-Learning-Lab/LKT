# Joint item and skill output learning rates

`plasticity="item_skill"` uses the first two categorical columns as item and skill.
Each item must map to exactly one skill in training data. Two tables are learned:
nonnegative skill rates r[k] (zero start) and raw item log multipliers u[j] (zero
start). Effective item multipliers are a[j]=exp(u[j]-mean(u in skill k(j))).
Thus each skill's item multipliers have geometric mean one and begin at one.
The effective response update is v_new=v_old+r[k]*a[j]*(y-p)*z. Prediction precedes
feedback. Rates and multipliers remain fixed during testing; only student v and
normal GRU/retrieval state evolve. There is no drifting rate or connection fatigue.

The nesting means arbitrary item and skill rate tables would trade off. Centering
item log multipliers within skill makes skill rates their geometric-mean baseline.
The two tables store I+K trainable entries but have I effective degrees of freedom
when every skill has items; item-log common shifts are removed by normalization.
AdamW weight decay applies as in the controls. The reserved unknown item has a
neutral multiplier of one; a reserved unknown skill has rate zero. Known items
with a different skill than the saved training mapping are rejected. The normal
unknown_levels setting still governs whether unseen category IDs are allowed.
Mapping and table vocabularies are learned only from fitting rows and saved in
preprocessing, with immutable parent/count buffers checked on checkpoint loading.

The Cloze comparison has 144 items nested four per each of 36 skills. It matches
the original six-input constant-output GRU (PPES and student logitdec included;
no experimental hidden/connection fatigue). Only the output-rate mode changes.
One joint set of 15 fits uses the existing participant folds and seeds 1:3 and the
same optimizer, training budget and minimum-validation-log-loss checkpoints.
