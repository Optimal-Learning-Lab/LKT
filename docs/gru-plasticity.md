# Compiled student-specific output connections

`GRUModel(plasticity='constant' or 'fatigue', plasticity_time_col='CF..Time.')`
adds adaptive connections to the final scalar output layer. The default `none`
preserves stock. The GRU recurrent matrices, encoders, retrieval, PPES features,
shared output bias and shared head weights retain their existing roles.
This experiment does not combine plasticity with hidden-state decay.

Each student has vectors v (output-weight adjustments) and E (signed learning
rates). A shared baseline rate eta starts at zero. Fatigue mode also learns
kappa, initially 1, and log_rho, initially log(log(2)). Rho=exp(log_rho) is a
recovery rate per minute, initially a one-minute recovery half-life. Eta and
kappa are directly constrained nonnegative by projection after the existing
optimizer step; E is allowed to become negative. Constant mode learns only eta.

For final-head activation vector z, shared stock logit b, observed answer y,
and elapsed minutes dt, the chronological operations are:

1. Recover Eminus=eta+(E-eta)exp(-rho dt), or Eminus=eta in constant mode.
2. Predict logit=b+sum(v*z), p=sigmoid(logit), before using y.
3. After feedback, delta=Eminus*(y-p)*z and v=v+delta.
4. Fatigue E=Eminus*(1-kappa*abs(delta)); constant mode retains E=eta.

V starts at zero and E at eta for each new student/epoch replay. No history of
weight changes is stored. There is no weight decay, consolidation, new STP
component or per-response optimizer. Time is converted using the declared
time_units, independently of the numeric predictor schema. The adaptation from
the original PDP code is recovery across the observed gap before the response
update and a logistic output delta rule. Literal signed linear fatigue is
retained. Nonfinite dynamics fail explicitly; no clipping or recovery workaround
is applied to connection states.

Shared parameters learn through the existing loss, optimizer and truncated
backpropagation. Student v/E states are detached at the same chunk boundaries
as other memory. Global weights are fixed during testing; v/E change only after
observed answers. Unanswered candidate scoring branches without committing any
state or clock. Existing student histories serialize v/E with inference context.
History grouping must be exactly the student identifier, avoiding separate rates
for different items belonging to the same student.

The head's existing operations and dropout draws are unchanged; the final layer
input is exposed once. The ordinary GRU remains fused. A TorchScript scan handles
only the small adaptive output vectors and preserves autograd. Compilation is
required, and executable code alone is cached: weights and student state remain
explicit arguments. Padding does not update either vector. Statistics count
negative rates actually used on nonzero connection activations separately from
negative post-fatigue rates.

At eta=0, v/E remain exactly zero and the logits match stock. Eta has a nonzero
one-sided learning derivative at zero. Kappa and rho cannot influence predictions
while eta stays zero, so their fitted values in that case are not identified by
the likelihood. Only rho has a time half-life: log(2)/rho minutes. Eta and kappa
do not have standalone temporal half-lives.

`tests/gru-plasticity.R` checks compiled/reference values and gradients, finite
differences, learning away from zero, stock equality with dropout, causal feedback,
padding, chunk continuity, fitting, saved-state replay and immutable candidates.
The experiment driver checks exact initial equality on real Cloze histories for
every fold and seed before fitting.

`scripts/compare-gru-plasticity.R` freezes 45 fresh fits: stock, constant plasticity
and fatigue/recovery, each across five unchanged whole-participant folds and
seeds 1-3. PPES, preprocessing, all shared initial weights, dropout, optimizer,
100-epoch cap, patience 10 and validation checkpoint selection are matched.
Parameter counts are 43073, 43074 and 43076. The primary mechanistic contrast is
fatigue versus constant plasticity; stock is the predictive baseline. Report
training/validation/test AUC and log loss, parameters, recovery half-life,
negative-rate use, epochs and elapsed time for every fold and seed. No test-set
tuning, central release promotion or edits to historical results are permitted.
