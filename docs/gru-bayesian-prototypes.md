# Bayesian learner-rate prototypes for output adaptation

Enable `GRUModel(plasticity='bayesian_prototypes', plasticity_time_col='clock', ...)`.
The existing explicit chronological timestamp contract for output plasticity remains;
the prototype rates themselves do not depend on elapsed time. The option is also
available through the existing GRU configuration/GUI plasticity field. Disabled
settings construct and execute the existing baseline path without extra parameters
or random draws. Test this condition independently of latent denoising.

This implements the mechanism transfer specified in
`C:/dev/LKT-outputs/pfa-mechanism-review-20261109/reviews/pfa-e0d0f83af140.json`,
whose original review score remains 7. The paper is *Supercharging BKT with
Multidimensional Generalizable IRT and Skill Discovery*, JEDM 16(1), 2024,
pp. 233-278, https://jedm.educationaldatamining.org/index.php/JEDM/article/view/642.
Section 3.2 (pp. 244-245, Eqs. 18-19) supplies predictive marginalization and
sequential Bayes updating with a uniform new-learner prior; Section 3.2.1 supplies
learned prototypes; Section 4.4 (p. 261) uses 20 prototypes. The GRU transfer uses
20 scalar learning-rate coordinates, rather than the paper's multidimensional BKT
parameters or skill discovery. Rate-only coordinates and their initialization span
are explicit choices from the frozen transfer contract, not reported GRU results.

For one learner, let `a_t` and `z_t` be the unchanged shared head's base logit and
final hidden activations. Maintain 20 correction vectors `v_kt` and normalized
log posterior weights `l_kt`. For a new learner, `v_k0=0` and `l_k0=-log(20)`.
The shared trainable parameters are a projected nonnegative scalar `eta`, initialized
at zero, and 20 unconstrained `d_k`, initialized evenly from `log(.25)` to `log(4)`.
There are 21 added learned scalars, with `eta_k=eta*exp(d_k)`.

For each trial, before observing the answer:

1. Compute the global GRU, retrieval and shared head once, then component logits
   `u_kt=a_t+dot(v_kt,z_t)` and component probabilities `p_kt=sigmoid(u_kt)`.
2. Predict `p_t=sum_k(exp(l_kt)*p_kt)`. The ordinary response objective is evaluated
   on this mixture using its log odds and the existing binary loss configuration.
3. For an observed binary answer `y_t`, update `l'_kt=l_kt+y_t*log(p_kt)+
   (1-y_t)*log(1-p_kt)`, then subtract `logsumexp_k(l'_kt)`.
4. Update every component from its own pre-feedback error:
   `v_k,t+1=v_kt+eta*exp(d_k)*(y_t-p_kt)*z_t`.

The component correction update does not use posterior-weighted errors. The
posterior is updated before the correction vectors; its likelihood is calculated
from the pre-update component probabilities. Components retain fixed learned rate
coordinates throughout prediction, while the posterior belief and learner-local
vectors evolve. No online global weight updates occur.

Both mixture marginalization and posterior updates use stable log arithmetic.
For stability and exact identical-component logits, the mixture log odds are computed
as `u_1 + logsumexp(l+log(p)-log(p_1)) -
logsumexp(l+log(1-p)-log(1-p_1))`, with the difference of the two log sums formed
before adding `u_1`. Stable `logaddexp` expressions calculate Bernoulli log
probabilities without probability clipping. This is an algebraic identity for the
full mixture. There is no component pruning, posterior reset, approximate likelihood,
rate clamping, or equality-triggered branch. Nonfinite dynamics cause an explicit
error instead of silently changing the model. Final exported probabilities retain
the package's ordinary output clipping; training uses the unclipped mixture loss.

Training differentiates through the posterior recursion and all correction updates
within each configured sequence chunk. The package's existing truncated
backpropagation detaches the carried context between chunks. Shared weights,
`eta`, and all 20 `d_k` are optimized jointly. At `eta=0`, all corrections remain
zero and predictions recover the shared head exactly; derivatives with respect to
`d_k` are zero there, while `eta` can learn away from zero. With every `d_k=0`,
all components are identical and both predictions and shared/eta gradients match
constant-rate output adaptation. The additional `d_k` gradients are permitted to
break the identical-component symmetry once the rates are learned.

Runtime context stores `prototype_weights` as `[batch,20,head_width]` and
`prototype_log_weights` as `[batch,1,20]`. Both use the package's existing context
subsetting, detachment, ordinary-array state serialization and `saveRDS`
continuation. Each configured history key owns its state. Trailing padding leaves
both unchanged. Unknown outcomes leave both unchanged; candidate scoring branches
from supplied state, and an unanswered `Step` commits neither adaptive nor recurrent
state. Feedback for a pending trial must follow the package's existing identity
checks before history advances. Saved model arrays include `plasticity.eta` and
`plasticity.log_rate_multipliers`, whose shape/finiteness are validated on load.

`tests/gru-bayesian-prototypes.R` covers independent equations and gradients,
finite differences, constant-rate prediction/gradient parity, zero-rate exact
logits, initialization/dropout RNG, isolated learners, padding/chunks, extreme
log posteriors, unknown feedback, fitting/replay, scoring and saved continuation.
Run model tests only in the authorized Big Blue environment. These tests establish
implementation behavior; they do not establish a Cloze predictive gain or execute
the review's proposed fixed 15-fit experiment. No combined mechanism condition,
research fit launch, or release promotion is implied by this module.
