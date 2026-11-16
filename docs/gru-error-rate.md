# Student-specific evolving output learning rate

Select `plasticity="error_rate"` instead of `constant`. For student s and output
connection i, process each answered event t in this order:

    p_t = sigmoid(base_logit_t + sum_i(v_s,i,t * z_i,t))
    v_s,i,t+1 = v_s,i,t + eta_s,t * (y_t-p_t) * z_i,t
    eta_s,t+1 = eta_s,t + alpha*abs(y_t-p_t) - c

Here y is the observed 0/1 response, p the prediction before feedback, z the final
output layer's input activations, v the student-specific output-weight adjustments,
and eta_s,t that student's learning rate. Each new student starts with v=0 and the
existing globally learned baseline `plasticity.eta` (optimizer start zero). The two
new shared parameters alpha and c start at zero, and are learned during
training. Project these three shared parameters nonnegative after optimizer steps,
just as the constant model projects its baseline eta. Do not clip or project the
student rate: it can be positive or negative. No exponential, recovery term,
threshold switch, multiplicative update, or extra online learning rule is added.

The current output-weight correction uses eta_s,t. The error then changes the rate
for the next answered event. At alpha=c=0 the complete dynamics reproduce constant
output adaptation at the same baseline eta. At baseline eta=alpha=c=0 the output
adjustments are exactly zero. Error magnitude occurs once in each of two distinct
updates: the original output correction and the new rate-state update.

During testing, shared baseline eta, alpha and c and all other fitted parameters
are fixed. Student-specific v and eta continue evolving from that student's
observed responses. A held-out student starts at the learned baseline eta; it does
not inherit another student's state. Histories reset for separate train/validation/
test replay as in the matched constant model. Prediction precedes its own answer.
Unanswered scoring and padding never commit an update. Rates persist across chunks
and saved runtime state. For compatibility with existing context layout, each
student's scalar eta is repeated across the output connection-rate vector; every
entry is initialized equally and receives the same rate update, irrespective of z.

The matched study changes only this output-rate mode. Baseline eta and optimizer
are retained; alpha/c inherit the existing weight decay and add two fitted scalars.
Recurrent-to-candidate fatigue in H1-H3, its 3072 fixed clocks, four numeric inputs,
categorical inputs, dropout, five folds, seeds 1:3, early stopping, checkpoint
selection and lambda's weight-decay exemption are unchanged. Reports give baseline
eta, alpha, c, crossover error c/alpha (when alpha>0), signed-rate use counts,
fatigue parameters and train/validation/test AUC and log loss.

The neutral-initialization rerun changes only alpha/c starts from .1/.25 to 0/0.
The prior experiment remains frozen in its own source bundle and output directory.
