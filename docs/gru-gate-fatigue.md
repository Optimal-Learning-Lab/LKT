# GRU write-gate fatigue and recovery

Opt in with `gate_fatigue=TRUE, gate_fatigue_time_col="CF..Time."`.
The ordinary GRU write fraction is g=1-z, where z is its retention gate.
Each student carries unit strengths h and a multiplier a for each unit in each
layer. Initially h=0 and a=1. Shared learned scalars are fatigue strength kappa
(initially zero, projected nonnegative after optimizer steps) and log recovery
rate log_rho (initially log(log(2)), giving a one-minute recovery half-life).
Elapsed time dt is converted to minutes independently of numeric predictors.

For each completed response, in chronological order:

1. Recover: a_before = 1 - (1-a) exp(-exp(log_rho) dt).
2. Write: h_new = h + a_before g (candidate-h).
3. Fatigue: a_new = a_before exp(-kappa g).

The fatigue driver g is an explicit design choice: ordinary pathway activity,
not prediction error or the magnitude of the actual unit-strength change.
Thus even candidate=h causes fatigue. These equations scale the effective
incoming write connections together per unit; stored individual weight entries
are not independently fatigued. Predictions use pre-response unit strengths.
Recovery modifies the next write, not the existing unit strengths directly.

The compiled Torch recurrence is differentiable. All shared weights and scalars
are learned during training and fixed during testing. Testing still advances
student h and a after observed responses. Padding and unanswered scoring never
commit updates. No complete event history is stored for this mechanism.

Kappa=0 reproduces the ordinary GRU mathematically; reordered floating-point
operations can cause rounding differences. The disabled path is unchanged.
The new options compose with existing output plasticity. Combined mechanisms
must use the same timestamp column. The initial experiment uses constant-rate
output plasticity, not its separate error-change-driven fatigue variant.

Comparison: 15 gate-only plus 15 gate-and-constant-output fits, using completed
stock and constant-output controls. Same five participant folds, three seeds,
six inputs, inner-validation checkpoint selection and training settings.
No stock or existing output-adaptation models are retrained.
