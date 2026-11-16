# Simplified connection fatigue plus constant output adaptation

Enable `gate_fatigue=TRUE, gate_fatigue_mode="connection"`, with an explicit
`gate_fatigue_time_col`. Combine with `plasticity="constant"` and the same
`plasticity_time_col`. The earlier unit-fatigue mode remains unchanged.

For every scalar input/recurrent GRU weight in each layer (including all three
gate blocks), learn a shared lambda in [0,1], initialized at 1. One positive
recovery rate rho is shared, initialized to log(2) per minute. Biases, encoders,
retrieval and prediction-head weights are not multiplied by A.

Each student starts with hidden state zero and connection multipliers A=1.
At each answered event, recover A_rec = 1-(1-A_old)*exp(-rho*dt), use
W_effective=A_rec*W_base in the ordinary GRU equations, then store
A_new=lambda*A_rec. Every connection fatigues once per answered event, regardless
of its activation or prediction error. There is no kappa or activity factor.
The clock is in minutes independently of the numeric feature schema.

Constant output adaptation is unchanged: v_new=v_old+eta*(y-p)*z, with v and
eta initialized at zero. Predictions precede their own response updates. Testing
updates student h, A and v; shared network weights, lambdas, rho and eta are fixed.
Padding and uncommitted/pending scoring never change stored student states.
Lambda=1 removes connection fatigue; eta=0 removes output adaptation.

The connection scan is compiled Torch with live parameters passed explicitly.
A is flattened in each layer as weight_ih then weight_hh. Its initial zero-width
representation is expanded to ones on the first transition, once actual module
shapes are known. It is packed/subset/detached through the existing state adapter.
The optimizer projects lambda to [0,1] after its ordinary update.

Requested study: 15 new combined fits, five unchanged participant folds and
seeds 1-3. Remove student logitdec and PPES from the six-numeric-input reference,
leaving four numeric inputs plus the original categorical inputs. Other training
controls and validation checkpoint selection are unchanged. No additional fits
are introduced for controls; earlier results have their feature sets identified.

## Lambda regularization

Connection lambda is excluded from optimizer weight decay. All other parameters,
including log_rho and output eta, retain their previous regularization and rates.
The matched no-decay rerun changes this single optimizer grouping; initialization,
student states, inputs, folds, seeds and stopping settings are preserved.

## Fixed connection clocks

`connection_time_scale_range=c(.1,10)` draws log-uniform positive recovery-clock
multipliers with `connection_time_scale_seed`, independent of training seed.
The generated vector is a persistent buffer, not a trainable parameter. The same
weight layout and fixed seed yield the same matrix set for all folds/students/seeds.
Only recovery uses scaled elapsed time; numeric predictors and per-practice lambda
fatigue remain unchanged. Default c(1,1) retains the unscaled historical format.
The experiment fixes the matrix seed at 20261108; it is not selected by held-out fit.

## Candidate-only scope

`connection_fatigue_scope="candidate"` limits lambda and student A states to
the candidate rows of every input/recurrent GRU matrix. Reset/update rows are
used directly, with no fatigue multiplier. Biases and output adaptation are unchanged.
Fixed clocks are drawn in the original full-matrix order and then subset to
candidate rows, so a candidate connection retains exactly its prior clock value.
The matched three-layer study has 7200 fatigued connections instead of 21600.

## Input-only scopes

`input_candidate` fatigues only input-to-candidate weights in all GRU layers.
`h1_input_candidate` limits this to the first layer. All recurrent, reset and
update weights remain unscaled. The study has 4128 and 2080 fatigued connections,
respectively. The exact physical indices select clocks from the original shared
matrix; unused lambda parameters and student A states are absent. Other settings
and constant output adaptation are unchanged.

## Recurrent-only scope

`recurrent_candidate` scales only recurrent-to-candidate connections in each GRU
layer. All input matrices and reset/update rows remain unscaled. The three-layer
study has 3072 fatigued connections; clocks are exact subsets of the saved shared
matrix. Constant-rate output adaptation and lambda decay exclusion are unchanged.
