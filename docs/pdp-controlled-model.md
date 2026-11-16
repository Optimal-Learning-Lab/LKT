# Controlled connection model from the 2004 PDP paper

This internal research model tests the mechanisms described in Philip I. Pavlik's
2004 one-page *A PDP Model of Spacing Effects in Memory*. It does not modify the
GRU, fit Cloze outcomes, reproduce the 200-unit network, or claim the paper's
reported numerical fit. Canonical equations live in `R/lkt-pdp-controlled.R`.

## Quantities and event order

For one learner, S and L are input-by-output matrices of short-term and long-term
connection strengths. E is the matrix of available long-term learning rates.
Initially S=L=0 and every E entry equals eta0, the base LTP learning rate. A model
holds etaS, the STP learning rate; eta0; kappa, fatigue sensitivity; rho, recovery
rate per second; and lambda, a fixed matrix of STP decay rates per second.
The clock is explicit elapsed seconds. The recovery half-life tau gives
rho=log(2)/tau; tau=Inf disables recovery.

At a practice event, x is a unit-norm cue vector, y the target output vector, and
dt the elapsed time since committed state. Operations are elementwise except
the stated vector/matrix products:

1. Sminus = exp(-lambda dt) S.
2. Eminus = eta0 + (E-eta0) exp(-rho dt). L is unchanged.
3. Predict yhat = x (Sminus+L), before observing y.
4. Error e = y-yhat; delta-rule direction U = outer(x,e).
5. DeltaS = etaS U; DeltaL = Eminus U.
6. Commit S = Sminus+DeltaS; L = L+DeltaL.
7. Commit E = Eminus exp(-kappa abs(DeltaL)), and the event time.

Fatigue uses the actual durable update, not the proposed update or net activation
change. It therefore has no circular calculation. The multiplicative rule keeps
available rates in [0,eta0] without switches or clipping. Positive and negative
updates remain signed in S and L; their magnitudes consume capacity symmetrically.
Delayed scoring advances a temporary copy and commits no learning. A test with
feedback is explicitly another practice event. Separate learner states share
model constants but no connection matrices or fatigue. No past event list is
needed by the recurrence; experiment logs are for verification and reporting.

## Paper statements versus explicit reconstruction choices

The paper specifies separate STP/LTP connections, delta-rule updates, no decay of
LTP, learning-dependent fatigue, exponential recovery of per-link learning rates,
and heterogeneous exponential STP forgetting rates. It gives mean STP decay .038
per approximately ten-second step. We interpret that as lambda=.038/10 per second
for a representative controlled connection. A heterogeneous demonstration uses
fixed seeded exponential draws with that mean, one rate per connection.

The exact multiplicative fatigue equation is a reconstruction choice. The paper
does not specify its formula or signed depression handling. We use abs(DeltaL)
as the smallest symmetric extension; negative-update results test that extension,
not an undocumented claim about the original model. The linear readout, unit-norm
cues, instantaneous practice events and illustrative parameters are also explicit
controlled-model choices. Unit-norm cues and etaS+eta0<=1 keep the total delta
step stable for repeated single-pattern diagnostics. Outputs are association
strengths, not calibrated correctness probabilities. No parameters are fitted.

Reference values, fixed before the diagnostic runs: etaS=.3, eta0=.1, kappa=8,
recovery half-life 300 seconds. Four practices have onset gaps 20 or 300 seconds.
Tests compare at equal delays after the last practice. Only the representative
STP decay rate is taken numerically from the paper. Time subdivision must preserve
decay and recovery exactly; inserting additional *practice* events changes learning.

## Verification design

The independent first-event calculation is DeltaS=.3, DeltaL=.1 and
Epost=.1 exp(-.8). After 300 seconds the capacity deficit must halve, STP must
multiply by exp(-.0038*300), and LTP must remain unchanged. Repeated practice
must reduce available learning rate; the ratio DeltaL/error isolates capacity
from ordinary error reduction.

Cross STP on/off with fatigue on/off. With STP off, fatigue plus recovery must
produce a durable spacing advantage. Removing fatigue or disabling recovery must
eliminate this advantage in that controlled case. With STP on, decaying STP can
reduce error-driven durable gain in massed practice even without fatigue. The
full-model spacing effect alone therefore does not identify fatigue causally.

The reusable experiment sweeps illustrative parameter values without selecting
winners. It reports all results, matched post-final-practice retention, a fixed
first-practice timeline, positive/negative updates, a seeded heterogeneous
autoassociation example, and machine-readable checks. This establishes what the
specified equations do; it does not validate a human-memory theory empirically.
