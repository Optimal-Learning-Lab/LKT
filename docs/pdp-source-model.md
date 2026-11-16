# Source-matched controlled PDP model

`R/lkt-pdp-source.R` preserves the connection equations and operation order in
Philip Pavlik's `demo pdp auto13.R` (April 2004). It is independent of the GRU
and the earlier positive-rate reconstruction in `lkt-pdp-controlled.R`.
The original files and earlier results are unchanged. No parameters are fitted.

## Quantities and literal equations

For each connection, S is short-term weight, T is target long-term weight,
L is expressed long-term weight, and E is the current signed LTP learning rate.
Initially S=T=L=0 and E=E0. A model contains fixed parameters: STP learning
rate a, resting LTP rate E0, recovery coefficient r, fatigue coefficient k,
consolidation coefficient g, sigmoid heat h, and per-connection STP decay d.
The original initialization draws d from an exponential distribution and caps
it at .8. Inputs x are unnormalized sparse row vectors with values -1, 0, +1.
All weight and rate operations below are elementwise unless stated otherwise.

Full-input output is y = 2 / (1 + exp(-x(S+L)/h)) - 1, using matrix multiplication.
The direction matrix is D = outer(x-y, x), **error outer input**, exactly as
written in the original, despite the row-vector forward convention. We do not
silently transpose it into a conventional gradient. Both increments use the
old rate: deltaS = a D; deltaT = E D.

One training event executes, in order:

1. S1 = S + deltaS; T1 = T + deltaT.
2. L1 = L + (T1-L) g, using the newly updated target.
3. S2 = S1 - S1 d.
4. Er = E + (E0-E) r.
5. E1 = Er - Er abs(deltaT) k.

One retention/interference event executes, in order:

1. Compute output and increments from the incoming state, then S1=S+deltaS,
   T1=T+deltaT, as in training.
2. Ef = E - E abs(deltaT) k.
3. E1 = Ef + (E0-Ef) (1-exp(-40 r)).
4. S2 = S1 exp(-40 d).
5. L1 = L + (T1-L) (1-exp(-40 g)).

Thus training recovers before fatigue, whereas retention fatigues before
recovery. A training event uses discrete coefficients; a retention event spans
40 original time units and uses exponential coefficients. These are source time
units, not an asserted conversion to seconds or days.

The literal fatigue multiplier can be negative. No clipping, positive
reparameterization, rate reset, or additional learning rule is introduced.
Negative E reverses deltaT relative to D. Because D is not generally the gradient
of the row-vector forward map, this is described as reversal of the original
learning direction, not necessarily gradient ascent on an objective. Fatigue can
move a negative E toward zero or even flip its sign again. It is not always a
monotone reduction in E. Nonfinite states stop with an explicit error.

## Rest, scoring and student state

Controlled rest is zero-input iterations of the selected original phase. It is
an explicit diagnostic intervention; the historical simulation filled retention
with random learned patterns. With zero input D=0, so T is constant. For n
training ticks the exact closed forms are S'=S(1-d)^n,
E'=E0+(E-E0)(1-r)^n, L'=T+(L-T)(1-g)^n. For n retention blocks replace those
three powers by exp(-40dn), exp(-40rn), and exp(-40gn). Tests compare these
closed forms to repeated literal zero-input updates. This is an algebraic
optimization of the original equations, not an alternative update rule.

Recall zeros the second half of x, propagates the first half through S+L, and
dot-products the predicted second half with its target. A nonpositive strength
is assigned log-strength -10; otherwise use log(strength). Probability is
1/(1+exp((threshold-log-strength)/noise)), with source threshold 1 and noise .4.
Scoring neither learns nor advances time. Each learner has separate S,T,L,E
matrices; there is no stored event history and no shared mutable learner state.
This controlled model has no outer optimizer or training/testing split. A
retention interference event learns a new pattern; a recall test only scores.

## Reproducible qualification

Run `scripts/run-pdp-source.R ROOT CANDIDATE_LIBRARY FRESH_OUTPUT ORIGINAL_AUTO13_R`.
The explicitly selected candidate must match the maintained source manifest.
The runner parses the original file but never sources it, reads its numeric
starting parameters and practice order, and evaluates only the original two
update loops in an isolated reference environment. It compares all four state
matrices and pre-learning recall scores for a small asymmetric network and the
full 200-unit original schedule, including 1575 retention interference patterns.
The full-schedule parity uses the explicit historical R 3.5 sampling convention
and original seeds 49/512789. It is an equation/schedule verification, not a
replication of the original empirical fit or recovered optimizer output.

The source starting parameters are a=.010214728, E0=.007472300, r=.005629399,
k=.188602174*1000, exponential decay mean=.060921298, g=.032804697, h=.35.
The paper reports different rounded fitted values and a different interference
schedule. We use the script's starting values consistently and do not silently
combine those versions.

Diagnostics include a 100-practice signed-rate trace; analytic recovery;
four-practice spacing with 0/1/13/97 intervening zero-input ticks; causal controls
removing fatigue, STP or recovery; and matched idle versus interference retention.
The fatigue-only control sets STP learning to zero and g=1 to eliminate delayed
expression as a competing explanation. Removing fatigue or recovery from that
control tests whether the spacing effect requires both. All controls are explicit
interventions, not edits to the source-matched baseline.

A 40-unit sparse autoassociator uses all seeds 1 through 20, all gaps 1/13/97,
four target presentations, and 100 retention blocks. Within each seed, patterns,
decays and interference are identical across gaps. Retention is measured after
the final target, so longer spacing does not shorten the retention interval.
Paired tables report every seed, including neutral or reversed effects. A
two-unit conflicting-association intervention separately tests causal forgetting.
These demonstrations do not estimate human parameters or establish GRU benefit.
