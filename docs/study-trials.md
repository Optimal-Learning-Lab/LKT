# Study exposures and scored responses

`Outcome` is authoritative when present. Supported fitting outcomes are
`CORRECT`, `INCORRECT` and `STUDY`. LKT rebuilds `CF..ansbin.` as 1, 0 and NA,
respectively, replacing legacy binary codes even when the binary column already
exists. Unknown or missing outcome labels are rejected during fitting. Binary-only
data remain supported for complete 0/1 responses; study events require `Outcome`.
Prediction also accepts `PENDING`, which differs from a committed study exposure.

Response histories count only scored answers. `expdecsuc`, `expdecfail`,
`propdec`, `propdec2`, `logitdec`, `logitdecevol` and `errordec` advance their
trial-based history only on responses. The 60-trial `logitdec` window means 60
responses. `recencysuc` and `recencyfail` use the immediately preceding scored
response and the time elapsed since it; an intervening study does not reset them.
Outcome-based success/failure counts likewise exclude studies. Time-based
decay can still change as time passes without a new response.

All AFM opportunity features (`lineafm`, `logafm`, `powafm`, `expdecafm`, and
`dashafm`) include every prior committed exposure, whether a study, success or
failure. The current trial is excluded. `lineafm` is the prior exposure count;
`logafm` and `powafm` transform that same count.

Exposure features retain studies. `recency` uses the preceding exposure;
`recencystudy` and `recencytest` gate that recency by whether the immediately
preceding exposure was a study. They do not search back for the latest event of
each type. `expdecafm`, `dashafm`, spacing, age, base and PPE timing calculations
retain exposure rows. Plain `ppe` is outcome-agnostic: its repetition factor and
spacing denominator both use all prior exposures. The `ppes` and `ppef` variants
retain their success/failure-specific repetition factors, with the same full
exposure spacing history. Base count factors still use scored correct/incorrect
counts. Component-exposure features such as `baseratepropdec` also retain studies.

Feature construction keeps the full exposure log. Built-in logistic, GLMM and
online adapters exclude study answers from fitting, likelihoods and response
updates, while returning predictions aligned with every input row. Accuracy
statistics use scored responses only. Custom adapters receive NA responses for
studies and must implement the same exclusion rule.

GRU/sequence models preserve `Outcome` in engineered-feature replay. Engineered
features use the complete exposure log; preprocessing estimates, recurrent
interactions and loss use only scored responses. A committed study is retained in
engineered exposure history without updating hidden, retrieval, adaptive-embedding
or response-clock state. It does not create a pending question. `PENDING` remains
an unanswered query awaiting completion. Continued and batched fitting histories
use the same response-only recurrence. Prepare `LKTGRUTimeFeatures()` on the full
exposure log before selecting any response rows when using its item timing inputs.

The simulator's shared calibration cleaner retains studies by default. Set
`"remove_study_trials": true` in the calibration JSON, or set the explicit process
environment flag `PF_REMOVE_STUDY_TRIALS=true`, to remove studies before history
construction. Only lowercase `true`/`false` are accepted for that environment flag;
an explicit environment value overrides the JSON value. Question-type and outcome
selection operations retain studies unless removal is enabled. The effective flag
is included in run and shared-model identities, preventing reuse across different
cleaning choices. Existing fitted results and historical cleaning snapshots are
unchanged.
