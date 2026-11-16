# Compiled hidden decay and PPES factorial comparison

The maintained implementation uses `hidden_decay_parameterization='amplitude'`:
rate = a exp(b faded-DK), with a initialized to zero and constrained nonnegative
by projection after ordinary optimizer updates. DK fades at softplus(c) per day.
The three shared scalars start at zero. The compiled TorchScript recurrence
passes live weights and preserves autograd; it has no uncompiled execution
fallback. Historical enabled models select the original softplus definition.
See [the model equations](../docs/gru-hidden-decay.md).

`compare-gru-hidden-decay-fast.R` freezes a separate 60-fit Cloze study: four
conditions (`stock_full`, `decay_full`, `stock_no_ppes`, `decay_no_ppes`), five
unchanged whole-participant folds, seeds 1–3. It reuses the hash-verified existing
numeric-ablation data and participant roles. All training controls remain fixed.
PPES removal deletes its actual input and its four fixed nonlinear constants;
both logitdec terms and their constants remain. Models have 43,073/43,076
parameters with PPES and 43,041/43,044 without. Full/no-PPES numeric schemas have
six/five coordinates. Shared initial weights match within each stock/decay pair.
Changing input width changes random initialization across feature groups.

Invocation: `Rscript compare-gru-hidden-decay-fast.R ROOT OUTPUT ACTION TASK
CANDIDATE_JSON CANDIDATE_LIBRARY [REFERENCE]`. Actions are `plan`, `verify`,
`smoke`, `fit`, `report`, `check`. Only plan accepts REFERENCE. The candidate
artifact and installation are selected and hash checked explicitly. This study
does not promote the central maintained release.

`stage-gru-hidden-decay-fast.py` builds a source-only upload from a verified
generated package build input. It uploads no learner rows or fitted models.
Remote setup verifies existing data/container hashes, reconstructs the package's
two sample files from an existing verified archive, builds and installs the
candidate privately, runs model and report tests, freezes the plan, verifies all
20 schemas/initial weights and runs four one-epoch smoke fits. The Slurm array
runs tasks 0–59. Reporting requires all 60 completion receipts.

Checkpoint selection uses minimum inner-validation log loss, with the existing
100-epoch cap, patience 10 and all remaining controls. Reported held-out metrics
pool five folds within each seed and average the three seed metrics. No held-out
outcome selects models or parameters. Full-stock predictions must reproduce the
preceding six-input controls within 3e-6.

The report includes every fold/seed's train, validation and test AUC/log loss,
learned a/b/c, baseline hidden half-life log(2)/a (Inf at a=0), DK half-life
log(2)/softplus(c), epochs, times and saved-weight replay discrepancy. b has no
standalone half-life. Fitted values are estimates, not guaranteed global optima.
Decay benefit uses decay-minus-stock AUC and stock-minus-decay log loss, so both
are favorable when positive. The interaction is benefit without PPES minus
benefit with PPES. Inspect both constituent effects before interpreting it.

The synthetic report fixture is explicitly labeled TEST ONLY and never enters
the research results. Historical experiment scripts and outputs are preserved.
