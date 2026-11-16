# Full-data calibration and fixed nonlinear defaults

Each LKT recipe has one initial accuracy-only nonlinear fit on the complete
Cloze dataset. Its estimated nonlinear parameters become explicit fixed values
for that recipe. Save and reuse the fitted accuracy coefficients and predictions;
do not optimize them again to add response time or start a simulation.

The PPES/PPEF ground-truth recipe has eight nonlinear values. Its complete-data
accuracy fit is already owned by the separate eight-input experiment. This
workflow requires that immutable checkpoint explicitly and never repeats it.
The PPES plus student/student-item logitdec control recipe has six values and
receives one initial full-data accuracy-only calibration here.

After both calibrations, call the maintained Wald fitter once on the final
ground-truth predictions. Attach the resulting latency model to the saved
accuracy model without refitting accuracy or changing its predictions,
coefficients or design matrix. Save `ground_truth.rds`, `control.rds` and
`fixed_defaults.json` with hashes and full-data provenance. Resuming the finish
step reuses its completed Wald artifact. Preliminary optimizer results remain
as provenance; downstream configurations contain only finite fixed values.

`scripts/fixed-cloze-calibration.R` accepts
`ROOT PROJECT DATA EXISTING_FULL_GT_RECORD OUTPUT ACTION`, with actions `plan`,
`control` and `finish`. `plan` is read-only. The SLURM launcher accepts a third
`control` or `finish` action. Submit `finish` with scheduler dependencies on the
control and existing source job, so waiting reserves no CPU or memory. The one
Wald calibration requires the existing full-data accuracy checkpoint. It does not cancel or modify either
older job, copy their algorithms, or install a different package.

The comparison driver accepts an explicit fifth `FIXED_CALIBRATION` argument;
the eight-input driver accepts an explicit seventh argument. Their launchers
accept the same directory as a third launcher argument. This mode requires a
verified complete frozen calibration, loads both accuracy models, and passes
the six fixed history parameters into GRU training. It never starts another LKT
nonlinear or accuracy calibration. Missing/incompatible artifacts fail rather
than triggering fitting. Older explicit unfrozen commands retain their original
behavior for artifact compatibility; they are not the commands for the corrected
run. No package-global feature defaults or historical artifacts are rewritten.

Both models use all 55,122 calibration rows. GRU folds still separate neural
weight training, but metrics are conditional on the full-data calibration.
Every simulated participant shares the same frozen source and tracer accuracy
parameters; only assigned traits and student-local state vary.

The focused check verifies zero free nonlinear parameters, exactly one call to
the latency fitter, unchanged accuracy/source objects, and rejection of a
second Wald attachment. No full-data fitting occurs inside that check.
