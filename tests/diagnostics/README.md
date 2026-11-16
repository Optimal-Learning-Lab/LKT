# Explicit Wald paper diagnostic

`wald-paper-review.R` is an opt-in native R diagnostic, not an automatic package
check. It loads only the installation selected by the shared current-release
loader. It never edits the scientific implementation or launches a sweep.

Run from `C:/dev/LKT` with an explicit new output directory:

```powershell
Rscript tests/diagnostics/wald-paper-review.R calibrate C:/dev/LKT-outputs/NEW-REVIEW-DIRECTORY
Rscript tests/diagnostics/wald-paper-review.R diagnose C:/dev/LKT-outputs/NEW-REVIEW-DIRECTORY
```

`calibrate` checks the declared V3 export, reproduces its 57,328-response,
180-student, 199-item cohort, fits linear accuracy coefficients at the confirmed
fixed nonlinear parameters, then performs one secondary Wald calibration with
the production fitting defaults. It refuses to replace a completed calibration.
This phase can take substantial time; it does not optimize nonlinear accuracy
parameters. `diagnose` validates and reuses the saved calibration. It fits the
PPES scheduler's linear coefficients at the fixed confirmed nonlinear parameters.

The diagnostic uses three anonymized students and six artificial items drawn
using the paper's trimmed coefficient population summaries. Its protocol is a
sequential pretest, 60 seconds of OET practice, one-day retention, another 60
seconds of practice, seven-day retention, and a sequential posttest. It calls the
maintained simulator's actual trial loop and checkpoint implementation. Each
invocation creates fresh execution subdirectories for ordinary, instrumented,
replayed, resumed and parallel runs.

Instrumentation uses temporary R function tracing to capture local values and
RNG states. It neither draws extra variates nor replaces the model or generator.
The independent quantile reference uses a conventional inverse-Gaussian CDF and
a bracketed root solver; density quadrature supplies a second numerical check.

Read `report.md`, `trace.csv`, `controlled.csv`, `checks.csv`, and `diagnose.log`.
`latency-rng-states.rds` records the per-call RNG evidence. The script reports
known contract failures as false checks rather than silently repairing them.
For example, rejection checks for malformed failure constants, an imported shift
above the cap, and mismatched effect vectors expose existing implementation
defects. Numerical checks report their measured errors rather than coercing them
to booleans. Review the actual outcomes before drawing a verdict.

`worker` is an internal subprocess phase for saved-closure portability.
`inspect` reads the confirmed historical accuracy-fit metadata and source
identity without fitting or simulating. Neither phase selects another package
installation. Generated fits, traces, logs and reports belong only in the
explicit output directory, never here.
