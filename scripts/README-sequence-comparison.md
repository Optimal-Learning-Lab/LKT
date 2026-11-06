# GRU versus native Mamba-1

The driver freezes an experiment and calls LKT; model implementation stays in R/.
It reads the completed KTST protocol at
`C:/dev/LKT-outputs/gru-ktst-comparison-20261104/local-plan/study.rds`, verifies prepared-data
and assignment hashes, and reuses five participant folds plus development.
The direct recipe disables feature towers, retrieval and zero item initialization;
the rich recipe freezes all_three. Both retain common prepared predictors, timing
values and fixed nonlinear feature parameters.

After neural regression and package checks against the explicit candidate build:

```powershell
python scripts/run-sequence-experiment.py --root C:/dev/LKT `
  --output C:/dev/LKT-outputs/sequence-mamba/experiment `
  --library C:/dev/LKT-outputs/sequence-mamba/library `
  --rscript 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --workers 2
```

The supervisor freezes the protocol, runs four full-data one-epoch smoke fits,
then 72 fresh fits. Two single-threaded workers are the default. Failures stop
new work. Re-running resumes missing tasks after candidate and driver identity
checks. Every saved model independently replays from weights before acceptance.
Partial results cannot produce a final report. status.json, logs and per-fit
training CSVs show progress; completion produces reports/RESULTS.md and CSVs.
Atomic progress publication retries brief Windows access denials for up to two
seconds. A persistent launcher error stops new work and waits for its active fits
before exiting. Resume only after surviving workers finish, so tasks are not
duplicated. The first local run records its status-writer amendment in
`experiment/provenance/protocol-original.rds` and `protocol.rds`; all scientific
settings, assignments, candidate identity and R drivers are identical across that
orchestration-only change.

Main AUC pools five test folds within each seed then averages three seed AUCs.
Development is separate. Use 2,000 paired whole-participant bootstrap draws shared
across models and seeds. The interaction is rich minus direct architecture effect.
Also report calibration, log loss, Brier, timing, size and selected epochs.

Common schedule: CPU float32, one Torch thread, AdamW .001, weight decay .0001
(with Mamba's reference exclusions), batch 32, chunk 100, clipping 5, maximum 100
epochs, patience 10, min_delta .0001 and validation-log-loss checkpoint selection.
Three layers and 32-value output widths are shared, not parameter counts.
This fixed-setting study on repeatedly inspected data is exploratory. Timings
describe native R/Torch, not optimized CUDA. No remote jobs, central release
promotion, historical-result changes or simulator modifications occur.

The local study uses four concurrent workers, each with one Torch thread. Fit
and scoring times describe that shared Windows workstation, including ordinary
desktop contention and availability; they are descriptive resource measurements,
not isolated hardware throughput estimates. Worker concurrency does not change
the frozen model settings, data assignments or seeds.

On Windows the launcher explicitly uses the supported C locale for native R;
the desktop shell's C.UTF-8 locale is not accepted by this R installation.
