# Comparing separate student inference with the current GRU

This is an architectural ablation within LKT, not a reproduction of UNVaMP's
unpublished layer settings. It uses `largerawsample`, the original 144 item labels
and 36 skill labels, and the existing seed-1 whole-participant split. It does not
resolve the paper's response-inclusion or split differences.

## Prespecified conditions

| Condition | What changes from the preceding main stage |
|---|---|
| `baseline` | Current GRU and neural prediction head. |
| `deterministic` | Separate inference of a student vector from older memory, the latest completed interaction, and current numeric predictors. |
| `gaussian` | Same shared network, plus a log-variance output and one reparameterized sample per training trial. |
| `gaussian_kl_1e-03` | Same Gaussian model with beta 0.001 times adjacent-distribution KL. |
| `capacity_control` | Additional baseline head width, chosen to approximately match the deterministic model's parameter count. |

The default has **45 fits**: five conditions, three seeds, and all three input
families (matched engineered features, raw item/skill/time, and augmented).
All stages share a three-layer, 32-unit GRU, learning rate 0.001, AdamW, 100-epoch
maximum, validation early stopping, 16 values per item/skill embedding, and a
32-unit prediction head except the capacity control. Inference uses one 32-unit
ReLU hidden layer and a 32-coordinate student vector. Input/head/recurrent dropout
is 0.1; inference dropout is zero so the sampling comparison introduces no extra
dropout mechanism. These are fixed settings, not separately optimized winners.

The capacity control changes only the baseline head width. The nearest integer
width is determined from actual network parameter counts before training; its
remaining count difference is visible in the manifest. The experiment reports
deterministic-versus-baseline and deterministic-versus-capacity-control contrasts.
It does not claim that this head control exhausts all deeper/wider GRU alternatives.

## Evidence and state semantics

To predict response t, the separate inference network receives:

1. Top-layer GRU memory through interaction t-2.
2. The encoded predictors and observed answer from completed interaction t-1.
3. Numeric predictors available before response t, including a supplied time gap.

The inference network produces a mean vector, and in Gaussian mode a bounded
log-variance vector. Current categorical embeddings enter the decoder alongside
the inferred student vector. Current numeric predictors enter the inference
network; in augmented mode these include causal engineered LKT features, so the
student summary can depend on current item-specific numeric predictors. It is not
claimed to be item-independent or an interpretable proficiency estimate.

GRU memory is updated independently from the completed interaction. The inferred
student vector is never fed back into recurrent memory. The current response is
excluded from its own mean, variance and prediction. The first trial has zero older
memory and zero previous-interaction evidence; there is no invented previous answer.

Gaussian training uses `z = mean + exp(logvar/2) * noise`. Validation, testing and
continuation use the mean, with all dropout disabled. The log-variance output starts
at -2 and is clamped to [-10,4]; both choices are explicit controls. With beta zero,
variance can approach its lower bound, so the report includes its mean and boundary
frequency rather than treating a Gaussian output as calibrated uncertainty.

The temporal term is `KL(new distribution || previous distribution)`, averaged
over latent dimensions and valid adjacent pairs. It excludes the first state of
each participant and padding. Detached context retains the last distribution and
recent interaction across chunks; chunk boundaries do not remove the penalty.
Early stopping always uses validation prediction log loss, not the combined loss.

## Run locally

Use the current package selected by `scripts/load-current-lkt.R` and the central
release definition. Archived runs retain their frozen source and release identities.

```powershell
$env:R_LIBS = 'C:/dev/LKT/.codex-r-lib'
$env:LC_ALL = 'English_United States.utf8'
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla `
  scripts/compare-gru-inference.R C:/dev/LKT `
  C:/dev/LKT-outputs/gru-inference-new-run plan 3 all
```

Replace `plan` with `run` using the same folder and unchanged source to execute
the frozen experiment. A `smoke` action requires a separate folder: it uses the
first 12 real responses per participant, one epoch and one seed. Its outputs are
explicitly labeled pipeline checks and must not be used as scientific results.

An optional sixth argument names a design RDS containing exactly `seeds`,
`kl_betas`, `policy_root` and `common`. The default design is saved in the protocol.
`common` accepts the ordinary shared GRU controls, including all inference widths,
activation and variance bounds. Stage-defining options and family inputs are
assigned by the experiment. Declare additional positive KL weights in `kl_betas`
before fitting; all declared weights are reported without test-based selection.

The PolicyFactory2 parameter file is read only to specify the fixed engineered
inputs. This experiment does not run or change simulations and does not fit a
PPES comparator. It compares the neural conditions with one another.

## Reports and verification

- `inference_protocol.rds/.txt`: resolved settings, data/source hashes, design and limitations.
- `inference_runs.csv`: stages, seeds, actual parameter counts and control widths.
- `scores_by_seed.csv` and `stage_summary.csv`: validation/test log loss, AUC and RMSE.
- `paired_stage_differences.csv`: prespecified loss contrasts with 2,000 whole-participant bootstrap intervals.
- `test_predictions.csv`: probabilities linked to original source rows.
- `verification.txt`: independent metric checks and held-out prediction replay from every checkpoint.

The report also includes mean-coordinate total variation, adjacent Gaussian KL,
mean latent variance and variance-bound frequency. Parameter counts and epoch-cap
hits are visible. Every declared fit must finish before test summaries are created.
No condition or KL weight is chosen on test performance. Seed metrics average
separate models, not ensemble predictions. Intervals condition on this previously
inspected split; this is exploratory, not cross-validation or a confirmatory test.

After training, reports can be rebuilt with
`Rscript scripts/summarize-gru-inference.R OUTPUT` in the matching package runtime.
The Big Blue launcher is `scripts/hpc/run-gru-inference.slurm`; it requires a new
prepared workspace and the correct container/private library. No job is submitted
by creating a plan.

## Public API

`GRUModel(inference_mode = "none")` retains the ordinary GRU. Set
`inference_mode = "deterministic"` or `"gaussian"` and optionally specify
`latent_size`, `inference_hidden_sizes`, `inference_activation`,
`inference_dropout`, `temporal_kl_beta`, `logvar_bounds` and `logvar_init`.
These controls round-trip through saved configurations, generated calls and the
GRU settings GUI. Nonzero temporal KL is rejected outside Gaussian mode.

`predict_lkt`, `LKTGRUState` and `LKTGRUStep` support all three modes, including
pending trials and saved-state continuation. Archived plain-GRU fits are resolved
with inference mode `none`, preserving their original network weights and behavior.
