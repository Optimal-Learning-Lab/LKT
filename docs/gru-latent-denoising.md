# Optional training-only latent denoising

`GRUModel(latent_denoising=TRUE)` adds the single Gaussian reconstruction component
from KeenKT Eq.10 to the existing GRU. It does not implement the complete KeenKT
architecture. It is disabled by default. Use the fixed transfer settings:

```r
model <- GRUModel(
  latent_denoising=TRUE,
  denoising_noise_std=.20,
  denoising_loss_weight=.15,
  plasticity='constant',
  plasticity_time_col='CF..Time.'
)
```

The existing model still requires its ordinary data, encoder and fixed-feature
settings. FGUI exposes a **Latent denoising** tab with `TRUE`/`FALSE`, `.20`, and
`.15` fields. A coefficient of `0` is the explicit no-effect verification control;
other noise levels and coefficients are rejected for this fixed component. The
settings round-trip through `lkt_model_config`, GUI encoding and saved fits.

For each real training event, let `h` be the 32-coordinate top GRU state immediately
before the answer, before retrieval can replace prediction memory. Draw independent
`epsilon ~ Normal(0,.20^2)` and compute:

```text
x = h + epsilon
F(x) = x + Linear32(ReLU(Linear32(x)))
training objective = existing supervised objective + .15 * mean((F(x)-h)^2)
```

The mean covers valid events and 32 coordinates. Padding is selected out before
noise is drawn. Observation weights and positive-class weighting continue to
apply to the supervised objective; they do not reweight the reconstruction mean.
Gradients pass through both the corrupted-state/reconstruction path and the clean
target. The original BCE, constant-output-adaptation gradient conventions,
truncated BPTT boundaries and validation-log-loss checkpoint rule are preserved.
The auxiliary is added once per training minibatch/chunk. There is no diffusion
schedule, normalization, stop-gradient target or iterative sampling loop.

The two biased 32-to-32 affine layers add exactly **2,112 training parameters**.
Their ordinary framework initialization runs after the baseline modules while
preserving the baseline Torch RNG state. With the feature disabled no auxiliary
module, state, random draw or tensor is allocated. With coefficient zero the
auxiliary loss/state/noise path is also inactive, preserving common initial
weights and the later dropout stream. For the frozen study baseline, total
training parameters are **45,154**, while prediction parameters remain **43,042**.
`parameter_count` includes all trained parameters; `training_only_parameter_count`
and `prediction_parameter_count` make the distinction explicit. Training history
records the valid-event mean as `training_denoising_mse`.

Evaluation, prediction replay, candidate scoring and continued student updates
never call the denoiser, draw its noise, or use its reconstructed values. Noisy or
reconstructed states never enter retrieval, the recurrent update, the prediction
head or online adaptation. Saved fits retain all auxiliary arrays for exact
checkpoint preservation; changing only those arrays cannot change predictions.
Historical fits lacking all three new controls are explicitly decoded as disabled;
partially present controls fail rather than reinterpreting a checkpoint.

The supported transfer uses a GRU with `hidden_size=32`, `inference_mode='none'`,
optional ordinary feature encoders/retrieval and `plasticity='none'` or `'constant'`.
Other experimental mechanisms, semantic input, per-student embeddings and warm
starts are rejected when latent denoising is enabled. `SequenceModel(unit='gru')`
can express the same controls; other sequence units reject this GRU-specific
component. This independent component has not established a benefit on the study
baseline, and no default promotion follows from its implementation.

`tests/gru-latent-denoising.R` exercises exact zero-coefficient predictions,
loss/gradient/common-weight and RNG parity; valid-event noise/masking; causal
state timing; joint target gradients; BPTT detachment; recurrent gradients;
parameter counts; fitting; GUI/config serialization; inference isolation; and
saved continued prediction. Run it against the verified package inside the
Big Blue Slurm/container runtime. Local source parsing does not establish those
runtime properties.
