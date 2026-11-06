# Development candidate (not released)

* Set GRUModel argument defaults to the selected three-layer, width-32 GRU with
  a 128-unit head, 16-value feature encoders and embeddings, filtered one-head
  retrieval, zero-initialized trainable item embeddings and 0.1 dropout. The GRU
  settings editor and configurations use these same argument defaults. Explicit
  settings remain supported; LibLinear remains the overall LKT default.

* Add one optional shared current-input MLP before retrieval, prediction and
  sequence updates, with configurable widths, activation and dropout. Disabled
  settings preserve the existing input representation and random streams.

* Add SequenceModel with GRU and native Mamba-1 units, opaque state operations,
  generic continuation APIs and portable sequence configurations. Preserve old
  GRU adapters, initialization and saved-weight contracts.
* Add independent Mamba numerical/gradient fixtures, state/causality tests and a
  frozen local 72-fit architecture comparison. This candidate is not promoted.

# LKT 2.0.0.9018

* Add literal sums or concatenation within the time/history and categorical input
  groups, and between the two group sums. No fusion network is introduced.
* Add independent output-width overrides for time, history and response encoders,
  allowing parameter-matched comparisons with unchanged recurrent/head widths.
* Retain concatenation defaults and expose every new control in FGUI. Validate
  sum dimensions and reject incompatible inference/cue configurations explicitly.

# LKT 2.0.0.9017

* Add categorical-only question cues for attention retrieval, keeping the full
  predictor inputs in the GRU and prediction head.
* Add zero initialization of selected, still-trainable embedding tables while
  preserving other initial weights and random streams.
* Add a separate learning-rate multiplier for retrieval decay, with rate histories,
  portable configuration and FGUI controls for all three options.

# LKT 2.0.0.9016

* Add exact-identity retrieval filters, matching any configured item/skill column
  before attention. Keep causal windows and per-student recurrent histories intact.
* Retain raw filter identities across minibatches, candidate scoring and saved
  student states, including unseen categories. Expose the option in FGUI.

# LKT 2.0.0.9015

* Add function-preserving zero initialization for additional input connections.
  Preserve ordinary random shared weights and the RNG endpoint, including the
  subsequent prediction-head and retrieval initialization.
* Add explicit warm-start expansion from an ordinary fitted GRU on matching data,
  preprocessing, architecture and partitions. Retain epoch zero as a validation
  checkpoint and record initial predictions and training/validation AUC histories.
* Expose initialization controls in FGUI and portable configurations. Fixed AUC
  direction prevents chance-level initial predictions from reversing the metric.

# LKT 2.0.0.9014

* Add configurable direct input connections to every GRU layer. Upper layers
  receive the original predictor-and-observed-answer interaction alongside the
  preceding layer's output, with a separate learned input matrix per upper layer.
* Keep first-layer-only input wiring as the default and preserve historical
  architectures. Support all-layer inputs independently of attention retrieval,
  raw/encoded inputs, inference modes, continuation and saved GUI configurations.

# LKT 2.0.0.9013

* Add optional one-head or multi-head attention retrieval over completed GRU
  memories. Expose head count, total width, rolling window, dropout, memory fusion,
  optional position/AKT-style decay, initial decay, shared projections and biases.
* Preserve exact existing architectures when the head count is zero. Carry causal
  retrieval banks across backpropagation chunks, retained runtimes and portable
  student states, with no updates from pending trials or candidate scoring.
* Support raw/encoded inputs and deterministic/Gaussian inference modes. Expose
  all retrieval controls in FGUI and saved model specifications.
* Verify attention against independent matrix equations, one/multiple heads,
  empty and bounded/unbounded histories, gradients, held-out isolation, uneven
  batches and continued prediction. Add an editable one-head preparation script.

# LKT 2.0.0.9012

* Add `LKTGRUTimeFeatures()` for item age since first exposure and average spacing
  among prior exposures. Compute these on complete event logs so tests, drills and
  studies count; the current timestamp is excluded from average prior spacing.
* Undefined spacing remains missing until two prior exposures exist. Explicit
  `time_feature_cols` and `time_missing_cols` route new timing values and availability
  indicators to the time encoder, with scaling estimated only on observed training
  values. Existing models keep their saved architecture and default input schema.
* Expose both timing controls in FGUI and test causal calculations, study exposures,
  scaling, serialization, pending trials and retained-runtime continuation.

# Version 2.0.0.9011
* Add optional, jointly trained response, time-gap and numeric-auxiliary feature encoders to plain and inference GRUs. Expose encoder hidden/output widths, activation and dropout in R, saved configurations and the FGUI.
* Preserve the original plain/inference architectures when feature encoding is disabled. Extend portable continuation, candidate scoring and retained runtimes to encoded models with explicit predict-before-observe ordering.
* Add a participant-split architecture search, validation-only capacity controls and Gaussian ablations, plus five outer participant folds with training-fold validation selection.

# Version 2.0.0.9010
* Add process-local LKTGRURuntime objects and independent LKTGRUScore candidate prediction. Reuse loaded weights during continued prediction without changing portable student states or caller random streams.

# Version 2.0.0.9009
* Add optional deterministic and Gaussian inference branches to GRUModel, with explicit latent width, hidden layers, activation, dropout, bounded log variance and temporal KL controls.
* Preserve causal prediction, chunk-boundary context, pending trials, portable state and deterministic mean-based evaluation for inference models. Keep plain-GRU defaults and archived checkpoint predictions unchanged.
* Add a frozen staged comparison with a matched-capacity prediction-head control, participant-paired contrasts and latent smoothness/variance diagnostics. Use the 144-item/36-skill Cloze structure and retain earlier experiments separately.

# Version 2.0.0.9008
* Add an optional Torch GRU backend with configurable recurrent/MLP architecture, raw and fixed LKT feature inputs, optimization, regularization, validation and runtime controls.
* Expose all GRU controls through saved configurations, generated R calls and the form GUI. Preserve causal response ordering, isolated fitting/validation/test phases and training-only preprocessing.
* Add explicit student and student-by-KC continuation states, pending-trial handling and portable RDS weights/state, with neural causality, persistence and learning regression tests.
* Complete the registered dashfail feature using the existing failure-specific DASH computation; verify causal feature construction across all supported engineered feature families.

# Version 2.0.0.9007
* Add explicit participant-level training/evaluation splits and a correlated logistic mixed-model population prefit for Bayesian priors. Estimate population means and between-participant covariance using training subjects only; evaluate separate subjects sequentially with the unchanged Bayesian adapter.
* Expose training fraction, split seed, participant column, personalized coefficients and particle diagnostics in a Training and evaluation tab. Save the assignment manifest, prefit and estimated priors with evaluation results; provide a split preview without fitting.
* Initialize integer GUI defaults correctly. Preserve supplied-prior workflows and reject singular or unconverged prefits without substitute priors.

# Version 2.0.0.9006
* Integrate OnlineBayesianModel, reusable Bayesian states and sequential prediction into the GUI candidate, preserving explicit priors, causal predictions and ESS diagnostics.
* Add Bayesian model configurations, a joint-prior editor, matrix-only predictor inspection and a tabbed blue/green fgui presentation with themed controls.
* Give GUI model outputs a default filename, create its directory when saving, and ask before every replacement of an existing file.

# Version 2.0.0.9005
* Retain Wald as the sole active latency model. Remove exponential and Weibull fitting, generation and specification helpers; reject retired distributions explicitly.
* Use Wald as the dual-fit default and expose only none/Wald in model configuration and fgui. Preserve the Wald mathematical implementation unchanged.

# Version 2.0.0.9002
* Recompute nonlinear features whenever a free or tied parameter changes, and rebuild the stored fit at the optimizer's returned optimum.
* Use collision-safe student-component history keys and complete automatic spacing/study preprocessing for registered dependent features.
* Preserve sequential predictions for both online models in predict_lkt; prediction-only requests do not require outcomes or fit diagnostics. Single-class AUC and null-relative R2 are explicitly undefined (NA).
* Correct student RMSE and match coefficient metadata by full feature/component prefixes.
* Require the documented request-based custom adapter interface, inspect optional verbose support before calling, and propagate backend errors once without retrying another API.
* Remove an overwritten baserate definition and four unreferenced internal helpers; connect and type-correct prevstudy.

# Version 2.0.0.9001
* Integrated centered normal-score Weibull and Wald latency likelihoods and self-contained predictors from the verified PolicyFactory2 research implementation.
* Weibull now uses equal-student centering and one normal trial-noise source. Historical weighted-MSE arguments are rejected; old saved models retain their original version identity.
* Added direct Wald fitting and explicit validated calibration import; the contamination allowance is used only for fitting.
* Latency observations now honor usefolds and the question-type filter while preserving complete feature histories.
* Preserved the 2.0 model adapters, native online-adaptive paths and component-aware parameter metadata.

# Version 2.0.0
* Added the generalized online-adaptive model with coefficient and nonlinear
  parameter adaptation.
* Added registered native evaluators for supported online-adaptive model shapes.
* Refactored model construction, feature computation, prediction, and search
  behind explicit interfaces.
* Replaced the historical monolithic examples vignette with focused,
  independently executable vignettes based on bundled package data.

# Version 1.0.0
* Initial Release
  
# Version 1.0.1
* Bugfix for problem with predictions too close to 1 or 0 causing infinite misfit
* Corrected the dependencies for the package

# Version 1.1.0
* "connectors" parameter added to LKT which allows controlling component interactions
  
# Version 1.2.0
* Added automatic KC generation using covariance clustering
* "curvefeats" parameter added to allow other inputs for curvilinear effects
* Added HDIntervals (coefficient credibility intervals)

# Version 1.3.0
* Updated examples
* Large test sample added
* Bugfix for bug in predict function causing random catastrophic failure

# Version 1.5.0
* computeSpacingPredictors speed upgrade using better functions
* buildLKTModel
* LASSOLKTModel
* New examples

# Version 1.6.0
* usefolds input to LKT specifies subset of data to fit and precomputed features for test folds are also created for convenience
* restrict LASSOLKT and buildLKTModel by optionally removing component feature pairs that are known to be unjustified

# Version 1.7.0
* corrected namespace bug
* added predict_lkt function
* update of vignettes
