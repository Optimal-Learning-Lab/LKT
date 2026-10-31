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
