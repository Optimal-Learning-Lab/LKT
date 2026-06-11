# LKT GUI Interface Plan

Goal: build a compact GUI for fitting LKT models from a dataset, selecting model terms from valid dataset columns and package-supported features, then displaying fit results and diagnostic plots. Treat the GUI as a thin interface over existing LKT behavior; do not change exported model semantics to satisfy GUI convenience.

## Placement

Start as a prototype under `scripts/` or `inst/shiny/lkt-model-builder/`.

- `scripts/` is safest for early iteration because it is excluded from package builds.
- `inst/shiny/lkt-model-builder/` is appropriate once the app is stable enough to ship with the package.
- Do not add `shiny` to package dependencies until there is a decision to ship the app as an installed package feature.

## First Milestone

Implement a Shiny prototype with four compact areas:

1. Data panel
2. Model-builder table
3. Fit/results panel
4. Plots panel

Use tabs, compact tables, and tight controls. Avoid airy dashboard spacing so several related parts of the model can be viewed at once.

Current prototype files:

- `scripts/lkt-model-builder/app.R`: compact Shiny UI/server.
- `scripts/lkt-model-builder/helpers.R`: Shiny-independent data, model, and plot helpers.
- `scripts/lkt-model-builder/smoke-test.R`: runnable helper smoke test using `samplelkt`.
- `scripts/lkt-model-builder/shiny-server-test.R`: Shiny `testServer()` check for fit-button behavior and KC-by-trial server data.

Run the smoke test from the package root:

```r
source("scripts/lkt-model-builder/smoke-test.R")
```

Or from PowerShell:

```powershell
Rscript scripts\lkt-model-builder\smoke-test.R
Rscript scripts\lkt-model-builder\shiny-server-test.R
```

Launch the app after installing `shiny`:

```r
shiny::runApp("scripts/lkt-model-builder")
```

Or from PowerShell:

```powershell
Rscript scripts\lkt-model-builder\run-app.R 7865 127.0.0.1
```

As of the current prototype, the helper smoke test verifies the default GUI model fit, a multi-term GUI model fit, feature filtering, and the KC-by-trial summary against `samplelkt`. The Shiny server test verifies the app fit action, rendered model call, stored predictions, and KC-by-trial server data. The local Shiny dependency is installed, and the app has been launched locally with an HTTP 200 response containing the `LKT Model Builder` page title.

## Data Panel

Inputs:

- Built-in sample selector: start with `samplelkt`.
- Optional file upload later: `.csv`, `.rds`, `.RData`.
- Column selectors for subject, response, and time fields.

Default column detection:

- Subject: prefer `Anon.Student.Id`.
- Response: prefer `CF..ansbin.`; if missing and `Outcome` exists, derive `CF..ansbin.` as `Outcome == "CORRECT"`.
- Outcome: prefer `Outcome` when present.
- Time: prefer `CF..Time.`, then `CF..reltime.`, then `Duration..sec.`.
- Fold: prefer `fold` when present.
- Components: candidate factor/character/integer columns excluding response, outcome, time, fold, and obvious numeric predictor columns.

Data validation should produce explicit messages. Do not silently guess a required column if several plausible columns conflict.

Current prototype status: selecting `Outcome` as the response derives `CF..ansbin.` for fitting and plotting, so the KC-by-trial plot can use the package-standard binary response column even when the raw dataset does not contain it.

## Model Builder

Represent the model as a compact editable term table:

| On | Component | Feature | Fixed par | Interaction | Connector |
|----|-----------|---------|-----------|-------------|-----------|

Controls:

- `Component`: dropdown from detected component columns.
- `Feature`: dropdown from supported LKT feature names.
- `Fixed par`: numeric input or blank/`NA`.
- `Interaction`: dropdown from `NA` plus candidate columns.
- `Connector`: dropdown with `+`, `:`, `*`; applies between adjacent active terms.

Primary buttons:

- `Add term`
- `Remove selected`
- `Compute spacing`
- `Fit model`
- `Reset`

Initial default model:

```r
components <- "KC..Default."
features <- "intercept"
interc <- TRUE
```

If `KC..Default.` is not present, choose no component automatically; require the user to select one.

## Feature Dropdown Rules

Start with a conservative feature list from the package feature registry and existing examples:

```r
c(
  "intercept", "numer", "lineafm", "logafm", "powafm",
  "linesuc", "linefail", "logsuc", "logfail",
  "recency", "recencysuc", "recencyfail", "recencystudy", "recencytest",
  "expdecafm", "expdecsuc", "expdecfail",
  "propdec", "propdec2", "base", "base2", "base4",
  "ppe", "ppes", "ppef",
  "diffrelcor1", "diffrelcor2", "diffcor1", "diffcor2",
  "diffcorComp", "diffincorComp", "diffallComp",
  "diffincor1", "diffincor2", "diffall1", "diffall2",
  "dashafm", "dashsuc", "dashfail",
  "linecomp", "logit", "errordec", "logitdec", "logitdecevol",
  "baseratepropdec", "prop"
)
```

Filter or warn based on data requirements:

- Spacing/recency features need component spacing columns or enough timing data to call `computeSpacingPredictors()`.
- `numer` needs the selected component to be numeric.
- Multi-parameter features such as `base2`, `base4`, `ppe`, `ppes`, and `ppef` need either `NA` or the required number of comma-separated fixed parameters.
- Interaction columns must exist in the dataset.
- Component specifications using `%` or `?` can be added later as advanced mode.

Current prototype status: term rows filter feature choices by the selected component. `numer` is hidden for nonnumeric components, and spacing/recency features are hidden when the dataset lacks both precomputed spacing columns and enough timing data to compute spacing predictors.

The Model tab also renders a compact current-term preview table, and the Results tab renders a copyable `LKT()` call that includes components, features, fixed parameters, interactions, connectors when needed, `interc`, and `verbose`.

## Fit Flow

On `Fit model`:

1. Validate subject and response columns.
2. Derive `CF..ansbin.` only when explicitly needed and the source is unambiguous.
3. Compute spacing predictors only when requested or when fitting a spacing-dependent feature with clear timing data.
4. Build vectors for `components`, `features`, `fixedpars`, `interacts`, and `connectors`.
5. Call `LKT()` with `verbose = FALSE`.
6. Store the fitted object and the exact model call.

Representative call:

```r
fit <- LKT(
  data = gui_data,
  components = term_table$Component,
  features = term_table$Feature,
  fixedpars = term_table$FixedPar,
  interacts = term_table$Interaction,
  connectors = term_table$Connector[-1],
  interc = input$intercept,
  verbose = FALSE
)
```

The GUI should show errors inline near the fit button and leave the last successful fit visible.

## Results Panel

Show compact outputs:

- Fit status and elapsed time.
- Model call as copyable R code.
- Coefficients table from `fit$coefs`.
- Fit metrics when present: log likelihood, AUC, RMSE, R2, BIC/AIC if available.
- Prediction preview from `fit$prediction` and key columns from `fit$newdata`.
- Warnings or validation notes.

Do not hide important fit messages behind large narrative text.

## First Plot: KC By Trial

Definition: for each subject and KC, number the trials within that KC sequence; then average correctness by KC and trial number across subjects.

Required fields:

- subject column, default `Anon.Student.Id`
- KC/component column selected by the user, default `KC..Default.`
- binary response column, default `CF..ansbin.`
- optional ordering column, default `CF..Time.` when present, otherwise the current row order

Data calculation:

```r
kc_trial_summary <- function(data, subject_col, kc_col, response_col,
                             order_col = NULL) {
  dt <- data.table::copy(data.table::as.data.table(data))
  dt <- dt[!is.na(get(subject_col)) &
           !is.na(get(kc_col)) &
           !is.na(get(response_col))]
  dt[, source_row := .I]
  if (!is.null(order_col) && order_col %in% names(dt)) {
    data.table::setorderv(dt, c(subject_col, kc_col, order_col, "source_row"))
  } else {
    data.table::setorderv(dt, c(subject_col, kc_col, "source_row"))
  }
  dt[, trial := seq_len(.N), by = c(subject_col, kc_col)]
  dt[, .(
    probability_correct = mean(get(response_col)),
    n = .N
  ), by = c(kc_col, "trial")]
}
```

Plot:

```r
ggplot2::ggplot(plot_data, ggplot2::aes(
  x = trial,
  y = probability_correct,
  color = .data[[kc_col]],
  group = .data[[kc_col]]
)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(ggplot2::aes(size = n), alpha = 0.75) +
  ggplot2::coord_cartesian(ylim = c(0, 1)) +
  ggplot2::labs(
    x = "Trial within KC",
    y = "Mean probability correct",
    color = "KC",
    size = "N"
  )
```

Options:

- Aggregation selector: `By KC` keeps one curve per KC; `All cases` collapses across KCs into one mean-by-trial curve using the same trial-within-subject-and-KC definition.
- KC selector for the plot independent of the fitted model component.
- Minimum `n` filter.
- Limit to top K KCs by observation count.
- Toggle points, lines, and smoothing.

## Later Plots

Add after the first milestone:

- Observed vs predicted by trial.
- Prediction calibration bins.
- Learning curves by selected KC.
- Coefficient plot with confidence/uncertainty when available.
- Residual or error-rate plots by subject/KC.

## Verification

Small prototype changes:

- Run the app with `samplelkt`.
- Fit the default model.
- Confirm the coefficient table and KC-by-trial plot render.

Significant package-facing changes:

- Run relevant example scripts or vignettes.
- Preserve existing public APIs and output object shapes.
- Do not treat the GUI smoke test as package release confidence.

## Open Decisions

- Whether the first prototype should live only in `scripts/` or be prepared immediately for `inst/shiny/`.
- Whether fitting should happen automatically after term changes or only on explicit `Fit model`; explicit fitting is safer for expensive models.
- Whether Shiny should become a suggested dependency once the app is stable.
- Whether the GUI should support advanced component specifications (`%`, `?`) in the first release or a later advanced mode.
