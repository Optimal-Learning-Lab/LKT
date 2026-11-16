# Training-only Lasso feature selection for Cloze GRU

Status (2026-11-11): experimental and opt-in. The completed Cloze comparison
improved mean seed-pooled held-out AUC from 0.859277061 to 0.863067102, using
14–15 selected history inputs and constant output plasticity. These results use
the existing Cloze folds. The user explicitly requested that this approach not
be made a default yet. Keep existing GRU defaults, builders and experiment
recipes unchanged; promotion requires a separate user decision.

The maintained pipeline is `lasso-gru-study.R`, with its Big Blue controller
`run-lasso-gru-batch.py` and Slurm wrappers in `hpc/`. It uses the selected package
through `load-current-lkt.R`; it never loads an alternate LKT implementation.

The fixed candidate pool crosses `Anon.Student.Id`, `KC..Default.`, and
`KC..Cluster.` with these functions:

* Zero parameters: `lineafm`, `logafm`, `linesuc`, `logsuc`, `linefail`,
  `logfail`, `prop`.
* One parameter: `logit`, `propdec`, `propdec2`, `logitdec`, `recency`,
  `recencysuc`, `recencyfail`, `expdecafm`, `expdecsuc`, `expdecfail`, `base`.

One-parameter values are 0.1 through 0.9. The total is
`3 * (7 + 11 * 9) = 318` scalar predictors. Identity intercepts, LLTM identity
interactions, cross-student evolving predictors, text, and multi-parameter grids
are outside this pool. Multi-parameter grids require a separate explicit design;
the package will not infer a Cartesian product.

## Stages

The R entry point takes `ROOT OUTPUT REFERENCE ACTION [TASK]`. Reference names
the immutable matched constant-output Cloze study, including its prepared data,
five whole-student partition files, settings and task list.

1. `plan` freezes the reference data/partitions, 318-column definition, package
   identity, runtime, script hashes, settings and fifteen fold/seed tasks. It
   rejects an existing study directory.
2. `select` precomputes the causal sparse bank once and fits one Lasso path on
   **role 1 students only** for each fold. Role 2 validation students are excluded
   from selection. The chosen lambda is the training path point nearest fifteen
   nonzero predictor columns, excluding the intercept. Ties prefer fewer columns
   and then stronger regularization. Held-out metrics cannot choose the subset.
   Different parameter versions of one family count as distinct columns.
3. Each fold saves its dictionary, coefficient path, training identities and
   precomputed selected inputs in one atomic RDS checkpoint. The same selection
   is reused across seeds 1, 2 and 3. Interrupted completed stages are validated
   and reused; incompatible artifacts fail explicitly.
4. `fit TASK` trains one fresh GRU with selected values as `numeric_cols` and
   `use_lkt_features=FALSE`. This replaces the old history-feature block. The
   reference's three timing coordinates, categorical embeddings, 32-wide encoder,
   three 32-unit GRU layers, retrieval, 128-unit head and **constant output
   plasticity** remain configured. Nonlinear coefficients from Lasso are not
   neural weights; the GRU learns its own network. Scalers use training rows.
   Validation selects the neural checkpoint; held-out students are evaluated
   separately. Saved weights are replayed with the shared package inference code.
5. `report` pools each seed's five held-out prediction sets and reports training,
   validation and held-out AUC and log loss, epochs, predictor counts, runtime and
   replay differences. These are exploratory comparisons on previously used
   Cloze folds.

The earlier three-history-feature recipe differs from the current frozen
five-numeric constant-output reference (PPES and student-item logitdec plus
three timing coordinates). The selected reference and its full recipe are
recorded in the new protocol; historical experiments are not relabeled.

## Execution and recovery

All scientific fits run on Big Blue. The Python controller requires Slurm,
single-thread BLAS settings and at most three concurrent GRU fits. It checks the
combined home/project 10 GB cap and stops launching pending tasks after a failure
or insufficient space; it does not terminate other jobs. Every attempt gets a
separate log. Completed model, training and prediction files receive SHA256
receipts and are verified on resume.

The deployment JSON explicitly binds the container, dependency library, release
descriptor, reference, Rscript and Python paths. No versioned installation is
hardcoded in these maintained scripts. Its hash is part of the frozen runtime
identity, alongside the study/controller/loader/Slurm/auditor script hashes.
Native package verification precedes
scientific execution. The headless native check records unavailable optional
GUI/rendering Suggests with `_R_CHECK_FORCE_SUGGESTS_=false`; none of the native
tests depends on those packages. All native tests remain enabled, and absence
of Torch is an error. The real semantic fixture is explicitly supplied. This
does not verify interactive GUI rendering or rebuild the vignettes.

Outputs, fits and reports belong under `C:/dev/LKT-outputs` after collection;
reusable implementation and tests remain in `C:/dev/LKT`.

`report-lasso-gru.R SOURCE_ROOT RESULTS BASELINE OUTPUT` verifies completed
artifacts, checks selected coefficients and training-input hashes, and compares
the existing constant-plasticity reference without retraining. It writes feature
stability, learned constant rates, epochs, paired results and a Markdown report.
The standard-library Python auditor independently checks all 45 phase AUCs.
`export-lasso-gru-audit.R` can generate canonical prediction exports from stored
fits for a separate audit without replacing original exports or fitting again.

The HPC deployment helper copies only a successfully checked package and verifies
every installed file. The retirement helper requires all account jobs to finish
and the study to complete, verifies the archived installation file by file, then
retires only the explicitly superseded dependency-library `LKT`. Other packages,
shared runtimes, source archives, protocols and results remain separate artifacts.
