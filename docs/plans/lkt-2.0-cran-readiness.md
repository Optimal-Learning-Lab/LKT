# LKT 2.0 CRAN Readiness Plan

## Goal

Release LKT 2.0.0 as a clean, portable CRAN package whose installed behavior,
documentation, tests, compiled code, and declared dependencies match the
source tarball submitted to CRAN. Research analyses and generated outputs stay
in `LKT-Projects` and are not package inputs.

## Release invariants

- Package version is `2.0.0`; the published CRAN `1.7.0` source is not reused
  as the version identity for materially different code.
- `R CMD build` produces only intentional package source and documentation.
- `R CMD check --as-cran` runs against the built tarball, not the working tree.
- The generalized online-adaptive implementation under `R/` and `src/` is
  package functionality. Full-data trajectory and clustering analyses are not.
- Tests use small deterministic package data and do not depend on files in
  `C:\dev\PolicyFactory2\research` or any maintainer-specific path.
- Native and R implementations either agree within documented tolerances or
  fail explicitly; neither path silently substitutes for the other.

## Phase 1: source-tree hygiene

1. Change `DESCRIPTION` from `1.7.0` to `2.0.0` and add a dated LKT 2.0 section
   to `NEWS.md` describing the new online-adaptive model and compiled path.
2. Remove tracked session and backup artifacts, especially `R/.Rhistory` and
   `vignettes/Examples.Rmd.orig`.
3. Audit root dot-directories such as `.agents/` and `.codex/`; relocate them
   outside the package root or exclude each intentional development-only path
   explicitly in `.Rbuildignore`.
4. Review `doc/` against its sources in `vignettes/`. Delete redundant tracked
   generated copies rather than maintaining two documentation sources.
5. Keep package-maintenance runners under an excluded development directory,
   but move research runners and outputs to `LKT-Projects`.
6. Build a tarball and inventory every included path. Treat an unexplained file
   as a release blocker rather than adding a broad catch-all exclusion.

## Phase 2: formal regression coverage

1. Add the standard `tests/testthat/` structure and declare `testthat` in
   `Suggests`, subject to explicit dependency approval before implementation.
2. Convert the valuable assertions from the former full-data scripts into
   small deterministic tests for:
   - `OnlineAdaptiveModel` construction and `LKT(model = "online_adaptive")`
     dispatch;
   - coefficient learning-rate selection;
   - recency nonlinear parameter adaptation;
   - PPES nonlinear parameter adaptation;
   - registry, prediction, final-state, and optimizer result shapes;
   - native-versus-R objective, prediction, and final-state agreement;
   - explicit failure when `require_native = TRUE` cannot serve a requested
     model shape.
3. Add coverage for argument validation, parameter bounds, subject ordering,
   missing values, and degenerate response or feature inputs.
4. Keep expensive full-data benchmarks in `LKT-Projects`; formal tests must be
   bounded enough for CRAN's shared check infrastructure.

## Phase 3: package metadata and API audit

1. Regenerate `NAMESPACE` and `man/` from a single documentation source and
   inspect every diff. Remove hand-maintained/generated disagreement.
2. Verify every `Depends`, `Imports`, and `Suggests` entry against actual
   namespace use, examples, tests, and vignettes. Add no package solely because
   a research project uses it.
3. Decide which online-adaptive functions are the supported LKT 2.0 API.
   Document exported functions, return shapes, numerical bounds, native-path
   behavior, and stability expectations.
4. Review functions currently described as legacy. Remove confirmed obsolete
   paths or document their continued contract; do not add compatibility
   dispatch implicitly.
5. Update `Authors@R`, package URLs, bug-report URL, citation, license coverage,
   and compiled-code metadata as needed.

## Phase 4: vignettes and examples

1. Keep concise vignettes that teach supported package behavior, including a
   bounded online-adaptive example.
2. Audit every vignette for execution time, undeclared packages, private paths,
   external data, generated output, and source-worktree assumptions.
3. Ensure vignettes run from the installed package namespace. Development
   helpers that source `R/` files directly must not define release behavior.
4. Remove duplicate or historical monolithic material after confirming the
   split vignettes preserve the intended user documentation.
5. Ensure examples finish in seconds and write only to temporary directories.

## Phase 5: native-code portability

1. Run registration and symbol checks for `src/init.c` and
   `src/online_simple_adaptive.c`.
2. Compile with current Rtools on Windows and current CRAN toolchains on Linux
   and macOS through supported check services.
3. Run sanitizer, valgrind, and compiler-warning checks where available.
4. Verify protection, allocation, bounds, interrupt handling, and numerical
   behavior at the R/C boundary.
5. Confirm that package checks use no more than CRAN's allowed resources and
   that native optimization is deterministic within documented tolerances.

## Phase 6: release verification

1. Run package documentation generation and inspect the resulting diff.
2. Run `R CMD build` with the current R release and inspect the tarball manifest
   and size.
3. Run `R CMD check --as-cran LKT_2.0.0.tar.gz` in a clean library.
4. Repeat checks with only hard dependencies and with suggested dependencies
   available, ensuring conditional examples and tests behave correctly.
5. Run Windows and cross-platform CRAN check services on the exact tarball.
6. Inspect current CRAN checks and reverse dependencies; test affected reverse
   dependencies and communicate deliberate API changes before submission.
7. Resolve every error and warning. Explain only unavoidable notes in
   `cran-comments.md`, with evidence from the exact final tarball.
8. Tag and submit only after the repository is clean and the checked tarball is
   byte-for-byte the submission artifact.

## Completion evidence

- Reviewed tarball file manifest and compressed size.
- Clean local `R CMD check --as-cran` output for `LKT_2.0.0.tar.gz`.
- Clean supported remote platform checks.
- Test evidence for R and native online-adaptive paths.
- Reviewed documentation, dependency, namespace, and exported-API inventories.
- Final `cran-comments.md`, reverse-dependency results, and submission tarball.

