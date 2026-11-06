LKT [![](https://cranlogs.r-pkg.org/badges/LKT)](https://cran.r-project.org/package=LKT)
========================================================================================

For this research workspace, the maintained source is `C:/dev/LKT` and the
current package is selected by the single central release definition.

See the [workspace layout guide](docs/workspace-layout.md) for the code, bulk-data,
and experiment-output homes, the frozen GRU checkpoint/results, and package builds.

Load the verified local installation through the research bridge:

```r
root <- "C:/dev/LKT"
source(file.path(root, "scripts/load-current-lkt.R"))
```

The working candidate adds [selectable GRU and Mamba sequence units](docs/sequence-units.md)
and a [fixed-setting architecture experiment](scripts/README-sequence-comparison.md).
These candidate capabilities have not been promoted to the central release.

Obsolete installed packages and copied sources have been physically retired to
compressed historical storage outside the active projects. Older workflows must
be explicitly migrated; do not restore an old package or search alternate
libraries to get them running. See [AGENTS.md](AGENTS.md) and the retirement
manifest in `C:/dev/LKT-outputs/retirement-20261103`. `.codex-r-lib` now supplies dependencies
only. The current project-root source archives in LKT and PolicyFactory2 have
identical hashes and represent the same centrally selected build.

The [default GRU design](docs/figures/gru-base-model/README.md) uses separate
16-value feature encoders, filtered one-head retrieval, zero-initialized trainable
item embeddings, three 32-unit GRU layers and a 128-unit prediction head. These
are ordinary `GRUModel()` argument defaults, also used by the GRU settings editor;
explicit settings select other variants. `LKT()` still defaults to LibLinear.
The design figure, editable specification and verified checkpoint reference are
kept together.
  
  Examples with outputs are available as a vignette.
==============
  
  Please see the manual and vignettes for more information.

<!-- badges: start -->
  [![R-CMD-check](https://github.com/Optimal-Learning-Lab/LKT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Optimal-Learning-Lab/LKT/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
