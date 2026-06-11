# LKT Style and Efficiency Review Goal Prompt

Inspect `C:\dev\LKT` as an important published R package, with behavior preservation as the controlling invariant. Improve style, elegance, readability, and runtime efficiency only where the current code makes the flow harder to compare, reason about, maintain, or run quickly. Efficiency means actual execution speed, memory use, allocation behavior, algorithmic complexity, and avoiding repeated expensive work in hot paths. Prefer compact, information-dense R code over airy formatting: keep related logic close together, avoid excessive vertical whitespace, and use line breaks where they clarify structure rather than creating space.

Work from the actual package surfaces: `DESCRIPTION`, `NAMESPACE`, `R/`, `src/`, `scripts/`, and `vignettes/`. Preserve public APIs, exported names, documented examples, output object shapes, error semantics, and numerical behavior unless a deliberate behavior change is explicitly approved. Treat legacy vignette-facing helpers and published-paper workflows as regression-sensitive.

Refactor by small coherent moves. Before editing, identify the invariant being protected and the exact behavior or performance property being improved. Prefer local cleanup, clearer helper extraction, duplicated-condition removal, safer argument validation, and measurable speed or memory improvements. Do not perform broad rewrites, style-only churn across unrelated files, or changes that merely make code look modern while obscuring the model flow.

Performance priorities:

- Look for hot paths in feature computation, model matrix construction, prediction, search loops, online calibration, fast online simple-adaptive evaluation, and native C bridges.
- Prefer reducing repeated scans, repeated formula parsing, repeated column lookup, avoidable copies, growing objects in loops, and unnecessary dense conversions of sparse data.
- Preserve sparse matrix workflows and compiled-code paths where they are part of the package's speed story.
- Use vectorization, data.table idioms, preallocation, caching, or C/native routines when they make the code demonstrably faster without changing results.
- Be skeptical of changes that make code shorter but slower, allocate more memory, or hide expensive work behind helper calls.
- For any nontrivial speed change, compare before/after timing on a representative package example or vignette-sized workflow and report the benchmark method.

Style preferences:

- Keep code compact enough that several related blocks can be viewed at once.
- Preserve readable alignment for long function signatures and list construction.
- Avoid large blank gaps, decorative comments, and over-expanded one-line logic.
- Use clear helper names that encode domain behavior, not generic mechanics.
- Keep comments sparse and useful, especially around model invariants, numerical assumptions, and compatibility constraints.
- Use base R, data.table, SparseM, Matrix, LiblineaR, and existing package idioms before adding dependencies.

Verification expectations:

- For small local style changes, run only the smallest relevant smoke check or syntax/load check when useful.
- For changes touching exported functions, model fitting, prediction, feature computation, native C integration, or vignette behavior, run package checks appropriate to the change.
- Use the vignettes as compatibility evidence for significant changes, especially workflows covering `LKT()`, `buildLKTModel()`, LASSO helpers, model adapters, online calibration, recency decay, and fast online simple-adaptive code.
- If full vignette or package checks are too slow for the moment, state exactly what was not run and what remains at risk. Some of the checks are very slow, and as soon as you discover that, abort.

Current orientation from inspection:

- `R/LKTfunctions.R`, `R/lkt-feature-computation.R`, and `R/lkt-search.R` contain much of the older dense public workflow code and should be treated carefully.
- Newer model adapter, online calibration, and native simple-adaptive code already show a more modular pattern; use that as a local style reference without forcing unrelated legacy code into a new architecture.
- The repository currently has many modified and untracked files, including generated check output, vignettes, `scripts/`, `src/`, and new R modules. Do not overwrite, revert, or normalize unrelated existing changes.
