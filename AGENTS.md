# One maintained LKT implementation

The only maintained LKT source tree is `C:/dev/LKT`. GRUs, inference modes,
logistic models, GLMMs, and online adaptive/Bayesian models belong in this package,
not in copied package trees or loose versions of `LKTfunctions.R`.

The single current release definition is
`C:/dev/PolicyFactory2/workflows/simulation/shared/lkt-release.env`.
It selects the verified build, checksums, and sole active local installation.
Read it through the shared R/PowerShell release loaders; LKT research scripts
use `scripts/load-current-lkt.R`. Do not embed release versions, hashes, or
versioned library paths in individual scripts. See PolicyFactory2's
`docs/current-lkt.md` for the update procedure.
The identically hashed source archives at this repository root and the
PolicyFactory2 root are build/deployment artifacts of the same source.
`C:/dev/LKT/.codex-r-lib` contains dependencies, not an LKT installation.

Obsolete installations, old source checkouts, partial source snapshots and
duplicate experimental build archives were moved to compressed historical storage
under `C:/dev/.retired-code/LKT/2026-11-03`. Do not restore them to active paths to
make an old workflow run. Old workflows must fail until explicitly updated to the
current build and checked against their model/data/seed contracts. Do not silently
search alternate libraries. Select and verify the intended installation explicitly.

When updating LKT, build from this source tree, verify supported capabilities,
update only the central release definition after validation,
and retire the superseded installation rather than keeping runnable variants.
Temporary build/test sources must not become alternate maintained copies.
Do not modify historical fitted models, results, or original submission records.
Do not use ad hoc fallbacks.

Keep maintained source, tests, reusable research scripts, documentation and curated
figures here. Bulk inputs belong in `C:/dev/LKT-data`; experiment folders, prepared
experiment data, fitted checkpoints, reports, logs and one-off working files belong
in `C:/dev/LKT-outputs`. Keep the two intentional package samples in `data/`.
Use the supplied source root to derive these sibling paths, or pass an explicit
input/output argument. Never search old locations or add junctions/symlinks.
Check jobs and completion watchers before moving their files; wait for them without
stopping/restarting them. See [docs/workspace-layout.md](docs/workspace-layout.md).

When automatic tool-policy review blocks a command, append its exact text, tool,
working directory, returned reason, execution status and resolution to policy.md.
Do not invent an explanation when no specific rule is returned. Preserve earlier
entries and do not put credentials or secrets in the log.
Select tools to complete the user's authorized outcome. If an invocation fails,
evaluate suitable alternative tools and try a properly scoped authorized action;
do not treat one tool's unexplained rejection as proof that every tool is blocked.
Record the alternative attempt and its verified outcome in policy.md.

For package verification in this research workspace, create a fresh generated
build input with `scripts/stage-package-verification.R ROOT OUTPUT`, then run
ordinary R CMD build/check on OUTPUT/LKT. On Windows, R otherwise copies ignored
research data and live lock files before applying .Rbuildignore. The helper
prunes those ignored trees before copying and verifies every included file hash.
This input is an immutable build artifact, not another maintained source tree;
edit C:/dev/LKT and regenerate it when changes are required.
