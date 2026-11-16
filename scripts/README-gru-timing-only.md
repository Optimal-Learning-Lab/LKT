# Cloze four-timing-input follow-up

`compare-gru-timing-only.R` fits the existing default architecture with only:
previous-response gap, item age, mean prior spacing, and spacing availability.
`use_lkt_features=FALSE` disables engineered-feature construction;
`numeric_feature_cols` explicitly selects these four coordinates. The serialized
engineered schema must be NULL. All other neural and training settings, the
no-intercept specification, participant folds and seeds match the completed
numeric-ablation study. Fifteen fresh fits are compared with thirty frozen
all-seven/no-numeric controls. The fixed recipe is retained as provenance but
its LKT feature values are never constructed or supplied to this network.

This follows the frozen candidate package and data contract explicitly. No
package implementation or central release is changed. Read-only reference
bindings provide the already-verified package, data, folds and controls on Big
Blue. Code-only staging uploads scripts and hashes; it uploads no learner data,
partitions, fitted models, predictions or packaged sample data.

Driver arguments: ROOT OUTPUT ACTION PART CANDIDATE_JSON CANDIDATE_LIBRARY [REFERENCE].
Actions are plan, verify, smoke, fit, report and check. REFERENCE is required by
plan; PART is cv1-cv5 for fit. Source hashes and input contracts are frozen in
study.rds. Output belongs in the sibling LKT-outputs directory.

The setup checks all remote input hashes, package identity and source archive,
five fold-specific input/parameter schemas and a one-epoch replay smoke fit.
The model has 43,009 parameters (96 fewer than all seven). Widths remain fixed.
Three workers per fold each use one Torch thread and explicit unique socket ports.

The reporter recomputes saved-fit phase AUCs and canonical replay pooled AUCs
using both rank statistics and independent pROC, requiring agreement within
1e-12 on all 135 phase and nine pooled AUCs. It checks masks, fingerprint/weights,
minimum validation-loss selected epochs, no LKT schema, all fifteen new held-out
replays and exact reproduction of the two controls' original reported metrics.
Report/check actions and a local rerun provide independent host verification.
No new fit or control is selected based on held-out outcomes.
