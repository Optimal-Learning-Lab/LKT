# Cloze: three single LKT additions to four data inputs

`compare-gru-single-additions.R` adds exactly one original LKT predictor to the
four-input GRU: student-item logitdec, student logitdec, or student-item ppes.
Each condition uses a single-feature recipe and the corresponding unchanged
nonlinear constants extracted from the frozen full recipe. The other two LKT
features are neither constructed nor passed to the network. The four data
inputs remain previous-response gap, item age, mean prior spacing and spacing
availability. Each model has five numeric coordinates and 43,041 parameters.

Three conditions times five existing participant folds times seeds 1-3 give
45 fresh fits. Thirty saved timing-only and all-seven controls are reused from
the verified preceding studies. Neural widths, optimizer, dropout, stopping
budget, checkpoint restoration, embeddings, response history and no-intercept
specification are held fixed. No package implementation or central release is
changed. There is no selection based on held-out results.

Driver: ROOT OUTPUT ACTION PART CANDIDATE_JSON CANDIDATE_LIBRARY [REFERENCE].
Actions: plan, verify, smoke, fit, report, check. Fit PART is cvN:CONDITION.
The preflight verifies all 15 fold/condition schemas, one LKT column and exact
numeric equality with the original full recipe's matching coordinates. Three
smoke fits check serialization and held-out saved-weight replay.

`stage-gru-single-additions-code.R` takes ROOT ARTIFACT LOCAL_PLAN
LOCAL_REFERENCE_STUDY_ROOT REMOTE_REFERENCE_STUDY_ROOT LOCAL_RUNTIME_STUDY_ROOT
REMOTE_RUNTIME_STUDY_ROOT. It stages scripts and hashes only. No learner rows,
folds, fitted models, predictions, source archives or package samples are
uploaded. Scientific inputs and the verified candidate runtime are bound
read-only from their explicitly declared existing locations. SHA256 verification
precedes fitting. Outputs belong in the sibling LKT-outputs directory.

The fit array has fifteen tasks, three one-thread workers each, with unique
explicit parallel socket ports. It fits each condition and seed afresh.
The reporter checks all 75 checkpoint contracts, original nonlinear recipes,
minimum validation-loss epochs, finite weights, exact saved control metrics,
and replay correspondence. Rank and pROC AUC calculations must agree within
1e-12 for 225 phase AUCs and fifteen canonical pooled AUCs. Train/validation
AUCs average fifteen fits; held-out AUC pools five test folds within seed and
then averages three seeds. `verify-gru-single-additions-results.R` repeats the
numeric/contract checks locally against frozen scripts and the local preflight.
