# Existing base GRU on KTST-cleaned EdNet

This is one fixed training/validation split and three fresh seeds, selected by
the user for a first test. Only the existing base model is fitted. It is not
five-fold cross-validation or a hyperparameter search.

The publisher's EdNet-KT1 and content archives are retained in
`C:/dev/LKT-data/ednet-ktst-base-20261105/downloads`. KTST source commit
`e6331a6f45e597f286d1272fbb96e7bb67bcaa52` is retained with its license under
the artifact's `reference` directory. The preprocessing adapter calls its
original EdNet cleaning, minimum-length parser and participant-split functions
without editing those functions. All exported question, skill, response and
timestamp sequences are checked against the original parser.

The exact initial sample is 5,000 students, shuffled with Python seed 2 over the
authors' integer range. After their checks, 4,687 students and 596,600 question
responses remain. There are 11,901 observed items and 1,743 observed skill-tag
combinations. The authors' fixed test has 937 students and 133,974 responses.
Validation fold zero supplies 750 students and 79,103 responses; the remaining
3,000 students and 383,523 responses fit the weights. No rare-item minimum,
additional deduplication or total-history cap is introduced.

The neural architecture remains three 32-unit GRU layers, a 128-unit prediction
head, four original 16-value input blocks, separate response encoding, one
32-value retrieval head, exact item-or-skill filtering within 100 scored events,
and zero-initialized trainable item embeddings. Skill combinations remain one
category for the base's single skill input and exact-match filter. This differs
from KTST's aggregation of separate atomic skill embeddings. The documented
zero-encoding option handles held-out-only identities without dropping rows.
Vocabularies and scaling use training rows only.

Question-presentation timestamps are converted from milliseconds to seconds.
Item age and prior-only mean spacing are computed on the cleaned chronological
histories, preserving undefined spacing and its availability flag. LKT's
additional practice clock uses cumulative durations from completed earlier
responses; the current response duration is excluded and is not a raw predictor.
The base's three engineered history transforms and six fixed nonlinear constants
are carried over unchanged; no EdNet-based retuning is performed.

The base's 100-step backpropagation chunks, 100-event retrieval bank, optimizer,
learning rate, epoch budget and validation stopping criterion remain fixed.
These differ from KTST training and history limits. Primary reported test AUC
excludes each student's first response, matching KTST's target exclusion; that
response still updates subsequent memory. Full-response AUC is saved separately.
Three individual AUCs are averaged, without probability ensembling.

The first setup submission stopped before training because the adapter omitted
LKT's required relative practice-time column. Its original plan, bundle and
submission are preserved. `attempt-2` contains the corrected adapter inputs and
fresh plan; no first-attempt fitted weights or results are reused.

All neural computation uses `scripts/load-current-lkt.R` and the central release
definition. The active release is not rebuilt or promoted for this experiment.
No package implementation is copied into this research driver. Saved fits,
participant masks, held-out replay, restored training AUC and protocol hashes
must pass verification before a completed result is reported.

The three fits completed successfully. The original reporting job exposed an
integer overflow in the research AUC helper when multiplying class counts.
The maintained helper now uses double counts; known large-sample cases and an
independent pROC comparison pass. `scripts/audit-gru-ednet-ktst.R` checks the
original frozen training-script hashes and permits exactly this arithmetic
correction for analysis, leaving protocols and fitted models unchanged. The
original failed report is retained. The participant bootstrap uses canonical
saved scores after independent replay agreement checks, avoiding tiny AUC
differences caused by float32 batching changing a few exact score ties.
