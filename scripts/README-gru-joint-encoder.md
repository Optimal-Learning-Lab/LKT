# One shared learned input representation

Compare the current zero-item, skill-filtered GRU with one additional shared
input encoder. Keep all four original source blocks at 16 values each. Join
them into 64 values, apply linear 64 -> 32, ReLU, then linear 32 -> 49. The same
49 values feed the retrieval cue, prediction head and recurrent input. The
separately encoded answer is appended only after prediction for the GRU update.
There is no raw-input bypass around the new shared encoder.

The 32-unit hidden layer is fixed. The 49-value output was selected analytically
to match the baseline parameter count, without training or AUC selection. The
shared encoder adds 3,697 entries while reducing downstream input columns by
3,840 entries. Total: 46,114 versus 46,257 for baseline, a difference of 0.31%.
The original encoders, zero-initialized trainable item embeddings, random skill
embeddings, three 32-unit GRU layers, 128-unit head, one-head skill-filtered
retrieval, three timing features and optimization settings remain fixed.

Input dropout occurs once after the shared encoder; its own hidden-layer dropout
is zero. Epoch selection retains the previous validation criterion. This is one
predeclared case versus baseline, not a search over network sizes or architectures.
Two conditions times three seeds times the development split and five outer
participant folds gives 36 fresh fits. The development split is excluded from
primary pooled cross-validation AUC. Eighteen new baseline controls must reproduce
the preceding benchmark, and every held-out history must replay from saved weights.

The public controls are joint_encoder_output_size (NULL disables),
joint_encoder_hidden_sizes, joint_encoder_activation and joint_encoder_dropout.
The shared encoder requires MLP feature encoding, direct prediction and a full
retrieval cue. Disabled settings allocate no parameters or random draws and
preserve historical GRU behavior. FGUI exposes all four controls.

The implementation remains in the single maintained LKT source. A frozen package
candidate is used for validation while the separate sequence-unit experiment is
under development; this comparison does not promote the central release or deploy
the other experiment. It runs only GRU conditions. Candidate source/archive and
manifest identities, the current active release identity and exact data/fold
hashes are recorded separately. A validation candidate is selected explicitly,
never by searching alternative installations.
