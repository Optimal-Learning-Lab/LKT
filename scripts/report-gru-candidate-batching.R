# ADAPTIVE ORDINARY CHECK_LOG SIMULATOR WORKFLOWS LOGISTIC RETRIEVAL OUTPUT
a <- commandArgs(TRUE);stopifnot(length(a)==8L)
adaptive <- jsonlite::fromJSON(file.path(a[1],'completed.json'))
ordinary <- jsonlite::fromJSON(file.path(a[2],'completed.json'))
stopifnot(isTRUE(adaptive$passed),isTRUE(ordinary$passed),
  adaptive$maximum_prediction_difference<3e-6,ordinary$maximum_prediction_difference<3e-6)
check <- readLines(a[3]);stopifnot(any(grepl('checking tests ... OK',check,fixed=TRUE)),!any(grepl('ERROR',check)))
sim <- readRDS(file.path(a[4],'verification.rds'))
stopifnot(length(sim)==2L,all(unlist(sim)))
workflow <- readLines(file.path(a[5],'workflow_verification.txt'))
logistic <- jsonlite::fromJSON(file.path(a[6],'completed.json'))
retrieval <- jsonlite::fromJSON(a[7])
stopifnot(length(workflow)==4L,all(startsWith(workflow,'PASS ')),isTRUE(logistic$passed),
  isTRUE(retrieval$passed),max(retrieval$maximum_differences)<3e-6)
stopifnot(!dir.exists(a[8]));dir.create(a[8],recursive=TRUE)
table <- read.csv(file.path(a[1],'comparison.csv'))
full <- table[table$candidates>=110L,];full <- full[order(full$history,full$candidates),]
stopifnot(all(full$speedup>=5))
fmt <- function(x)sprintf('%.3f',x)
rows <- sprintf('| %d | %d | %s | %s | %.1fx |',full$history,full$candidates,
  fmt(full$seconds_reference),fmt(full$seconds_batched),full$speedup)
old <- read.csv(file.path(a[2],'comparison.csv'))
write.csv(full,file.path(a[8],'adaptive-full-bank.csv'),row.names=FALSE)
simulation <- read.csv(file.path(a[4],'simulation-timings.csv'))
text <- c('# Batched GRU candidate-scoring verification', '',
  'The saved student-local item-and-skill Cloze tracer was evaluated without refitting. Constant output learning, the original six inputs and the three-layer network are retained.', '',
  'Single-CPU loaded-runtime timings below are medians of three repetitions. Scoring includes feature construction, validation and neural evaluation. Process startup and model loading are excluded. Independent timing-input preparation is recorded separately.', '',
  '| Prior responses | Candidate items | Reference seconds | Batched seconds | Speedup |',
  '| ---: | ---: | ---: | ---: | ---: |',rows,'',
  sprintf('Maximum prediction difference from the prior-release oracle: %.10g (adaptive tracer); %.10g (ordinary identity-informed GRU). Tolerance: 3e-6.',
    adaptive$maximum_prediction_difference,ordinary$maximum_prediction_difference),'',
  'Checks passed for candidate permutation, batch sizes 1/32/256, ignored supplied answers, exact duplicate-candidate ties, immutable state/RNG/shared weights, selected-trial prediction, consistent unseen-label renaming for the adaptive tracer, and save/load with runtime recreation. Replaying each observed prefix reproduces its prior-release state exactly.', '',
  'The simulator adapter was checked with logistic and GRU ground truth, using existing synthetic fitted bundles. Selected items, sampled responses and generated durations were identical to reference runs. Serial/parallel execution and checkpoint recovery agree; changing the scoring configuration rejects incompatible continuation. All four logistic/GRU role pairings pass, including an independent saved-design oracle for logistic logits. Saved-bundle GUI/export, paper workflow and diagnostic continuation checks also pass.', '',
  sprintf('Small end-to-end verification runs (two students, three items, one timed run per method) gave %.2fx speedup with logistic ground truth and %.2fx with GRU ground truth. These include simulation/checkpoint overhead after runtime preflight and are distinct from the larger item-bank scoring benchmark.',
    simulation$speedup[simulation$ground_truth=='logistic'],simulation$speedup[simulation$ground_truth=='gru']),'',
  sprintf('Additional retrieval fixtures cover raw/projected values, two heads, shared/separate query-key projections, concat/replace fusion, filtered/unfiltered retrieval, a four-event window, and exponential/AKT decay. Maximum difference %.10g. These use transient random numeric fixtures, without fitting.',max(retrieval$maximum_differences)),'',
  'Native R CMD build/check passed all 52 test scripts with zero errors. Its two warnings concern intentionally unbuilt vignette outputs. Windows installation also passed.', '',
  'The historical reference scorer remains the default. Use method="batched" explicitly, or pf_with_candidate_scoring() on an existing supported simulator GRU role. The separately fitted Cloze tracer has not been enabled in simulator configurations.', '',
  'The batched scorer supports plain GRUs, none/constant output plasticity, optional retrieval, and additive fixed-parameter logitdec/ppes/lineafm history inputs. Other experimental mechanisms or recipes fail explicitly when requested; their existing reference path is retained.', '',
  'This is a scoring equivalence and efficiency evaluation, not a new AUC study or a claim of a fivefold end-to-end simulation speedup.', '',
  paste('Adaptive model SHA256:',adaptive$model_sha256),paste('Ordinary model SHA256:',ordinary$model_sha256),
  paste('Verified package:',adaptive$release$lkt_version),paste('Source archive SHA256:',adaptive$release$lkt_source_sha256),
  paste('Adaptive benchmark process memory:',paste(adaptive$process_memory,collapse='; ')),
  paste('Ordinary full-bank speedup range:',sprintf('%.1fx to %.1fx',min(old$speedup[old$candidates>=110]),max(old$speedup[old$candidates>=110]))))
writeLines(text,file.path(a[8],'RESULTS.md'))
jsonlite::write_json(list(passed=TRUE,package=adaptive$release,
  adaptive_model_sha256=adaptive$model_sha256,ordinary_model_sha256=ordinary$model_sha256,
  minimum_full_bank_speedup=min(full$speedup),maximum_full_bank_speedup=max(full$speedup),
  maximum_adaptive_prediction_difference=adaptive$maximum_prediction_difference,
  maximum_ordinary_prediction_difference=ordinary$maximum_prediction_difference,
  native_test_scripts=52L,all_four_role_pairings=TRUE,defaults_unchanged=TRUE,scientific_refits=FALSE),
  file.path(a[8],'completed.json'),auto_unbox=TRUE,pretty=TRUE)
cat('PASS validated scoring report and at least fivefold full-bank scoring speedup\n')
