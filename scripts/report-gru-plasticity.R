# AUDIT_DIRECTORY REPORT_DIRECTORY
a<-commandArgs(TRUE);stopifnot(length(a)==2L)
audit<-a[1];out<-a[2];stopifnot(dir.exists(out))
verification<-jsonlite::fromJSON(file.path(audit,'verification.json'));stopifnot(verification$complete)
fits<-read.csv(file.path(audit,'fits.csv'));pooled<-read.csv(file.path(audit,'pooled.csv'))
summary<-aggregate(cbind(AUC,log_loss)~condition,pooled,mean)
summary<-summary[match(c('stock','constant','fatigue'),summary$condition),]
training<-aggregate(cbind(training_AUC,training_log_loss,minimum_training_log_loss)~candidate,fits,mean)
runtime<-aggregate(elapsed_seconds~candidate,fits,median)
negative<-aggregate(cbind(negative_rate_uses,active_connection_updates)~candidate,fits,sum)
negative$fraction<-with(negative,ifelse(active_connection_updates>0,negative_rate_uses/active_connection_updates,NA_real_))
contrasts<-list()
for(pair in list(c('stock','constant'),c('stock','fatigue'),c('constant','fatigue'))) {
  x<-merge(subset(pooled,condition==pair[1]),subset(pooled,condition==pair[2]),by='seed',suffixes=c('_baseline','_extension'))
  contrasts[[length(contrasts)+1L]]<-data.frame(comparison=paste(pair,collapse='_to_'),seed=x$seed,
    AUC_gain=x$AUC_extension-x$AUC_baseline,log_loss_improvement=x$log_loss_baseline-x$log_loss_extension)
}
contrasts<-do.call(rbind,contrasts)
write.csv(contrasts,file.path(out,'paired-seed-contrasts.csv'),row.names=FALSE)
fatigue<-subset(fits,candidate=='fatigue')
fatigue<-fatigue[order(fatigue$partition,fatigue$seed),]
constants<-subset(fits,candidate=='constant')
format_table<-function(x) {
  old<-options(width=180);on.exit(options(old))
  capture.output(print(x,row.names=FALSE,digits=9))
}
writeLines(c('CLOZE: COMPILED OUTPUT-CONNECTION FATIGUE AND RECOVERY','',
 '45 fresh fits: stock, constant-rate adaptive connections, fatigue/recovery adaptive connections; five whole-participant folds and seeds 1-3. PPES and every shared training control retained.','',
 'HELD-OUT RESULTS',
 'AUC and log loss pool all five held-out folds within each seed, then average the three seed metrics.',
 format_table(summary),'',
 'PAIRED DIFFERENCES (positive favors the extension)',format_table(contrasts),'',
 'TRAINING RESULTS',
 'training_* evaluates the validation-selected checkpoint with dropout disabled. minimum_training_log_loss is the smallest training evaluation loss observed across all epochs, independently of selection.',
 format_table(training),'',
 'MEDIAN FIT TIME (seconds)',format_table(runtime),'',
 'FATIGUE PARAMETERS AND HELD-OUT METRICS, EVERY FOLD AND SEED',
 'eta is the baseline adaptive learning rate; kappa is linear fatigue strength; rho is recovery per minute. Only rho has a time half-life: log(2)/rho minutes.',
 format_table(fatigue[,c('partition','seed','eta','kappa','recovery_rate_per_minute','recovery_half_life_minutes','test_AUC','test_log_loss')]),'',
 'CONSTANT-RATE PARAMETERS',format_table(constants[,c('partition','seed','eta','test_AUC','test_log_loss')]),'',
 'NEGATIVE LEARNING RATES USED DURING HELD-OUT REPLAY',
 'Counts refer to recovered rates on active (nonzero-feature) connections, before the response update.',format_table(negative),'',
 'VERIFICATION AND INTERPRETATION',
 '- Both extensions begin at eta=0 and exactly reproduce stock logits for all 15 fold/seed initializations. Shared network weights and schemas are identical at initialization. Eta can learn away from zero.',
 '- The compiled scan was checked against independent equations and gradients, including finite differences, dropout equivalence, padding, chunking, causal feedback, saved-state continuation and read-only candidate scoring.',
 '- All 35 package test files passed. R CMD check: zero errors, one existing dependency-version warning; vignettes/manual were excluded.',
 '- The independent audit verifies 135 phase metric pairs, 9 pooled metric pairs, participant partitions, parameter extraction and saved-model held-out predictions.',
 sprintf('- Maximum probability difference between the 15 fresh stock fits and their prior stock controls: %.12g.',verification$maximum_previous_stock_probability_difference),
 '- Fatigue versus constant-rate adaptation isolates the added mechanism. An adaptive model beating stock alone would not establish that fatigue helps.',
 '- These are fitted estimates, not guaranteed global optima. Starting from the stock function does not guarantee optimization or validation selection yields lower training loss.',
 sprintf('- Zero-rate fits: constant %d/15; fatigue %d/15. At eta=0, fatigue and recovery parameters do not affect predictions and are not likelihood-identified.',sum(constants$eta==0),sum(fatigue$eta==0)),
 '- This changes student-specific output connections, not the recurrent GRU matrices. Shared network parameters are fixed during testing; only student-specific adjustments/rates update after observed answers.',
 '- The signed linear fatigue equation is preserved without clipping. No new weight decay, consolidation, STP subsystem or per-answer optimizer is added.',
 '- No held-out parameter tuning, central release promotion, or alteration of previous experiment outputs occurred.',
 '- All fold/seed training, validation and test metrics, parameters, times and counts are in the independent audit fits.csv.'),file.path(out,'RESULTS.txt'))
print(summary);print(training);print(runtime);print(negative)
print(aggregate(cbind(AUC_gain,log_loss_improvement)~comparison,contrasts,mean))
