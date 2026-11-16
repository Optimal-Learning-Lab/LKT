# Report new timing-only fits alongside immutable, seed-matched saved controls.
single_additions_report <- function(output,p,action) {
 stopifnot(length(list.files(file.path(output,'models'),pattern='[.]rds$'))==45L)
 d <- readRDS(file.path(output,'data/prepared_data.rds'));records<-predictions<-replays<-list()
 auc <- function(y,q){rank<-unname(study_metrics(y,q)[['AUC']]);independent<-as.numeric(pROC::auc(pROC::roc(y,q,levels=c(0,1),direction='<',quiet=TRUE)));stopifnot(abs(rank-independent)<1e-12);rank}
 for(part in p$partitions){
  roles<-readRDS(file.path(output,'partitions',paste0(part,'.rds')))
  stopifnot(all(file.exists(file.path(output,'reports',paste0(part,'-',p$conditions,'-verification.txt')))))
  held<-rbind(read.csv(file.path(output,'reference',paste0(part,'-predictions.csv'))),do.call(rbind,lapply(p$conditions,function(cn)read.csv(file.path(output,'reports',paste0(part,'-',cn,'-predictions.csv'))))))
  for(condition in c('timing',p$conditions,'all'))for(seed in p$seeds){
   id<-paste(part,condition,paste0('seed',seed),sep='__');fresh<-condition %in% p$conditions
   model_path<-if(fresh)file.path(output,'models',paste0(id,'.rds')) else file.path(output,'reference/models',paste0(id,'.rds'))
   saved<-readRDS(model_path);f<-saved$fit
   s<-if(fresh)p$settings[[condition]] else p$reference_settings[[condition]];s$seed<-as.integer(seed)
   expected_count<-if(fresh)p$parameter_counts[[condition]] else p$reference_parameter_counts[[condition]]
   protocol_hash<-if(fresh)study_hash(file.path(output,'study.rds')) else p$reference_protocol_hashes[[condition]]
   stopifnot(identical(s,f$gru_settings),identical(saved$settings,s),identical(f$model$recipe$interc,FALSE),
    identical(f$fit_rows,which(roles==1)),identical(f$validation_rows,which(roles==2)),identical(f$evaluation_rows,which(roles==3)),
    f$parameter_count==expected_count,all(is.finite(unlist(f$model$weights))),
    identical(saved$fingerprint,digest::digest(list(study=protocol_hash,task=saved$task,settings=s),algo='sha256')))
   if(fresh)stopifnot(s$use_lkt_features,length(f$model$preprocessing$engineered$columns)==1L,
    identical(f$model$preprocessing$numeric_names,p$numeric_names[[condition]]),
    identical(f$model$recipe[names(p$recipes[[condition]])],p$recipes[[condition]]))
   phase_auc<-vapply(1:3,function(k)auc(d$CF..ansbin.[roles==k],f$prediction[roles==k]),numeric(1))
   h<-f$training_history;j<-match(f$selected_epoch,h$epoch)
   stopifnot(abs(phase_auc[1]-h$training_auc[j])<1e-12,abs(h$validation_loss[j]-min(h$validation_loss,na.rm=TRUE))<1e-12)
   q<-held[held$condition==condition & held$seed==seed,]
   q<-q[match(d$comparison_source_row[roles==3],q$source_row),]
   difference<-max(abs(q$prediction-f$prediction[roles==3]))
   stopifnot(nrow(q)==sum(roles==3),!anyNA(q),!anyDuplicated(q$source_row),
    all(q$source_row==d$comparison_source_row[roles==3]),all(q$participant==d$Anon.Student.Id[roles==3]),
    all(q$observed==d$CF..ansbin.[roles==3]),difference<3e-6)
   replays[[id]]<-data.frame(run_id=id,maximum_probability_difference=difference,fresh=fresh)
   records[[id]]<-data.frame(partition=part,condition=condition,seed=seed,training_AUC=phase_auc[1],validation_AUC=phase_auc[2],fold_test_AUC=phase_auc[3],
    parameters=f$parameter_count,selected_epoch=f$selected_epoch,epochs_run=f$trained_epochs,cap_hit=f$trained_epochs==s$epochs,elapsed_seconds=saved$elapsed,fresh=fresh)
  }
  predictions[[part]]<-held
 }
 r<-data.table::as.data.table(do.call(rbind,records));pred<-data.table::as.data.table(do.call(rbind,predictions))
 stopifnot(!anyDuplicated(pred[,.(condition,seed,source_row)]),all(pred[,.N,by=.(condition,seed)]$N==nrow(d)))
 pooled<-pred[,.(heldout_AUC=auc(observed,prediction)),by=.(condition,seed)]
 phases<-r[,.(training_AUC=mean(training_AUC),validation_AUC=mean(validation_AUC)),by=.(condition,seed)]
 by_seed<-data.table::as.data.table(merge(phases,pooled,by=c('condition','seed'),sort=FALSE))
 summary<-by_seed[,.(training_AUC=mean(training_AUC),validation_AUC=mean(validation_AUC),heldout_AUC=mean(heldout_AUC),heldout_seed_sd=sd(heldout_AUC)),by=condition]
 extras<-r[,.(parameters=unique(parameters),cap_hits=sum(cap_hit),mean_selected_epoch=mean(selected_epoch)),by=condition]
 summary<-data.table::as.data.table(merge(summary,extras,by='condition',sort=FALSE))[match(c('timing',p$conditions,'all'),condition)]
 summary$heldout_gain_vs_timing<-summary$heldout_AUC-summary[condition=='timing',heldout_AUC]
 summary$heldout_change_vs_all<-summary$heldout_AUC-summary[condition=='all',heldout_AUC]
 paired<-data.table::copy(by_seed)
 for(k in p$seeds){paired[seed==k,gain_vs_timing:=heldout_AUC-by_seed[condition=='timing' & seed==k,heldout_AUC]];paired[seed==k,change_vs_all:=heldout_AUC-by_seed[condition=='all' & seed==k,heldout_AUC]]}
 old<-read.csv(file.path(output,'reference/summary.csv'));old_seed<-read.csv(file.path(output,'reference/auc-by-seed.csv'))
 for(condition_name in c('timing','all')){
  x<-summary[condition==condition_name];y<-old[old$condition==condition_name,]
  for(n in c('training_AUC','validation_AUC','heldout_AUC'))stopifnot(abs(x[[n]]-y[[n]])<1e-12)
  for(seed_value in p$seeds){x<-by_seed[condition==condition_name & seed==seed_value];y<-old_seed[old_seed$condition==condition_name & old_seed$seed==seed_value,];for(n in c('training_AUC','validation_AUC','heldout_AUC'))stopifnot(abs(x[[n]]-y[[n]])<1e-12)}
 }
 result<-list(summary=summary,auc_by_seed=by_seed,paired_seed_changes=paired,fits=r,replay_comparison=do.call(rbind,replays))
 for(n in names(result)){
  path<-file.path(output,'reports',paste0(gsub('_','-',n),'.csv'))
  if(action=='report')study_write_csv(result[[n]],path) else stopifnot(isTRUE(all.equal(as.data.frame(data.table::fread(path)),as.data.frame(result[[n]]),tolerance=1e-12,check.attributes=FALSE)))
 }
 if(action=='report'){
  labels<-c(timing='Four data-based inputs',p$labels,all='All seven inputs')
  rows<-vapply(seq_len(nrow(summary)),function(i){x<-summary[i];sprintf('| %s | %.6f | %.6f | %.6f | %+.6f | %d |',labels[[x$condition]],x$training_AUC,x$validation_AUC,x$heldout_AUC,x$heldout_gain_vs_timing,x$parameters)},character(1))
  writeLines(c('# Cloze: single LKT additions to the four-data-input GRU','',
   'Forty-five fresh fits: three single additions, five exact existing participant folds and seeds 1-3. Four-input and all-seven controls reuse thirty saved, hash-matched fits.','',
   '| Numeric inputs | Training AUC | Validation AUC | Held-out AUC | Gain over four | Parameters |','|---|---:|---:|---:|---:|---:|',rows,'',
   'Every fresh model retains the previous-response gap, item age, mean prior spacing and spacing availability, and adds exactly one predictor: student-item logitdec, student logitdec, or student-item ppes. Each uses a single-feature LKT recipe with its original fixed nonlinear constants. No other LKT predictor is constructed or supplied. The numeric encoder is 5 -> 32 -> 32; item/skill embeddings and response history remain. No global intercept.','',
   'Training/validation AUCs average fifteen restored fits per condition. Held-out AUC pools five test folds within seed then averages three seed AUCs. Optimizer, neural widths, dropout, training budget and minimum-validation-loss checkpoint restoration match the references.','',
   'Rank-based and independent pROC calculations agree within 1e-12 on all 225 saved phase AUCs and fifteen canonical pooled AUCs. All 45 fresh checkpoints passed held-out saved-weight replay. The thirty saved controls reproduce their original phase and pooled reported metrics.','',
   'Each addition has 43,041 parameters: 32 more than the four-input model, 64 fewer than all seven, and identical parameter counts across the three additions. Per-seed contrasts, checkpoints, epochs and replay differences are preserved. These are exploratory matched-budget point estimates on previously inspected Cloze data; no held-out condition selection or active-release change.'),file.path(output,'reports/RESULTS.md'))
  verification<-list(new_fits=45L,saved_control_fits=30L,phase_AUCs_checked=225L,pooled_AUCs_checked=15L,independent_auc_tolerance=1e-12,
   maximum_replay_probability_difference=max(vapply(replays,function(x)x$maximum_probability_difference,numeric(1))),
   exactly_one_LKT_feature_per_new_model=TRUE,all_selected_epochs_minimize_validation_loss=TRUE,controls_match_prior_reports=TRUE,study_sha256=study_hash(file.path(output,'study.rds')))
  jsonlite::write_json(verification,file.path(output,'reports/verification.json'),auto_unbox=TRUE,pretty=TRUE)
  writeLines('PASS 45 new fit contracts and held-out replays, 30 frozen controls, 225 independent phase AUCs, fifteen independent pooled AUCs, single-feature recipes and minimum-loss checkpoints.',file.path(output,'reports/verification.txt'))
 }
 print(summary);print(paired)
 invisible(result)
}
