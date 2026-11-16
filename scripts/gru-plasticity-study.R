# Scientific reporting for the frozen three-condition output-plasticity study.
plastic_study_parameters <- function(fit) {
  s<-fit$gru_settings;w<-fit$model$weights
  eta<-kappa<-rho<-half<-NA_real_
  if(!s$plasticity%in%c('none','item_skill'))eta<-as.numeric(w[['plasticity.eta']])
  if(s$plasticity=='fatigue') {
    kappa<-as.numeric(w[['plasticity.kappa']]);rho<-exp(as.numeric(w[['plasticity.log_rho']]))
    half<-log(2)/rho
  }
  list(eta=eta,kappa=kappa,recovery_rate_per_minute=rho,recovery_half_life_minutes=half)
}

plastic_study_replay <- function(fit,data) {
  f<-function(n)getFromNamespace(n,'LKT');s<-fit$gru_settings
  cleanup<-f('lkt_gru_runtime')(s);on.exit(cleanup())
  ord<-f('lkt_gru_order')(data,s);data<-data[ord,,drop=FALSE]
  encoded<-f('lkt_gru_encode')(data,fit$model$preprocessing,fit$model$recipe,s)
  net<-LKTGRURuntime(fit)$network;net$eval();pred<-rep(NA_real_,nrow(data));stats<-c(0,0,0)
  groups<-split(seq_len(nrow(data)),f('lkt_gru_keys')(data,s$history_group_cols))
  torch::with_no_grad({
    for(start in seq.int(1,length(groups),by=s$batch_size)) {
      g<-groups[seq.int(start,min(length(groups),start+s$batch_size-1L))]
      longest<-max(lengths(g));chunk<-if(is.null(s$sequence_length))longest else s$sequence_length
      hidden<-context<-NULL
      for(offset in seq.int(1,longest,by=chunk)) {
        active<-which(lengths(g)>=offset)
        hidden<-f('lkt_sequence_state_map')(hidden,s,'subset',active)
        context<-f('lkt_gru_context_subset')(context,active);g<-g[active]
        pos<-lapply(g,function(idx)seq.int(offset,min(length(idx),offset+chunk-1L)))
        b<-f('lkt_gru_batch')(encoded,data,g,pos,s)
        q<-f('lkt_gru_forward')(net,b$numeric,b$categorical,b$answers,hidden,context,b$retrieval_ids,s,b$decay_gaps,!is.na(b$rows))
        hidden<-q$hidden;context<-q$context
        p<-torch::as_array(torch::torch_sigmoid(q$logits));keep<-!is.na(b$rows);pred[b$rows[keep]]<-p[keep]
        if(s$plasticity!='none')stats<-stats+colSums(torch::as_array(q$plastic_stats))
      }
    }
  })
  list(prediction=pmin(pmax(pred[order(ord)],s$clip_epsilon),1-s$clip_epsilon),
    negative_rate_uses=stats[1],active_connection_updates=stats[2],negative_post_update_rates=stats[3],
    negative_rate_use_fraction=if(stats[2]>0)stats[1]/stats[2] else NA_real_)
}

plastic_study_fit <- function(task,s,output,p,d) {
  recipe<-p$baseline;s$seed<-as.integer(task$seed);s<-do.call(GRUModel,s)$settings
  path<-file.path(output,'models',paste0(task$run_id,'.rds'))
  fingerprint<-digest::digest(list(study=study_hash(file.path(output,'study.rds')),task=task,settings=s),algo='sha256')
  if(!file.exists(path)) {
    cat('START',task$run_id,'\n');flush.console()
    elapsed<-system.time(fit<-LKT(d,usefolds=1,components=recipe$components,features=recipe$features,
      fixedpars=recipe$fixedpars,interc=recipe$interc,interacts=recipe$interacts,connectors=recipe$connectors,
      maketimes=recipe$maketimes,model=do.call(GRUModel,s),verbose=TRUE))[['elapsed']]
    fit$newdata<-NULL;fit$nullmodel<-NULL
    study_write_rds(list(fingerprint=fingerprint,task=task,settings=s,fit=fit,elapsed=elapsed),path)
  }
  saved<-readRDS(path);fit<-saved$fit
  stopifnot(identical(saved$fingerprint,fingerprint),identical(saved$settings,s),identical(fit$gru_settings,s),
    identical(fit$fit_rows,which(d$fold==1L)),identical(fit$validation_rows,which(d$fold==2L)),
    identical(fit$evaluation_rows,which(d$fold==3L)),fit$parameter_count==p$parameter_counts[[task$candidate]],
    all(is.finite(unlist(fit$model$weights))))
  held<-d[d$fold==3L,,drop=FALSE];replay<-plastic_study_replay(fit,held);q<-replay$prediction
  difference<-max(abs(q-fit$prediction[d$fold==3L]));stopifnot(difference<3e-6)
  predictions<-data.frame(partition=task$partition,condition=task$candidate,seed=task$seed,
    source_row=held$comparison_source_row,participant=held$Anon.Student.Id,observed=held$CF..ansbin.,prediction=q)
  study_write_csv(predictions,file.path(output,'reports',paste0(task$run_id,'-predictions.csv')))
  replay$prediction<-NULL
  study_write_csv(cbind(task,as.data.frame(plastic_study_parameters(fit)),as.data.frame(replay),
    maximum_replay_probability_difference=difference),file.path(output,'reports',paste0(task$run_id,'-parameters.csv')))
  study_write_csv(fit$training_history,file.path(output,'training',paste0(task$run_id,'.csv')))
  writeLines('PASS exact split/settings, finite saved weights and held-out saved-model replay.',file.path(output,'reports',paste0(task$run_id,'-verification.txt')))
  cat('DONE',task$run_id,'\n');flush.console()
}

plastic_study_report <- function(output,p,action) {
  stopifnot(length(list.files(file.path(output,'models'),pattern='[.]rds$'))==45L)
  d<-readRDS(file.path(output,'data/prepared_data.rds'));records<-predictions<-list()
  for(i in seq_len(nrow(p$tasks))) {
    task<-p$tasks[i,,drop=FALSE];id<-task$run_id
    saved<-readRDS(file.path(output,'models',paste0(id,'.rds')));fit<-saved$fit
    s<-p$settings[[task$candidate]];s$seed<-as.integer(task$seed)
    roles<-readRDS(file.path(output,'partitions',paste0(task$partition,'.rds')))
    stopifnot(identical(saved$settings,s),identical(fit$gru_settings,s),identical(saved$task,task),
      identical(saved$fingerprint,digest::digest(list(study=study_hash(file.path(output,'study.rds')),task=task,settings=s),algo='sha256')),
      identical(fit$fit_rows,which(roles==1L)),identical(fit$validation_rows,which(roles==2L)),
      identical(fit$evaluation_rows,which(roles==3L)),fit$parameter_count==p$parameter_counts[[task$candidate]],
      all(is.finite(unlist(fit$model$weights))),fit$selected_epoch==fit$training_history$epoch[which.min(fit$training_history$validation_loss)],
      file.exists(file.path(output,'reports',paste0(id,'-verification.txt'))))
    for(n in c('components','features','fixedpars','interc','maketimes','connectors'))stopifnot(identical(fit$model$recipe[[n]],p$baseline[[n]]))
    pars<-read.csv(file.path(output,'reports',paste0(id,'-parameters.csv')))
    for(n in names(plastic_study_parameters(fit)))stopifnot(isTRUE(all.equal(as.numeric(pars[[n]]),plastic_study_parameters(fit)[[n]],tolerance=1e-12)))
    q<-read.csv(file.path(output,'reports',paste0(id,'-predictions.csv')))
    stopifnot(!anyNA(q),!anyDuplicated(q$source_row),identical(q$source_row,d$comparison_source_row[roles==3L]),
      all(q$observed==d$CF..ansbin.[roles==3L]),all(q$participant==d$Anon.Student.Id[roles==3L]),
      max(abs(q$prediction-fit$prediction[roles==3L]))<3e-6)
    metrics<-unlist(lapply(1:3,function(k) {
      y<-d$CF..ansbin.[roles==k];pred<-if(k==3L)q$prediction else fit$prediction[roles==k]
      m<-study_metrics(y,pred)[c('AUC','log_loss')]
      stopifnot(abs(m[1]-as.numeric(pROC::auc(y,pred,quiet=TRUE,direction='<')))<1e-12,
        abs(m[2]+mean(stats::dbinom(y,1,pred,log=TRUE)))<1e-12);m
    }),use.names=FALSE)
    names(metrics)<-c('training_AUC','training_log_loss','validation_AUC','validation_log_loss','test_AUC','test_log_loss')
    records[[id]]<-cbind(pars,as.data.frame(as.list(metrics)),parameter_count=fit$parameter_count,
      selected_epoch=fit$selected_epoch,epochs_run=fit$trained_epochs,elapsed_seconds=saved$elapsed)
    predictions[[id]]<-q
  }
  fits<-data.table::as.data.table(do.call(rbind,records));pred<-data.table::as.data.table(do.call(rbind,predictions))
  stopifnot(!anyDuplicated(pred[,.(condition,seed,source_row)]),all(pred[,.N,by=.(condition,seed)]$N==nrow(d)))
  pooled<-pred[,as.list(study_metrics(observed,prediction)[c('AUC','log_loss')]),by=.(condition,seed)]
  summary<-pooled[,.(heldout_AUC=mean(AUC),heldout_log_loss=mean(log_loss)),by=condition]
  pairs<-list()
  for(pair in list(c('stock','constant'),c('stock','fatigue'),c('constant','fatigue'))) {
    a<-fits[candidate==pair[1]];b<-fits[candidate==pair[2]]
    q<-merge(as.data.frame(a),as.data.frame(b),by=c('partition','seed'),suffixes=c('_baseline','_extension'))
    pairs[[length(pairs)+1L]]<-data.frame(comparison=paste(pair,collapse='_to_'),partition=q$partition,seed=q$seed,
      training_log_loss_improvement=q$training_log_loss_baseline-q$training_log_loss_extension,
      test_AUC_gain=q$test_AUC_extension-q$test_AUC_baseline,
      test_log_loss_improvement=q$test_log_loss_baseline-q$test_log_loss_extension)
  }
  tables<-list(fits=fits,pooled_by_seed=pooled,summary=summary,paired_contrasts=do.call(rbind,pairs))
  for(n in names(tables)) {
    path<-file.path(output,'reports',paste0(gsub('_','-',n),'.csv'))
    if(action=='report')study_write_csv(tables[[n]],path) else
      stopifnot(isTRUE(all.equal(as.data.frame(data.table::fread(path)),as.data.frame(tables[[n]]),tolerance=1e-12,check.attributes=FALSE)))
  }
  if(action=='report')jsonlite::write_json(list(complete=TRUE,fits=45L,phase_metric_pairs=135L,
    maximum_replay_probability_difference=max(fits$maximum_replay_probability_difference),
    protocol_sha256=study_hash(file.path(output,'study.rds'))),file.path(output,'reports/verification.json'),auto_unbox=TRUE,pretty=TRUE)
  print(summary);invisible(tables)
}
