# The study orchestrates the maintained LKT package; no network implementation is
# copied here. Checkpoints, data partitions and release identities are immutable.
study_hash <- function(path)digest::digest(file=path,algo='sha256')
study_write_rds <- function(x,path) {
  if(file.exists(path))stopifnot(identical(readRDS(path),x)) else {
    tmp <- paste0(path,'.tmp');saveRDS(x,tmp);stopifnot(file.rename(tmp,path))
  }
}
study_write_csv <- function(x,path) {
  tmp <- paste0(path,'.tmp');write.csv(x,tmp,row.names=FALSE)
  if(file.exists(path)) {
    stopifnot(identical(readLines(path),readLines(tmp)));unlink(tmp)
  } else stopifnot(file.rename(tmp,path))
}
study_metrics <- function(y,p) {
  stopifnot(length(y)==length(p),length(y)>0L,all(y %in% 0:1),all(is.finite(p)),all(p>0 & p<1))
  # Counts must be doubles before multiplication: a large, valid dataset can
  # exceed R's signed-integer product limit even when its row count is modest.
  n1 <- as.double(sum(y==1));n0 <- as.double(sum(y==0))
  c(log_loss=-mean(ifelse(y==1,log(p),log1p(-p))),
    AUC=(sum(rank(p,ties.method='average')[y==1])-n1*(n1+1)/2)/(n1*n0),
    Brier=mean((p-y)^2),RMSE=sqrt(mean((p-y)^2)))
}
study_require_release <- function(release_file,library) {
  release <- jsonlite::fromJSON(release_file)
  stopifnot(dir.exists(library))
  .libPaths(c(library,.libPaths()))
  suppressPackageStartupMessages(library(LKT,lib.loc=library,character.only=FALSE))
  stopifnot(as.character(utils::packageVersion('LKT'))==release$version,
    normalizePath(find.package('LKT'),winslash='/')==paste0(normalizePath(library,winslash='/'),'/LKT'),
    study_hash(system.file('research-source-manifest.json',package='LKT'))==release$manifest_sha256,
    as.character(utils::packageVersion('torch'))=='0.17.0',torch::torch_is_installed())
  release
}

# Build all six partitions before fitting. Development preserves the original split;
# each outer CV fold has a separate whole-participant inner validation holdout.
study_plan <- function(output,reference,release,scripts) {
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  for(folder in c('data','partitions','models','validation','training','selection','reports'))
    dir.create(file.path(output,folder),showWarnings=FALSE)
  old <- readRDS(file.path(reference,'head_width_protocol.rds'))
  stopifnot(study_hash(file.path(reference,'prepared_data.rds'))==old$data_sha256)
  d <- readRDS(file.path(reference,'prepared_data.rds'))
  stopifnot(nrow(d)==55122L,length(unique(d$Anon.Student.Id))==478L,
    length(unique(d$KC..Default.))==144L,length(unique(d$KC..Cluster.))==36L)
  original <- file.path(reference,'largerawsample_source.rds')
  # Reference exports contain the original source dataset next to the prepared data.
  stopifnot(file.exists(original),study_hash(original)==old$input_sha256)
  source <- readRDS(original)
  stopifnot(all(d$KC..Default.==source$KC..Default.[d$comparison_source_row]),
    all(d$KC..Cluster.==source$KC..Cluster.[d$comparison_source_row]),
    !any(source$Outcome[d$comparison_source_row]=='STUDY'))
  study_write_rds(d,file.path(output,'data/prepared_data.rds'))
  study_write_rds(old,file.path(output,'data/reference_protocol.rds'))
  participants <- sort(unique(as.character(d$Anon.Student.Id)),method='radix')
  set.seed(20261103);outer <- sample(rep(1:5,length.out=length(participants)))
  partitions <- list(development=d$fold);assignments <- list()
  for(fold in 1:5) {
    train <- participants[outer!=fold]
    set.seed(20261103L+fold)
    validation <- sample(train,ceiling(.2*length(train)))
    partitions[[paste0('cv',fold)]] <- ifelse(as.character(d$Anon.Student.Id) %in% participants[outer==fold],3L,
      ifelse(as.character(d$Anon.Student.Id) %in% validation,2L,1L))
  }
  for(name in names(partitions)) {
    stopifnot(all(partitions[[name]] %in% 1:3))
    group <- unique(data.frame(participant=as.character(d$Anon.Student.Id),fold=partitions[[name]]))
    stopifnot(!anyDuplicated(group$participant))
    study_write_rds(as.integer(partitions[[name]]),file.path(output,'partitions',paste0(name,'.rds')))
    assignments[[name]] <- cbind(partition=name,group)
  }
  study_write_csv(do.call(rbind,assignments),file.path(output,'participant_assignments.csv'))
  candidates <- settings <- list();k <- 0L
  for(family in c('matched','raw','augmented'))for(stage in c('plain','encoders','inference','combined')) {
    grid <- expand.grid(encoder_width=if(stage %in% c('encoders','combined'))c(16L,32L) else 0L,
      inference_width=if(stage %in% c('inference','combined'))c(64L,128L) else 0L,
      inference_depth=if(stage %in% c('inference','combined'))1:2 else 0L,
      latent_size=if(stage %in% c('inference','combined'))c(32L,64L) else 0L,
      learning_rate=c(.001,.003))
    for(i in seq_len(nrow(grid))) {
      k <- k+1L;id <- sprintf('g%03d',k);row <- grid[i,]
      base <- old$settings[[paste(family,'head128','seed1',sep='__')]]
      stopifnot(!is.null(base),base$num_layers==3L,base$hidden_size==32L,base$head_hidden_sizes==128L)
      base$inference_mode <- if(stage %in% c('inference','combined'))'deterministic' else 'none'
      base$feature_encoder <- if(stage %in% c('encoders','combined'))'mlp' else 'none'
      base$encoder_hidden_sizes <- if(row$encoder_width)as.integer(row$encoder_width) else 16L
      base$encoder_output_size <- 16L;base$encoder_activation <- 'relu';base$encoder_dropout <- 0
      if(row$inference_width)base$inference_hidden_sizes <- rep(as.integer(row$inference_width),row$inference_depth)
      if(row$latent_size)base$latent_size <- as.integer(row$latent_size)
      base$learning_rate <- row$learning_rate
      settings[[id]] <- do.call(LKT::GRUModel,base)$settings
      candidates[[id]] <- cbind(candidate=id,family=family,stage=stage,grid[i,])
    }
  }
  candidates <- do.call(rbind,candidates);rownames(candidates) <- NULL
  stopifnot(nrow(candidates)==162L,all(table(candidates$family)==54L))
  protocol <- list(schema=1L,release=release,baseline=old$baseline,candidates=candidates,settings=settings,
    seeds=1:3,partitions=names(partitions),data_sha256=study_hash(file.path(output,'data/prepared_data.rds')),
    reference_protocol_sha256=study_hash(file.path(reference,'head_width_protocol.rds')),
    partition_hashes=setNames(vapply(names(partitions),function(p)study_hash(file.path(output,'partitions',paste0(p,'.rds'))),character(1)),names(partitions)),
    script_hashes=setNames(vapply(scripts,study_hash,character(1)),basename(scripts)),
    grid_fits_per_partition=486L,followup_fits_per_partition=36L,total_fits=3132L,
    selection='Mean validation log loss across seeds 1:3; ties choose fewer parameters, then candidate ID. All selection precedes test scoring.',
    followup='Best validation-selected deterministic inference candidate per family: Gaussian beta 0 and .001. Plain-head capacity match uses nearest integer width and both declared learning rates.',
    cv='Five outer whole-participant folds, seed 20261103; independent 20% participant validation within each outer training fold. Repeat the complete grid and followup selection independently.',
    caveats=c('Previously inspected dataset and development split; architecture development has used this dataset.',
      'CV is conditional on the prespecified fixed engineered-feature parameters inherited from the reference experiment; these are not reestimated in each fold.',
      'UNVaMP-inspired ablation, not a reproduction of unpublished winning settings. Current engineered inputs may be item-dependent.',
      'Scores average individual fitted-model metrics across seeds, not ensemble probabilities.'))
  study_write_rds(protocol,file.path(output,'study.rds'))
  study_write_csv(candidates,file.path(output,'candidates.csv'))
  writeLines(capture.output(dput(protocol)),file.path(output,'study.txt'))
  cat('FROZEN: 486 grid + 36 followup fits per partition; 6 partitions; total 3132.\n')
  invisible(protocol)
}
study_load <- function(output,release,scripts) {
  p <- readRDS(file.path(output,'study.rds'))
  stopifnot(identical(p$release,release),study_hash(file.path(output,'data/prepared_data.rds'))==p$data_sha256,
    identical(setNames(vapply(scripts,study_hash,character(1)),basename(scripts)),p$script_hashes))
  p
}
study_data <- function(output,p,partition) {
  stopifnot(partition %in% p$partitions)
  path <- file.path(output,'partitions',paste0(partition,'.rds'))
  stopifnot(study_hash(path)==p$partition_hashes[[partition]])
  d <- readRDS(file.path(output,'data/prepared_data.rds'));d$fold <- readRDS(path);d
}
study_tasks <- function(p,partition) {
  rows <- lapply(seq_len(nrow(p$candidates)),function(i)do.call(rbind,lapply(p$seeds,function(seed) {
    row <- p$candidates[i,];cbind(run_id=paste(partition,row$family,row$stage,row$candidate,paste0('seed',seed),sep='__'),
      partition=partition,row,seed=seed)
  })))
  do.call(rbind,rows)
}

# One fit is the unit of work. Checkpoints contain portable fitted weights and
# diagnostics, but do not duplicate the common raw dataset or null-model object.
study_fit_one <- function(task,s,output,p,d) {
  s$seed <- as.integer(task$seed);s <- do.call(LKT::GRUModel,s)$settings
  id <- task$run_id;path <- file.path(output,'models',paste0(id,'.rds'))
  fingerprint <- digest::digest(list(study=study_hash(file.path(output,'study.rds')),task=task,settings=s),algo='sha256')
  if(file.exists(path)) {
    saved <- readRDS(path);stopifnot(identical(saved$fingerprint,fingerprint))
  } else {
    cat('START',id,'\n');flush.console()
    elapsed <- system.time(fit <- LKT::LKT(data.table::copy(data.table::as.data.table(d)),usefolds=1,
      components=p$baseline$components,features=p$baseline$features,fixedpars=p$baseline$fixedpars,
      interc=TRUE,model=do.call(LKT::GRUModel,s),verbose=FALSE))[['elapsed']]
    fit$newdata <- NULL;fit$nullmodel <- NULL
    saved <- list(fingerprint=fingerprint,task=task,settings=s,fit=fit,elapsed=elapsed)
    study_write_rds(saved,path)
  }
  fit <- saved$fit
  stopifnot(identical(fit$gru_settings,s),identical(fit$fit_rows,which(d$fold==1L)),
    identical(fit$validation_rows,which(d$fold==2L)),identical(fit$evaluation_rows,which(d$fold==3L)))
  score <- cbind(task,as.data.frame(as.list(study_metrics(d$CF..ansbin.[d$fold==2L],fit$prediction[d$fold==2L]))),
    parameter_count=fit$parameter_count,selected_epoch=fit$selected_epoch,epochs_run=sum(fit$training_history$epoch>0L),
    cap_hit=sum(fit$training_history$epoch>0L)==s$epochs,elapsed=saved$elapsed)
  study_write_csv(score,file.path(output,'validation',paste0(id,'.csv')))
  study_write_csv(fit$training_history,file.path(output,'training',paste0(id,'.csv')))
  cat('DONE',id,'validation',score$log_loss,'\n');flush.console()
  invisible(NULL)
}
study_fit_grid <- function(output,p,partition,shard,shards,workers,library) {
  d <- study_data(output,p,partition);tasks <- study_tasks(p,partition)
  tasks <- tasks[(seq_len(nrow(tasks))-1L)%%shards==shard,,drop=FALSE]
  run <- function(i)study_fit_one(tasks[i,,drop=FALSE],p$settings[[tasks$candidate[i]]],output,p,d)
  if(workers==1L)invisible(lapply(seq_len(nrow(tasks)),run)) else {
    cluster <- parallel::makePSOCKcluster(workers,outfile=file.path(output,paste0(partition,'-shard',shard,'.log')))
    on.exit(parallel::stopCluster(cluster))
    parallel::clusterCall(cluster,function(paths,lib) {.libPaths(paths);library(LKT,lib.loc=lib);NULL},.libPaths(),library)
    parallel::clusterExport(cluster,c('study_fit_one','study_hash','study_write_rds','study_write_csv','study_metrics'))
    parallel::clusterExport(cluster,c('tasks','p','d','output','run'),envir=environment())
    invisible(parallel::parLapplyLB(cluster,seq_len(nrow(tasks)),run))
  }
}
study_validation <- function(output,tasks) {
  records <- lapply(tasks$run_id,function(id) {
    path <- file.path(output,'validation',paste0(id,'.csv'));stopifnot(file.exists(path));data.table::fread(path)
  })
  scores <- data.table::rbindlist(records)
  stopifnot(!anyDuplicated(scores$run_id),setequal(scores$run_id,tasks$run_id))
  summary <- scores[,.(validation_log_loss=mean(log_loss),seed_sd=sd(log_loss),seeds=.N,
    parameter_count=unique(parameter_count),mean_epoch=mean(selected_epoch),cap_hits=sum(cap_hit)),by=.(family,stage,candidate)]
  stopifnot(all(summary$seeds==3L))
  data.table::setorder(summary,family,stage,validation_log_loss,parameter_count,candidate)
  list(scores=scores,summary=summary,selected=summary[,head(.SD,1L),by=.(family,stage)])
}

# Freeze all primary choices using validation before fitting diagnostic followups.
# The control matches total parameters by widening only the ordinary prediction head.
study_followup_plan <- function(output,p,partition) {
  tasks <- study_tasks(p,partition);v <- study_validation(output,tasks)
  directory <- file.path(output,'selection');dir.create(directory,showWarnings=FALSE)
  study_write_csv(as.data.frame(v$summary),file.path(directory,paste0(partition,'-grid.csv')))
  study_write_csv(as.data.frame(v$selected),file.path(directory,paste0(partition,'-primary.csv')))
  best <- v$summary[stage %in% c('inference','combined')]
  data.table::setorder(best,family,validation_log_loss,parameter_count,candidate)
  best <- best[,head(.SD,1L),by=family]
  extra <- settings <- list();k <- 0L
  for(family_name in unique(best$family)) {
    target <- best[family==family_name]
    base <- v$selected[family==family_name & stage=='plain']
    baseline <- p$settings[[base$candidate]]
    saved <- readRDS(file.path(output,'models',paste0(partition,'__',family_name,'__plain__',base$candidate,'__seed1.rds')))
    prep <- saved$fit$model$preprocessing
    input <- baseline$hidden_size+length(prep$numeric_names)+sum(prep$embedding_dims)
    factor <- input+1L+as.integer(baseline$head_bias)
    nonhead <- base$parameter_count-128L*factor-as.integer(baseline$head_bias)
    width <- max(1L,as.integer(round((target$parameter_count-nonhead-as.integer(baseline$head_bias))/factor)))
    conditions <- list(gaussian=list(inference_mode='gaussian',temporal_kl_beta=0),
      gaussian_kl=list(inference_mode='gaussian',temporal_kl_beta=.001),
      capacity_lr001=list(head_hidden_sizes=width,learning_rate=.001),
      capacity_lr003=list(head_hidden_sizes=width,learning_rate=.003))
    for(name in names(conditions))for(seed in p$seeds) {
      k <- k+1L
      s <- if(startsWith(name,'capacity'))baseline else p$settings[[target$candidate]]
      s <- utils::modifyList(s,conditions[[name]],keep.null=TRUE);s$seed <- seed
      id <- paste(partition,family_name,name,paste0('seed',seed),sep='__')
      settings[[id]] <- do.call(LKT::GRUModel,s)$settings
      extra[[id]] <- data.frame(run_id=id,partition=partition,family=family_name,
        stage=if(startsWith(name,'capacity'))'capacity' else name,candidate=name,seed=seed,
        target_candidate=target$candidate,target_parameter_count=target$parameter_count)
    }
  }
  result <- list(tasks=do.call(rbind,extra),settings=settings,best_inference=as.data.frame(best))
  rownames(result$tasks) <- NULL
  study_write_rds(result,file.path(directory,paste0(partition,'-followup.rds')))
  result
}

study_finalize <- function(output,p,partition,workers,library) {
  extra <- study_followup_plan(output,p,partition);d <- study_data(output,p,partition)
  run <- function(i)study_fit_one(extra$tasks[i,,drop=FALSE],extra$settings[[extra$tasks$run_id[i]]],output,p,d)
  if(workers==1L)invisible(lapply(seq_len(nrow(extra$tasks)),run)) else {
    cluster <- parallel::makePSOCKcluster(workers,outfile=file.path(output,paste0(partition,'-followup.log')))
    on.exit(parallel::stopCluster(cluster))
    parallel::clusterCall(cluster,function(paths,lib) {.libPaths(paths);library(LKT,lib.loc=lib);NULL},.libPaths(),library)
    parallel::clusterExport(cluster,c('study_fit_one','study_hash','study_write_rds','study_write_csv','study_metrics'))
    parallel::clusterExport(cluster,c('extra','p','d','output','run'),envir=environment())
    invisible(parallel::parLapplyLB(cluster,seq_len(nrow(extra$tasks)),run))
  }
  primary <- study_validation(output,study_tasks(p,partition))
  followup <- study_validation(output,extra$tasks)
  choices <- rbind(primary$selected[,.(family,stage,candidate)],followup$selected[,.(family,stage,candidate)])
  choices <- rbind(choices,data.table::as.data.table(extra$best_inference)[,.(family,stage='best_inference',candidate)])
  study_write_csv(as.data.frame(choices),file.path(output,'selection',paste0(partition,'-final.csv')))
  study_score_partition(output,p,partition,choices,extra)
}

# Reconstruct probabilities from weights before scoring a selected test checkpoint.
# The batched replay follows each complete held-out history with dropout disabled.
study_replay <- function(fit,d) {
  ns <- asNamespace('LKT');s <- get('lkt_gru_settings',ns)(fit$gru_settings)
  cleanup <- get('lkt_gru_runtime',ns)(s);on.exit(cleanup())
  net <- LKT::LKTGRURuntime(fit)$network
  encoded <- get('lkt_gru_encode',ns)(d,fit$model$preprocessing,fit$model$recipe,s)
  groups <- split(seq_len(nrow(d)),get('lkt_gru_keys',ns)(d,s$history_group_cols));q <- numeric(nrow(d))
  torch::with_no_grad({
    for(start in seq.int(1L,length(groups),by=s$batch_size)) {
      g <- groups[seq.int(start,min(length(groups),start+s$batch_size-1L))]
      longest <- max(lengths(g));chunk <- if(is.null(s$sequence_length))longest else s$sequence_length;hidden <- context <- NULL
      for(offset in seq.int(1L,longest,by=chunk)) {
        active <- which(lengths(g)>=offset);g <- g[active]
        if(!is.null(hidden))hidden <- hidden[,active,,drop=FALSE]
        context <- get('lkt_gru_context_subset',ns)(context,active)
        positions <- lapply(g,function(i)seq.int(offset,min(length(i),offset+chunk-1L)))
        b <- get('lkt_gru_batch',ns)(encoded,d,g,positions,s)
        out <- if(s$inference_mode=='none' && s$retrieval_heads==0L)net(b$numeric,b$categorical,b$answers,hidden) else
          net(b$numeric,b$categorical,b$answers,hidden,context,b$retrieval_ids)
        pred <- torch::as_array(torch::torch_sigmoid(out$logits)$cpu())
        keep <- !is.na(b$rows);q[b$rows[keep]] <- pred[keep];hidden <- out$hidden
        if(s$inference_mode!='none' || s$retrieval_heads>0L)context <- out$context
      }
    }
  })
  pmin(pmax(q,s$clip_epsilon),1-s$clip_epsilon)
}
study_score_partition <- function(output,p,partition,choices,extra) {
  d <- study_data(output,p,partition);all_tasks <- data.table::rbindlist(list(study_tasks(p,partition),extra$tasks),fill=TRUE)
  scores <- predictions <- list();seen <- new.env(parent=emptyenv())
  for(i in seq_len(nrow(choices))) {
    choice <- choices[i,];tasks <- all_tasks[family==choice$family & candidate==choice$candidate]
    stopifnot(nrow(tasks)==3L)
    for(j in seq_len(nrow(tasks))) {
      task <- tasks[j,];id <- task$run_id
      if(!exists(id,seen,inherits=FALSE)) {
        saved <- readRDS(file.path(output,'models',paste0(id,'.rds')));fit <- saved$fit
        # Independent validation recomputation catches stale or mismatched score files.
        val <- data.table::fread(file.path(output,'validation',paste0(id,'.csv')))
        m <- study_metrics(d$CF..ansbin.[d$fold==2L],fit$prediction[d$fold==2L])
        for(n in names(m))stopifnot(abs(val[[n]]-m[[n]])<1e-12)
        q <- study_replay(fit,d[d$fold==3L,,drop=FALSE])
        stopifnot(max(abs(q-fit$prediction[d$fold==3L]))<3e-6)
        assign(id,list(q=q,fit=fit),seen)
      }
      result <- get(id,seen,inherits=FALSE);fit <- result$fit;q <- result$q
      key <- paste(choice$family,choice$stage,task$seed,sep='__')
      scores[[key]] <- data.frame(partition=partition,family=choice$family,stage=choice$stage,
        candidate=choice$candidate,seed=task$seed,as.list(study_metrics(d$CF..ansbin.[d$fold==3L],q)),
        parameter_count=fit$parameter_count,selected_epoch=fit$selected_epoch)
      predictions[[key]] <- data.frame(partition=partition,family=choice$family,stage=choice$stage,
        candidate=choice$candidate,seed=task$seed,source_row=d$comparison_source_row[d$fold==3L],
        participant=d$Anon.Student.Id[d$fold==3L],observed=d$CF..ansbin.[d$fold==3L],prediction=q)
    }
  }
  data.table::fwrite(data.table::rbindlist(scores),file.path(output,'reports',paste0(partition,'-test.csv')))
  data.table::fwrite(data.table::rbindlist(predictions),file.path(output,'reports',paste0(partition,'-predictions.csv')))
  writeLines(c('PASS: primary and followup choices frozen by validation before test scoring.',
    'PASS: all selected checkpoints replayed from saved weights; validation and test metrics independently scored.',
    'No test-based architecture/configuration/seed selection. Three separate initialization seeds, not an ensemble.'),
    file.path(output,'reports',paste0(partition,'-verification.txt')))
}
