# Input preparation is shared by fitting and continuation. No learned state is
# inferred here from validation/evaluation rows.
# Give each configured history an unambiguous key. Length prefixes prevent different
# combinations of identifiers from accidentally sharing memory.
lkt_gru_keys <- function(data,columns) {
  lkt_require_columns(data,columns,'GRU history')
  parts <- lapply(data[columns],function(x) {
    x <- enc2utf8(as.character(x))
    if(anyNA(x)||any(!nzchar(x)))stop('GRU history identifiers cannot be missing or empty.',call.=FALSE)
    paste0(nchar(x,type='bytes'),':',x)
  })
  do.call(paste0,parts)
}

# Sort histories chronologically while preserving the original order of tied times. The
# caller later reverses this ordering so returned predictions still align with the input
# rows.
lkt_gru_order <- function(data,s) {
  if(is.null(s$order_col))return(seq_len(nrow(data)))
  lkt_require_columns(data,c(s$subject_col,s$order_col),'GRU ordering')
  x <- data[[s$order_col]]
  if(!(is.numeric(x)||inherits(x,'POSIXct')) || anyNA(x) || any(!is.finite(as.numeric(x))))
    stop('GRU order_col must contain finite numeric or POSIXct values.',call.=FALSE)
  order(as.character(data[[s$subject_col]]),as.numeric(x),seq_len(nrow(data)),method='radix')
}

lkt_gru_time <- function(data,s) {
  if(is.null(s$time_col))return(NULL)
  lkt_require_columns(data,s$time_col,'GRU timestamps')
  x <- data[[s$time_col]]
  if(!(is.numeric(x)||inherits(x,'POSIXct')) || anyNA(x) || any(!is.finite(as.numeric(x))))
    stop('GRU time_col must contain finite numeric or POSIXct timestamps.',call.=FALSE)
  as.numeric(x)*if(inherits(x,'POSIXct'))1 else switch(s$time_units,seconds=1,minutes=60,hours=3600,days=86400)
}

# Separate weight-fitting rows from validation and evaluation rows. Fold values are
# partition labels, and histories must be whole-person groups or chronological segments
# to prevent future holdout outcomes from entering training.
lkt_gru_masks <- function(data,usefolds,s) {
  if(!((length(usefolds)==1L && is.na(usefolds)) ||
      (is.numeric(usefolds) && length(usefolds)>0L && all(is.finite(usefolds)))))
    stop('GRU usefolds must be numeric fitting folds or a single NA.',call.=FALSE)
  allrows <- length(usefolds)==1L && is.na(usefolds)
  if(!allrows || !is.null(s$validation_folds))lkt_require_columns(data,'fold','GRU fold selection')
  if(!is.null(s$validation_folds) && (allrows || length(intersect(usefolds,s$validation_folds))))
    stop('Explicit validation_folds require disjoint explicit fitting folds.',call.=FALSE)
  train <- if(allrows)rep(TRUE,nrow(data)) else data$fold %in% usefolds
  val <- if(is.null(s$validation_folds))rep(FALSE,nrow(data)) else data$fold %in% s$validation_folds
  if(!any(train))stop('GRU has no fitting rows.',call.=FALSE)
  if(is.null(s$validation_folds) && s$validation_fraction>0) {
    ids <- sort(unique(as.character(data[[s$subject_col]][train])),method='radix')
    n <- max(1L,ceiling(length(ids)*s$validation_fraction))
    if(n>=length(ids))stop('GRU validation splitting requires at least two fitting students; explicitly disable validation for fixed-epoch training.',call.=FALSE)
    # Runtime wrapper preserves the caller's RNG; split RNG is independent of training.
    set.seed(s$split_seed)
    held <- ids[sample.int(length(ids),n)]
    val <- train & as.character(data[[s$subject_col]]) %in% held
    train[val] <- FALSE
    set.seed(s$seed)
  }
  if((s$early_stopping || s$scheduler!='none' || !is.null(s$validation_folds)) && !any(val))
    stop('GRU validation selection is empty.',call.=FALSE)
  phase <- ifelse(train,1L,ifelse(val,2L,3L))
  for(idx in split(seq_len(nrow(data)),as.character(data[[s$subject_col]])))
    if(any(diff(phase[idx])<0))stop('GRU folds must be whole-student groups or chronological training, validation, evaluation segments; interleaved holdout outcomes cannot enter training.',call.=FALSE)
  list(train=train,validation=val,evaluation=!(train|val))
}

lkt_gru_feature_frame <- function(data,recipe) {
  data <- data.table::as.data.table(data.table::copy(data))
  if(!is.null(recipe$subject_col))data$Anon.Student.Id <- data[[recipe$subject_col]]
  if('CF..Time.' %in% names(data)) {
    if(!is.numeric(data$CF..Time.) || anyNA(data$CF..Time.) || any(!is.finite(data$CF..Time.)))
      stop('Engineered GRU features require finite CF..Time. values.',call.=FALSE)
    for(idx in split(seq_len(nrow(data)),as.character(data$Anon.Student.Id)))
      if(any(diff(data$CF..Time.[idx])<0))stop('Engineered GRU timestamps go backwards.',call.=FALSE)
  }
  # Never inherit computed caches or fitted predictions when replaying raw history.
  remove <- unique(c(grep('^\\.lkt\\.',names(data),value=TRUE),'pred','curvefeat'))
  for(n in intersect(remove,names(data)))data[[n]] <- NULL
  # Engineered features depend only on observed prior answers. A pending final
  # answer remains NA; it is not imputed as a success/failure to make code run.
  data$Outcome <- ifelse(is.na(data$CF..ansbin.),'PENDING',ifelse(data$CF..ansbin.==1,'CORRECT','INCORRECT'))
  spacing <- lkt_spacing_requirements(data,recipe$components,recipe$features)
  if(length(spacing$features)) {
    # Recompute spacing from the retained raw history, never trust supplied caches.
    needed <- lkt_spacing_requirements(data.frame(),recipe$components,recipe$features)$components
    data <- computeSpacingPredictors(data,needed)
  }
  data <- lkt_prepare_lkt_data(data,recipe$components,recipe$features,recipe$maketimes,FALSE)
  result <- lkt_build_feature_iteration(data,recipe$components,recipe$features,
    rep(NA_character_,length(recipe$features)),recipe$fixedpars,numeric(0),
    c('+',recipe$connectors),recipe$interacts,recipe$interc,TRUE,FALSE,FALSE,FALSE)
  formula <- lkt_glmm_formula(stats::as.formula(paste('~',result$formula_rhs)))
  # Preserve just the formula variables; no data environment is serialized.
  environment(formula) <- baseenv()
  list(data=as.data.frame(result$data),formula=formula)
}

# Build the engineered design matrix using the same recipe as LKT. Learn any categorical
# schema on training rows and keep that schema fixed for held-out prediction.
lkt_gru_engineered <- function(data,recipe,fit_rows=NULL,schema=NULL,unknown='error') {
  f <- lkt_gru_feature_frame(data,recipe)
  mf <- stats::model.frame(f$formula,f$data,na.action=stats::na.pass)
  if(is.null(schema)) {
    levels <- list()
    for(n in names(mf))if(is.character(mf[[n]])||is.factor(mf[[n]])) {
      known <- sort(unique(as.character(mf[[n]][fit_rows])),method='radix')
      if(anyNA(known))stop('Missing engineered categorical predictor.',call.=FALSE)
      token <- '.LKT_GRU_UNKNOWN'
      while(token %in% known)token <- paste0(token,'_')
      levels[[n]] <- c(known,token)
    }
    schema <- list(levels=levels,formula=f$formula)
  }
  for(n in names(schema$levels)) {
    values <- as.character(mf[[n]]);lv <- schema$levels[[n]]
    unseen <- !values %in% head(lv,-1L)
    if(anyNA(values))stop('Missing engineered categorical predictor.',call.=FALSE)
    if(any(unseen) && unknown=='error')stop('Unknown engineered category in ',n,'.',call.=FALSE)
    values[unseen] <- utils::tail(lv,1L)
    mf[[n]] <- factor(values,levels=lv)
  }
  matrix <- Matrix::sparse.model.matrix(schema$formula,mf)
  # Reserved unknown indicators are zero-encoded by omitting their columns.
  if(is.null(schema$columns)) {
    unknown_columns <- rep(FALSE,ncol(matrix))
    for(lv in schema$levels)unknown_columns <- unknown_columns | grepl(utils::tail(lv,1L),colnames(matrix),fixed=TRUE)
    schema$columns <- colnames(matrix)[!unknown_columns]
  }
  if(!all(schema$columns %in% colnames(matrix)))stop('Engineered GRU predictor schema changed.',call.=FALSE)
  list(matrix=matrix[,schema$columns,drop=FALSE],schema=schema)
}

# Assemble the requested numeric inputs, optional time gap, and optional engineered
# features. The gap is measured within each history, with zero used at a new history's
# first observation.
lkt_gru_numeric <- function(data,recipe,s,engineered=NULL,fit_rows=NULL,previous_time=NULL) {
  lkt_require_columns(data,s$numeric_cols,'GRU numeric inputs')
  parts <- lapply(s$numeric_cols,function(n) {
    v <- data[[n]]
    if(!is.numeric(v)||anyNA(v)||any(!is.finite(v)))stop('GRU numeric predictor must be finite: ',n,call.=FALSE)
    transform <- if(n %in% names(s$numeric_transforms))s$numeric_transforms[[n]] else 'identity'
    if(transform=='log1p') {
      if(any(v<=-1))stop('log1p predictor must be greater than -1: ',n,call.=FALSE)
      v <- log1p(v)
    }
    v
  })
  if(length(parts))names(parts) <- paste0('raw:',s$numeric_cols)
  time <- lkt_gru_time(data,s)
  if(!is.null(time)) {
    gap <- numeric(nrow(data))
    groups <- split(seq_len(nrow(data)),lkt_gru_keys(data,s$history_group_cols))
    for(key in names(groups)) {
      idx <- groups[[key]]
      prior <- previous_time[[key]]
      g <- c(if(is.null(prior))0 else time[idx[1L]]-prior,diff(time[idx]))
      if(any(g<0))stop('GRU timestamps go backwards within a history.',call.=FALSE)
      gap[idx] <- log1p(g)
    }
    parts[['time:log1p_gap_seconds']] <- gap
  }
  # These columns can include study exposures that are absent from the scored response
  # sequence. Keep them in the time tower, with the same transform as recency, rather
  # than mixing them into the engineered-feature tower.
  lkt_require_columns(data,s$time_feature_cols,'GRU additional timing inputs')
  for(n in s$time_feature_cols) {
    value <- data[[n]]
    missing_allowed <- n %in% s$time_missing_cols
    if(!is.numeric(value) || any(is.nan(value)) || (!missing_allowed && anyNA(value)) ||
       any(!is.na(value) & (!is.finite(value) | value<0)))
      stop('GRU timing predictor must contain finite nonnegative seconds: ',n,call.=FALSE)
    parts[[paste0('time:log1p_',n)]] <- log1p(value)
    if(missing_allowed)parts[[paste0('time:available_',n)]] <- as.numeric(!is.na(value))
  }
  out <- if(length(parts))do.call(cbind,parts) else matrix(numeric(0),nrow(data),0L)
  schema <- engineered
  if(s$use_lkt_features) {
    e <- lkt_gru_engineered(data,recipe,fit_rows,schema,s$unknown_levels)
    colnames(e$matrix) <- paste0('lkt:',colnames(e$matrix))
    out <- cbind(out,e$matrix);schema <- e$schema
  }
  values <- if(inherits(out,'sparseMatrix'))out@x else out
  allowed <- paste0('time:log1p_',s$time_missing_cols)
  invalid <- out[,!colnames(out) %in% allowed,drop=FALSE]
  if(anyNA(invalid)||any(!is.finite(values[!is.na(values)])))stop('Engineered GRU predictors must be finite and causal for pending trials.',call.=FALSE)
  list(matrix=out,engineered=schema)
}

# Learn category vocabularies and numeric scaling from training rows only. Validation
# and test responses must not change how a predictor is represented.
lkt_gru_preprocess <- function(data,train,recipe,s) {
  lkt_require_columns(data,s$categorical_cols,'GRU categorical inputs')
  vocab <- lapply(data[s$categorical_cols],function(x) {
    if(anyNA(x)||any(!nzchar(as.character(x))))stop('Missing GRU categorical predictor.',call.=FALSE)
    sort(unique(as.character(x[train])),method='radix')
  })
  dims <- if(!is.null(names(s$embedding_dims)))s$embedding_dims[s$categorical_cols] else rep(s$embedding_dims,length(vocab))
  numeric <- lkt_gru_numeric(data,recipe,s,fit_rows=train)
  x <- numeric$matrix
  training <- x[train,,drop=FALSE]
  mean <- Matrix::colMeans(training)
  center <- if(s$standardize)mean else rep(0,ncol(x))
  scale <- if(s$standardize)sqrt(pmax(0,(Matrix::colSums(training^2)-sum(train)*mean^2)/(sum(train)-1))) else rep(1,ncol(x))
  # Missing timing values provide no observations for estimating a distribution.
  # Their availability indicators remain separate inputs. If a training column has
  # no observed values, its explicit neutral scaling is center zero, scale one.
  for(column in s$time_missing_cols) {
    n <- paste0('time:log1p_',column)
    j <- match(n,colnames(x));observed <- as.numeric(training[,j]);observed <- observed[!is.na(observed)]
    center[j] <- if(s$standardize && length(observed))base::mean(observed) else 0
    scale[j] <- if(s$standardize && length(observed)>1L)stats::sd(observed) else 1
  }
  # A constant column needs a neutral divisor so standardization remains well-defined.
  # In particular, a retained intercept column becomes zero after centering rather than
  # being removed from the saved input schema.
  scale[is.na(scale)|scale==0] <- 1
  list(vocabularies=vocab,embedding_dims=unname(dims),numeric_names=colnames(x),
       center=center,scale=scale,engineered=numeric$engineered)
}

# Apply the frozen vocabulary and engineered-column schema to these rows. The explicit
# unknown-category policy determines whether an unseen value is rejected or zero-
# encoded.
lkt_gru_encode <- function(data,prep,recipe,s,previous_time=NULL) {
  # Exact retrieval identities are metadata, separate from learned embeddings.
  # Retain their strings so unseen items stay distinct and can match later calls.
  lkt_require_columns(data,s$retrieval_filter_cols,'GRU retrieval filter')
  retrieval_ids <- if(length(s$retrieval_filter_cols)) {
    ids <- lapply(data[s$retrieval_filter_cols],function(x) {
      x <- enc2utf8(as.character(x))
      if(anyNA(x)||any(!nzchar(x)))stop('GRU retrieval identifiers cannot be missing or empty.',call.=FALSE)
      x
    })
    do.call(cbind,ids)
  } else NULL
  lkt_require_columns(data,s$categorical_cols,'GRU categorical inputs')
  cat <- matrix(1L,nrow(data),length(prep$vocabularies))
  for(j in seq_along(prep$vocabularies)) {
    v <- data[[names(prep$vocabularies)[j]]]
    if(anyNA(v)||any(!nzchar(as.character(v))))stop('Missing GRU categorical predictor.',call.=FALSE)
    ids <- match(as.character(v),prep$vocabularies[[j]])
    if(anyNA(ids) && s$unknown_levels=='error')stop('Unknown GRU category in ',names(prep$vocabularies)[j],'.',call.=FALSE)
    ids[is.na(ids)] <- 0L;cat[,j] <- ids+1L
  }
  x <- lkt_gru_numeric(data,recipe,s,engineered=prep$engineered,previous_time=previous_time)$matrix
  if(!identical(colnames(x),prep$numeric_names))stop('GRU numeric predictor schema changed.',call.=FALSE)
  # Keep engineered one-hot/interactions sparse. Center only the rows that are
  # about to become a dense minibatch tensor, not the entire dataset.
  list(numeric=x,categorical=cat,center=prep$center,scale=prep$scale,retrieval_ids=retrieval_ids)
}

# Densify and standardize only the rows about to enter a Torch minibatch. Keeping the
# full engineered matrix sparse avoids unnecessary memory use.
lkt_gru_numeric_rows <- function(encoded,rows) {
  x <- as.matrix(encoded$numeric[rows,,drop=FALSE])
  if(ncol(x))x <- sweep(sweep(x,2,encoded$center,'-'),2,encoded$scale,'/')
  # A missing feature is absent evidence, not an observed zero-second interval.
  # The associated availability input tells the network this coordinate is missing;
  # only the dense numerical tensor uses a neutral zero after standardization.
  x[is.na(x)] <- 0
  x
}

lkt_gru_lkt <- function(data,model,usefolds,components,features,fixedpars,interacts,connectors,
                        interc,maketimes,dualfit,nosolve,autoKC,curvefeats,model_options,verbose) {
  # Translate the public LKT call into a GRU fitting request. Unsupported fitting modes
  # are rejected rather than being approximated by a different model.
  s <- model$settings
  if(dualfit || nosolve || any(autoKC!=0) || any(!is.na(curvefeats)) || length(model_options) || any(grepl('@',features,fixed=TRUE)))
    stop('GRU requires accuracy-only fitting, autoKC = 0, no nosolve/random effects/curvefeats; configure controls in GRUModel().',call.=FALSE)
  fixed <- lkt_normalize_parameter_vector(fixedpars,sum(lkt_feature_param_counts(features)),'fixedpars')
  if(any(!is.finite(fixed)) || any(fixed>=1 & fixed%%1==0))
    stop('GRU requires explicit fixed nonlinear feature parameters, independently prefitted on training data; parameter ties are unsupported.',call.=FALSE)
  if(any(lkt_clean_feature_name(features) %in% c('logitdecevol','errordec','diffrelcor1','diffrelcor2','diffcor1','diffcor2','diffcorComp','diffincorComp','diffallComp','diffincor1','diffincor2','diffall1','diffall2')))
    stop('GRU does not support cross-student evolving or model-dependent difficulty/curve features.',call.=FALSE)
  if(length(interacts)==1L && is.na(interacts))interacts <- rep(NA_character_,length(features))
  if(length(interacts)!=length(features) || !length(connectors) %in%
      if(length(features)==1L)c(0L,1L) else length(features)-1L)
    stop('GRU interactions/connectors must align with the feature terms.',call.=FALSE)
  # Freeze the engineered feature recipe separately from neural weights. Later
  # prediction must rebuild the same causal predictors with the same nonlinear
  # parameters.
  recipe <- list(components=components,features=features,fixedpars=fixed,interacts=interacts,
    connectors=if(length(features)==1L)character() else connectors,interc=interc,maketimes=maketimes,subject_col=s$subject_col)
  cleanup <- lkt_gru_runtime(s);on.exit(cleanup(),add=TRUE)
  if(s$use_lkt_features && any(vapply(c(components,interacts[!is.na(interacts)]),function(x)
      any(c('CF..ansbin.','Outcome','pred') %in% strsplit(x,'[^[:alnum:]_.]+')[[1L]]),logical(1))))
    stop('Responses and fitted predictions cannot be engineered GRU predictors.',call.=FALSE)
  raw <- as.data.frame(data)
  if(!'CF..ansbin.' %in% names(raw)) {
    lkt_require_columns(raw,'Outcome','GRU response')
    raw$CF..ansbin. <- ifelse(raw$Outcome=='CORRECT',1,ifelse(raw$Outcome=='INCORRECT',0,NA_real_))
  }
  if(!nrow(raw)||anyNA(raw$CF..ansbin.)||any(!raw$CF..ansbin. %in% c(0,1)))stop('GRU fitting requires nonempty binary outcomes.',call.=FALSE)
  if(!s$subject_col %in% names(raw))stop('Missing GRU subject column.',call.=FALSE)
  lkt_gru_keys(raw,s$history_group_cols)
  ord <- lkt_gru_order(raw,s);d <- raw[ord,,drop=FALSE]
  if(s$use_lkt_features && 'CF..Time.' %in% names(d)) {
    if(anyNA(d$CF..Time.) || !is.numeric(d$CF..Time.) || any(!is.finite(d$CF..Time.)))
      stop('Engineered GRU features require finite CF..Time. values.',call.=FALSE)
    for(idx in split(seq_len(nrow(d)),as.character(d[[s$subject_col]])))
      if(any(diff(d$CF..Time.[idx])<0))stop('Engineered GRU timestamps go backwards; supply chronological histories.',call.=FALSE)
  }
  if(!is.null(s$weight_col)) {
    lkt_require_columns(d,s$weight_col,'GRU weights');w <- d[[s$weight_col]]
    if(!is.numeric(w)||anyNA(w)||any(!is.finite(w)|w<=0))stop('GRU trial weights must be positive finite numbers.',call.=FALSE)
  }
  # Once ordering and input checks are complete, assign the fitting roles and call the
  # model adapter. The adapter receives only the declared data, masks, and feature
  # recipe.
  masks <- lkt_gru_masks(d,usefolds,s)
  request <- list(data=d,response=d$CF..ansbin.,masks=masks,recipe=recipe)
  fit <- model$fit(request,verbose)
  # Return predictions and diagnostic row indices to the caller's original ordering. The
  # intercept-only fit supplies LKT's existing comparison statistic, not an additional
  # neural training stage.
  inverse <- order(ord);p <- fit$pred[inverse];raw$pred <- p
  meta <- fit$model_metadata
  for(n in c('fit_rows','validation_rows','evaluation_rows'))meta[[n]] <- sort(ord[meta[[n]]])
  null <- stats::glm(CF..ansbin.~1,data=raw[meta$fit_rows,,drop=FALSE],family=stats::binomial())
  null_ll <- as.numeric(stats::logLik(null))
  empty <- matrix(numeric(),0L,1L,dimnames=list(character(),'coefficient'))
  specification <- build_model_specification(numeric(),features,components,list())
  reported <- raw
  reported$Anon.Student.Id <- raw[[s$subject_col]]
  result <- lkt_lkt_result(fit$model,empty,specification,if(null_ll==0)NA_real_ else 1-fit$fitstat/null_ll,
    reported,null,NA,NULL,fit$fitstat,model$name,list())
  result$newdata <- raw
  c(result,meta)
}
