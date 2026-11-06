# Pure protocol helpers. Neural computation remains in the maintained LKT package.
ednet_author_tasks <- function(reference) {
  paths <- list.files(file.path(reference,'pkg/config/reproduce/ktst/benchmark'),
    pattern='^reproduce_[0-9]+[.]yaml$',full.names=TRUE)
  rows <- lapply(paths,function(path) {
    x <- yaml::read_yaml(path)
    if(!identical(x$data$dataset,'ednet') || !identical(x$data$format,'set_dense') ||
       !identical(x$model$aggregation,'q_mean_c'))return(NULL)
    data.frame(fold=paste0('fold',x$data$val_fold_idx),author_fold=as.integer(x$data$val_fold_idx),
      seed=as.integer(x$seed),author_config=basename(path),stringsAsFactors=FALSE)
  })
  tasks <- do.call(rbind,rows)
  if(is.null(tasks) || nrow(tasks)!=5L || !identical(sort(tasks$author_fold),0:4))
    stop('Expected one released KTST (mean) configuration per validation fold.')
  tasks <- tasks[order(tasks$author_fold),];rownames(tasks) <- NULL
  tasks$id <- with(tasks,paste0(fold,'__seed',seed));tasks
}

ednet_author_roles <- function(data,author_fold) {
  if(length(author_fold)!=1L || is.na(author_fold) || !author_fold %in% 0:4)stop('Invalid author fold.')
  v <- data$ktst_validation_fold
  if(is.null(v) || anyNA(v) || any(!v %in% -1:4))stop('Missing or invalid author assignments.')
  roles <- as.integer(ifelse(v==-1L,3L,ifelse(v==author_fold,2L,1L)))
  ednet_check_roles(data,roles);roles
}

ednet_check_roles <- function(data,roles) {
  stopifnot(length(roles)==nrow(data),!anyNA(roles),all(roles %in% 1:3),all(1:3 %in% roles))
  student_roles <- unique(data.frame(student=as.character(data$Anon.Student.Id),role=roles))
  if(anyDuplicated(student_roles$student))stop('A student crosses fitting roles.')
  invisible(TRUE)
}

ednet_author_bootstrap <- function(y,predictions,subjects,draws=2000L,seed=20261105L) {
  predictions <- as.matrix(predictions)
  stopifnot(length(y)==nrow(predictions),length(subjects)==length(y),all(y %in% 0:1),
    all(is.finite(predictions)),length(unique(y))==2L)
  ids <- match(subjects,sort(unique(subjects)));n <- max(ids)
  auc <- lapply(seq_len(ncol(predictions)),function(k) {
    index <- order(predictions[,k]);p <- predictions[index,k];outcomes <- as.double(y[index]);who <- ids[index]
    ties <- rle(p);ends <- cumsum(ties$lengths);groups <- rep(seq_along(ends),ties$lengths)
    function(counts) {
      positive <- as.double(counts[who])*outcomes;negative <- as.double(counts[who])*(1-outcomes)
      cumulative <- cumsum(negative);before <- c(0,cumulative[head(ends,-1L)])
      credit <- before[groups]+.5*(cumulative[ends][groups]-before[groups])
      sum(positive*credit)/(sum(positive)*sum(negative))
    }
  })
  had_seed <- exists('.Random.seed',.GlobalEnv,inherits=FALSE)
  if(had_seed)old_seed <- get('.Random.seed',.GlobalEnv)
  on.exit(if(had_seed)assign('.Random.seed',old_seed,.GlobalEnv) else
    if(exists('.Random.seed',.GlobalEnv,inherits=FALSE))rm('.Random.seed',envir=.GlobalEnv))
  set.seed(seed)
  vapply(seq_len(draws),function(i) {
    counts <- tabulate(sample.int(n,n,replace=TRUE),nbins=n)
    mean(vapply(auc,function(f)f(counts),numeric(1)))
  },numeric(1))
}
