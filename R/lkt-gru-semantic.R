# Frozen text features belong only to completed recurrent updates. The external
# encoder is explicit, local and hash-bound; the package never searches for one.
lkt_gru_semantic_active <- function(s) !is.null(s$semantic_input)

lkt_gru_semantic_validate <- function(s) {
  z <- s$semantic_input
  if(is.null(z))return(invisible(NULL))
  fields <- c('question_col','response_col','kc_col','kc_strip_number','python','helper',
    'helper_sha256','model_dir','model_id','revision','manifest_sha256')
  lkt_gru_named(z,c(fields,'cache_file','cache_sha256'),'semantic_input')
  if(!all(fields %in% names(z)))stop('semantic_input requires every explicit encoder and text binding.',call.=FALSE)
  if(xor(is.null(z$cache_file),is.null(z$cache_sha256)))stop('Specify both semantic cache_file and cache_sha256.',call.=FALSE)
  if(!is.null(z$cache_file) && (!is.character(z$cache_file) || length(z$cache_file)!=1L || is.na(z$cache_file) || !nzchar(z$cache_file) || !is.character(z$cache_sha256) || length(z$cache_sha256)!=1L || is.na(z$cache_sha256) || !grepl('^[a-f0-9]{64}$',z$cache_sha256)))stop('Invalid explicit semantic cache binding.',call.=FALSE)
  for(n in setdiff(fields,'kc_strip_number'))
    if(!is.character(z[[n]]) || length(z[[n]])!=1L || is.na(z[[n]]) || !nzchar(z[[n]]))
      stop('Invalid semantic_input ',n,'.',call.=FALSE)
  if(!is.logical(z$kc_strip_number) || length(z$kc_strip_number)!=1L || is.na(z$kc_strip_number))
    stop('semantic_input kc_strip_number must be TRUE or FALSE.',call.=FALSE)
  if(anyDuplicated(unlist(z[c('question_col','response_col','kc_col')],use.names=FALSE)))
    stop('Semantic question, response and KC columns must be distinct.',call.=FALSE)
  for(n in c('helper_sha256','manifest_sha256'))if(!grepl('^[a-f0-9]{64}$',z[[n]]))
    stop('Semantic hashes must be lowercase SHA256 values.',call.=FALSE)
  if(z$model_id!='sentence-transformers/all-mpnet-base-v2' ||
     z$revision!='e8c3b32edf5434bc2275fc9bab85f82640a19130')
    stop('Semantic input requires the pinned all-mpnet-base-v2 model revision.',call.=FALSE)
  if(s$inference_mode!='none' || s$hidden_decay || lkt_gru_gate_active(s) ||
     lkt_gru_adaptive(s) || !s$plasticity %in% c('none','constant') ||
     isTRUE(s$input_to_all_layers) || !is.null(s$initial_model_file))
    stop('Semantic input supports ordinary first-layer-only GRU, optional retrieval and none/constant output adaptation; other extensions and warm starts are unsupported.',call.=FALSE)
  invisible(NULL)
}

lkt_gru_semantic_hash <- function(file) {
  if(!requireNamespace('digest',quietly=TRUE))stop('Semantic input requires the optional digest package.',call.=FALSE)
  digest::digest(file=file,algo='sha256',serialize=FALSE)
}

lkt_gru_semantic_helper <- function(texts,z) {
  if(!requireNamespace('jsonlite',quietly=TRUE))stop('Semantic input requires the optional jsonlite package.',call.=FALSE)
  for(n in c('python','helper'))if(!file.exists(z[[n]]))stop('Semantic ',n,' file is unavailable: ',z[[n]],call.=FALSE)
  manifest <- file.path(z$model_dir,'lkt-semantic-manifest.json')
  if(!file.exists(manifest))stop('Semantic model manifest is unavailable.',call.=FALSE)
  if(lkt_gru_semantic_hash(z$helper)!=z$helper_sha256 ||
     lkt_gru_semantic_hash(manifest)!=z$manifest_sha256)
    stop('Semantic helper or model manifest SHA256 mismatch.',call.=FALSE)
  input <- tempfile(fileext='.json');output <- tempfile(fileext='.json');log <- tempfile()
  on.exit(unlink(c(input,output,log)),add=TRUE)
  jsonlite::write_json(list(texts=I(enc2utf8(texts))),input,auto_unbox=TRUE,na='null')
  args <- c(z$helper,'--model-dir',z$model_dir,'--manifest-sha256',z$manifest_sha256,
    '--request',input,'--output',output)
  status <- system2(z$python,vapply(args,shQuote,character(1)),stdout=log,stderr=log)
  if(!identical(as.integer(status),0L) || !file.exists(output))
    stop('Pinned semantic encoder failed: ',paste(readLines(log,warn=FALSE),collapse='\n'),call.=FALSE)
  result <- jsonlite::fromJSON(output,simplifyVector=TRUE)
  if(!identical(result$model_id,z$model_id) || !identical(result$revision,z$revision) ||
     !identical(result$manifest_sha256,z$manifest_sha256) ||
     !identical(as.integer(result$embedding_dim),768L) || !is.list(result$package_versions))
    stop('Semantic encoder returned incompatible model metadata.',call.=FALSE)
  x <- if(!length(texts))matrix(numeric(),0L,768L) else result$embeddings
  if(length(texts)==1L && is.numeric(x) && is.null(dim(x)))x <- matrix(x,1L)
  if(!is.matrix(x) || !is.numeric(x) || !identical(dim(x),c(length(texts),768L)) ||
     anyNA(x) || any(!is.finite(x)))stop('Semantic encoder must return finite N by768 vectors.',call.=FALSE)
  list(embeddings=x,metadata=result[setdiff(names(result),'embeddings')])
}

lkt_gru_semantic_cache <- function(saved=NULL) {
  if(!is.null(saved) && (!is.list(saved$metadata) || !is.character(saved$texts) || anyNA(saved$texts) || anyDuplicated(saved$texts) ||
      !is.matrix(saved$embeddings) || !is.numeric(saved$embeddings) ||
      !identical(dim(saved$embeddings),c(length(saved$texts),768L)) ||
      anyNA(saved$embeddings) || any(!is.finite(saved$embeddings)) ||
      !identical(digest::digest(saved$texts,algo='sha256'),saved$text_sha256)))
    stop('Saved semantic cache is corrupt or incompatible.',call.=FALSE)
  e <- new.env(parent=emptyenv())
  e$texts <- if(is.null(saved))character() else saved$texts
  e$embeddings <- if(is.null(saved))matrix(numeric(),0L,768L) else saved$embeddings
  e$metadata <- if(is.null(saved))NULL else saved$metadata
  e
}

lkt_gru_semantic_vectors <- function(texts,s,cache) {
  if(!is.character(texts) || anyNA(texts))stop('Semantic text must be character and cannot contain NA; literal empty responses are valid.',call.=FALSE)
  texts <- enc2utf8(texts)
  absent <- unique(texts[!texts %in% cache$texts])
  if(length(absent)) {
    encoded <- lkt_gru_semantic_helper(absent,s$semantic_input)
    if(!is.null(cache$metadata) && !identical(encoded$metadata,cache$metadata))
      stop('Semantic encoder metadata changed since this model cache was created.',call.=FALSE)
    cache$metadata <- encoded$metadata
    cache$texts <- c(cache$texts,absent);cache$embeddings <- rbind(cache$embeddings,encoded$embeddings)
  }
  match(texts,cache$texts)
}

lkt_gru_semantic_rows <- function(data,s,cache) {
  z <- s$semantic_input
  completed <- !is.na(data$CF..ansbin.)
  ids <- matrix(0L,nrow(data),3L)
  if(!any(completed))return(ids)
  cols <- unlist(z[c('question_col','response_col','kc_col')],use.names=FALSE)
  lkt_require_columns(data,cols,'Completed semantic interaction')
  values <- lapply(cols,function(n) {
    x <- data[[n]][completed]
    if(is.factor(x))x <- as.character(x)
    if(!is.character(x) || anyNA(x))stop('Completed semantic text must be character with no NA: ',n,call.=FALSE)
    enc2utf8(x)
  })
  if(any(!nzchar(values[[1L]])) || any(!nzchar(values[[3L]])))
    stop('Semantic question and KC descriptions cannot be empty.',call.=FALSE)
  if(z$kc_strip_number)values[[3L]] <- sub('^[[:space:]]*[0-9]+[[:space:]]*[.):_-]?[[:space:]]*','',values[[3L]])
  if(any(!nzchar(values[[3L]])))stop('Semantic KC text is empty after removing its numeric prefix.',call.=FALSE)
  ids[completed,] <- matrix(lkt_gru_semantic_vectors(unlist(values,use.names=FALSE),s,cache),sum(completed),3L)
  ids
}

lkt_gru_semantic_snapshot <- function(cache)list(texts=cache$texts,embeddings=cache$embeddings,
  metadata=cache$metadata,text_sha256=digest::digest(cache$texts,algo='sha256'))

lkt_gru_semantic_tensor <- function(encoded,rows,s) {
  if(!lkt_gru_semantic_active(s))return(NULL)
  rows <- as.matrix(rows);out <- array(0,c(nrow(rows),ncol(rows),2304L))
  for(i in seq_len(nrow(rows)))for(t in seq_len(ncol(rows)))if(!is.na(rows[i,t])) {
    ids <- encoded$semantic_ids[rows[i,t],]
    if(all(ids>0L))out[i,t,] <- as.vector(t(encoded$semantic_cache$embeddings[ids,,drop=FALSE]))
  }
  lkt_gru_tensor(out,s)
}

lkt_gru_semantic_interaction <- function(net,interaction,semantic,s) {
  if(!lkt_gru_semantic_active(s))return(interaction)
  if(is.null(semantic) || length(semantic$size())!=3L || semantic$size(3L)!=2304L ||
     !identical(as.integer(semantic$size()[1:2]),as.integer(interaction$size()[1:2])))
    stop('Completed semantic vectors are missing or incompatible.',call.=FALSE)
  torch::torch_cat(list(interaction,torch::torch_tanh(net$semantic_projection(semantic))),dim=3L)
}

# An optional cache file is a content-addressed input, never a searched location.
lkt_gru_semantic_saved_cache <- function(prep,s) {
  if(!is.null(prep$semantic_cache))return(prep$semantic_cache)
  z <- s$semantic_input
  if(is.null(z$cache_file))return(NULL)
  if(!file.exists(z$cache_file) || lkt_gru_semantic_hash(z$cache_file)!=z$cache_sha256)
    stop('Explicit semantic cache file is unavailable or its SHA256 differs.',call.=FALSE)
  readRDS(z$cache_file)
}

lkt_gru_semantic_prepare_cache <- function(texts,semantic_input,encoder_result=NULL) {
  s <- GRUModel(semantic_input=semantic_input)$settings
  cache <- lkt_gru_semantic_cache()
  if(is.null(encoder_result))lkt_gru_semantic_vectors(unique(texts),s,cache) else {
    if(!is.character(texts) || anyNA(texts) || anyDuplicated(texts))stop('Imported semantic cache requires exact unique ordered texts.',call.=FALSE)
    expected <- lkt_gru_semantic_helper(character(),semantic_input)$metadata
    metadata <- encoder_result[setdiff(names(encoder_result),'embeddings')]
    if(!identical(metadata,expected))stop('Imported semantic encoder metadata differs.',call.=FALSE)
    cache$texts <- enc2utf8(texts);cache$embeddings <- encoder_result$embeddings;cache$metadata <- metadata
    lkt_gru_semantic_cache(lkt_gru_semantic_snapshot(cache))
  }
  lkt_gru_semantic_snapshot(cache)
}

lkt_gru_semantic_check_runtime <- function(cache,s) {
  result <- lkt_gru_semantic_helper(character(),s$semantic_input)
  if(!is.null(cache$metadata) && !identical(cache$metadata,result$metadata))
    stop('Pinned semantic runtime differs from the saved encoder contract.',call.=FALSE)
  invisible(NULL)
}
