# Offline, hash-bound exercise attributes replace only the free item-ID lookup.
# The original item padding row stays a padding-only parameter with zero gradient.
lkt_gru_exercise_active <- function(s)!is.null(s$exercise_attributes)
lkt_gru_exercise_hash <- function(x) {
  if(!requireNamespace('digest',quietly=TRUE))stop('Exercise attributes require digest.',call.=FALSE)
  digest::digest(x,algo='sha256',serialize=FALSE)
}
lkt_gru_exercise_validate <- function(s) {
  z <- s$exercise_attributes
  if(is.null(z))return(invisible(NULL))
  fields <- c('artifact_file','artifact_sha256','provenance_file','provenance_sha256',
    'item_col','kc_col','question_col','answers_col','expected_item_count','expected_kc_count',
    'source_data_sha256','vocabulary_prompt_sha256','solutions_prompt_sha256','attributes_prompt_sha256','purpose')
  lkt_gru_named(z,fields,'exercise_attributes')
  if(!setequal(names(z),fields))stop('Exercise attributes require all explicit artifact, provenance and catalog bindings.',call.=FALSE)
  for(n in setdiff(fields,c('expected_item_count','expected_kc_count')))
    if(!is.character(z[[n]]) || length(z[[n]])!=1L || is.na(z[[n]]) || !nzchar(z[[n]]))stop('Invalid exercise_attributes ',n,'.',call.=FALSE)
  for(n in fields[endsWith(fields,'sha256')])if(!grepl('^[a-f0-9]{64}$',z[[n]]))stop('Exercise attribute hashes must be lowercase SHA256.',call.=FALSE)
  for(n in c('expected_item_count','expected_kc_count'))
    if(!is.numeric(z[[n]]) || length(z[[n]])!=1L || !is.finite(z[[n]]) || z[[n]]<1 || z[[n]]!=as.integer(z[[n]]))stop('Invalid exercise catalog count.',call.=FALSE)
  if(!z$purpose %in% c('study','synthetic_test'))stop('Exercise attribute purpose must be study or synthetic_test.',call.=FALSE)
  dims <- if(!is.null(names(s$embedding_dims)))s$embedding_dims[s$categorical_cols] else rep(s$embedding_dims,length(s$categorical_cols))
  if(!identical(s$categorical_cols,c(z$item_col,z$kc_col)) || s$categorical_encoding!='embedding' ||
     length(dims)!=2L || any(dims!=16L) || !z$item_col %in% s$zero_init_embedding_cols)
    stop('Exercise attributes replace the first16-wide zero-initialized item embedding and retain the separate KC16.',call.=FALSE)
  if(lkt_sequence_kind(s)!='gru' || s$inference_mode!='none' || s$hidden_decay || lkt_gru_gate_active(s) ||
     lkt_gru_adaptive(s) || lkt_gru_semantic_active(s) || isTRUE(s$latent_denoising) ||
     !s$plasticity %in% c('none','constant') || isTRUE(s$input_to_all_layers) || !is.null(s$initial_model_file))
    stop('Exercise attributes support the ordinary GRU, optional retrieval and none/constant output adaptation; other mechanism combinations are unsupported.',call.=FALSE)
  invisible(NULL)
}

# Keep the published validated-item-artifact shape intact. A separately hash-bound
# provenance sidecar supplies exact prompts, catalog/answer alternatives, raw outputs
# and content-review records that the original short JSON schema does not encode.
lkt_gru_exercise_parse <- function(artifact_raw,provenance_raw,z) {
  if(!requireNamespace('jsonlite',quietly=TRUE))stop('Exercise attributes require jsonlite.',call.=FALSE)
  fail <- function(message)stop('Invalid exercise attribute artifact: ',message,call.=FALSE)
  fields <- function(x,names,label) {
    if(!is.list(x) || is.null(names(x)) || anyDuplicated(names(x)) || !setequal(names(x),names))fail(paste(label,'fields differ.'))
  }
  string <- function(x) is.character(x) && length(x)==1L && !is.na(x) && nzchar(x)
  strings <- function(x) is.list(x) && length(x)>0L && all(vapply(x,string,logical(1))) && !anyDuplicated(unlist(x,use.names=FALSE))
  number <- function(x,lo,hi) is.numeric(x) && length(x)==1L && is.finite(x) && x>=lo && x<=hi
  hash <- function(x)string(x) && grepl('^[a-f0-9]{64}$',x)
  if(!is.raw(artifact_raw) || !is.raw(provenance_raw) ||
     lkt_gru_exercise_hash(artifact_raw)!=z$artifact_sha256 || lkt_gru_exercise_hash(provenance_raw)!=z$provenance_sha256)fail('bound file hash mismatch.')
  a <- jsonlite::fromJSON(rawToChar(artifact_raw),simplifyVector=FALSE)
  p <- jsonlite::fromJSON(rawToChar(provenance_raw),simplifyVector=FALSE)
  fields(a,c('model','vocabulary','items','source_data_sha256','content_validation_complete','frozen_utc'),'annotation')
  if(!identical(a$model,'gpt-4o-2024-08-06') || !identical(a$source_data_sha256,z$source_data_sha256) ||
     !identical(a$content_validation_complete,TRUE) || !string(a$frozen_utc))fail('model, source, validation or freeze identity differs.')
  if(!is.list(a$vocabulary) || !length(a$vocabulary) || !is.list(a$items) || length(a$items)!=z$expected_item_count)fail('empty vocabulary or wrong item coverage.')
  for(v in a$vocabulary) {
    fields(v,c('ability_id','name','definition'),'ability')
    if(!all(vapply(v,string,logical(1))))fail('empty ability definition.')
  }
  abilities <- vapply(a$vocabulary,`[[`,character(1),'ability_id')
  if(anyDuplicated(abilities))fail('duplicate ability ID.')
  for(item in a$items) {
    fields(item,c('item_id','difficulty','expected_seconds','ability_ids','kc_id','difficulty_rank','time_rank'),'item')
    if(!string(item$item_id) || !string(item$kc_id) || !number(item$difficulty,0,1) ||
       !number(item$expected_seconds,0,100) || !strings(item$ability_ids) ||
       any(!unlist(item$ability_ids,use.names=FALSE)%in%abilities))fail('invalid item values or ability membership.')
    for(n in c('difficulty_rank','time_rank'))if(!number(item[[n]],1,5) || item[[n]]!=as.integer(item[[n]]))fail('invalid quantitative rank.')
  }
  ids <- vapply(a$items,`[[`,character(1),'item_id');kcs <- vapply(a$items,`[[`,character(1),'kc_id')
  if(anyDuplicated(ids) || length(unique(kcs))!=z$expected_kc_count || !identical(order(ids,method='radix'),seq_along(ids)))fail('duplicate, unsorted or incomplete item/KC identities.')
  for(pair in list(c('difficulty','difficulty_rank'),c('expected_seconds','time_rank'))) {
    values <- vapply(a$items,`[[`,numeric(1),pair[1L]);ranks <- integer(length(ids))
    ranks[order(values,ids,method='radix')] <- 1L+floor(5*(seq_along(ids)-1L)/length(ids))
    if(any(vapply(a$items,`[[`,numeric(1),pair[2L])!=ranks))fail('ranks differ from frozen global stable-ID ranking.')
  }
  fields(p,c('schema','purpose','model','temperature','seed','seed_supported','source_data_sha256',
    'artifact_sha256','prompts','catalog','reviews','raw_outputs'),'provenance')
  if(!number(p$schema,1,1) || !identical(p$purpose,z$purpose) || !identical(p$model,a$model) ||
     !number(p$temperature,0,0) || !is.logical(p$seed_supported) || length(p$seed_supported)!=1L || is.na(p$seed_supported) ||
     (p$seed_supported && !number(p$seed,1,1)) || (!p$seed_supported && !is.null(p$seed)) ||
     !identical(p$source_data_sha256,z$source_data_sha256) || !identical(p$artifact_sha256,z$artifact_sha256))fail('provenance model/request/source binding differs.')
  fields(p$prompts,c('vocabulary','solutions','attributes'),'prompt set')
  for(stage in names(p$prompts)) {
    prompt <- p$prompts[[stage]];fields(prompt,c('text','sha256'),'prompt')
    if(!string(prompt$text) || !hash(prompt$sha256) || lkt_gru_exercise_hash(charToRaw(enc2utf8(prompt$text)))!=prompt$sha256 ||
       prompt$sha256!=z[[paste0(stage,'_prompt_sha256')]])fail('prompt hash differs.')
  }
  if(!is.list(p$catalog) || length(p$catalog)!=length(ids) || !is.list(p$reviews) || length(p$reviews)!=length(ids))fail('catalog/review coverage differs.')
  for(i in seq_along(ids)) {
    c <- p$catalog[[i]];r <- p$reviews[[i]]
    fields(c,c('item_id','question','kc_id','kc_statement','accepted_answers'),'catalog row')
    if(!identical(c$item_id,ids[i]) || !identical(c$kc_id,kcs[i]) || !string(c$question) || !string(c$kc_statement) || !strings(c$accepted_answers))fail('catalog identity/text/complete answer list is invalid.')
    fields(r,c('item_id','proposed_answer','explanation','accepted_answer_match','content_validation_complete','corrections'),'content review')
    alternatives <- trimws(unlist(strsplit(unlist(c$accepted_answers,use.names=FALSE),'|',fixed=TRUE),use.names=FALSE))
    if(any(!nzchar(alternatives)) || !identical(r$item_id,ids[i]) || !string(r$proposed_answer) || !string(r$explanation) ||
       !identical(r$content_validation_complete,TRUE) || !string(r$accepted_answer_match) ||
       !r$accepted_answer_match%in%alternatives || !is.list(r$corrections))fail('missing content review or accepted-answer evidence.')
    for(change in r$corrections) {
      fields(change,c('before','after','content_reason'),'correction')
      if(!all(vapply(change,string,logical(1))))fail('invalid content correction record.')
    }
  }
  if(!is.list(p$raw_outputs) || length(p$raw_outputs)!=1L+2L*z$expected_kc_count)fail('raw output coverage differs from fixed annotation stages.')
  keys <- character()
  for(raw in p$raw_outputs) {
    fields(raw,c('stage','group_id','model','response_id','input','input_sha256','output','output_sha256'),'raw output')
    if(!raw$stage%in%c('vocabulary','solutions','attributes') || !string(raw$group_id) || !identical(raw$model,a$model) ||
       !string(raw$response_id) || !string(raw$input) || !hash(raw$input_sha256) ||
       lkt_gru_exercise_hash(charToRaw(enc2utf8(raw$input)))!=raw$input_sha256 || !string(raw$output) || !hash(raw$output_sha256) ||
       lkt_gru_exercise_hash(charToRaw(enc2utf8(raw$output)))!=raw$output_sha256)fail('raw request/output identity or hash invalid.')
    input <- jsonlite::fromJSON(raw$input,simplifyVector=FALSE)
    required <- if(raw$stage=='attributes')c('items','vocabulary','solutions') else 'items'
    fields(input,required,'content-only request input')
    selected <- if(raw$stage=='vocabulary')seq_along(ids) else which(kcs==raw$group_id)
    expected_items <- lapply(p$catalog[selected],function(row)row[c('item_id','question','kc_id','kc_statement')])
    if(!length(selected) || !identical(input$items,expected_items))fail('request input differs from immutable content-only item catalog.')
    if(raw$stage=='attributes') {
      expected_solutions <- lapply(p$reviews[selected],function(row)row[c('item_id','proposed_answer','explanation')])
      if(!identical(input$vocabulary,a$vocabulary) || !identical(input$solutions,expected_solutions))fail('attribute request omitted frozen vocabulary or validated solutions.')
    }
    output <- jsonlite::fromJSON(raw$output,simplifyVector=FALSE)
    output_field <- switch(raw$stage,vocabulary='abilities',solutions='solutions',attributes='attributes')
    fields(output,output_field,'parsed raw output')
    if(!is.list(output[[output_field]]) || !length(output[[output_field]]))fail('empty parsed raw output.')
    if(raw$stage=='vocabulary' && !identical(output$abilities,a$vocabulary))fail('frozen vocabulary differs from raw vocabulary output.')
    if(raw$stage!='vocabulary') {
      output_ids <- vapply(output[[output_field]],function(row)if(string(row$item_id))row$item_id else '',character(1))
      if(!identical(output_ids,ids[selected]))fail('raw output item coverage/order differs.')
      for(j in seq_along(selected)) {
        row <- output[[output_field]][[j]];item <- a$items[[selected[j]]];review <- p$reviews[[selected[j]]]
        if(raw$stage=='attributes') {
          fields(row,c('item_id','difficulty','expected_seconds','ability_ids'),'raw attribute')
          if(!number(row$difficulty,0,1) || !number(row$expected_seconds,0,100) || !strings(row$ability_ids) ||
             any(!unlist(row$ability_ids,use.names=FALSE)%in%abilities))fail('raw attribute values are invalid.')
          changed <- !isTRUE(all.equal(row,item[c('item_id','difficulty','expected_seconds','ability_ids')],tolerance=0,check.attributes=FALSE))
        } else {
          fields(row,c('item_id','proposed_answer','explanation','ambiguous','ambiguity_note'),'raw solution')
          if(!string(row$proposed_answer) || !string(row$explanation) || !is.logical(row$ambiguous) ||
             length(row$ambiguous)!=1L || is.na(row$ambiguous) || !is.character(row$ambiguity_note) || length(row$ambiguity_note)!=1L || is.na(row$ambiguity_note))fail('raw generated solution is invalid.')
          changed <- !identical(row[c('item_id','proposed_answer','explanation')],review[c('item_id','proposed_answer','explanation')])
        }
        if(changed && !length(review$corrections))fail('content correction lacks a recorded content-based reason.')
      }
    }
    keys <- c(keys,paste(raw$stage,raw$group_id,sep=':'))
  }
  expected <- c('vocabulary:ALL',paste('solutions',sort(unique(kcs),method='radix'),sep=':'),paste('attributes',sort(unique(kcs),method='radix'),sep=':'))
  if(anyDuplicated(keys) || !setequal(keys,expected))fail('raw stage/KC coverage differs.')
  statements <- vapply(p$catalog,`[[`,character(1),'kc_statement')
  if(any(vapply(split(statements,kcs),function(x)length(unique(x))!=1L,logical(1))))fail('one stored KC has inconsistent statements.')
  mask <- matrix(0,length(ids)+1L,length(abilities))
  for(i in seq_along(ids))mask[i+1L,match(unlist(a$items[[i]]$ability_ids,use.names=FALSE),abilities)] <- 1
  list(artifact_raw=artifact_raw,provenance_raw=provenance_raw,item_ids=ids,kc_ids=kcs,ability_ids=abilities,
    difficulty=c(1L,vapply(a$items,`[[`,numeric(1),'difficulty_rank')),
    time=c(1L,vapply(a$items,`[[`,numeric(1),'time_rank')),ability_mask=mask,catalog=p$catalog,
    model=a$model,artifact_sha256=z$artifact_sha256,provenance_sha256=z$provenance_sha256)
}

lkt_gru_exercise_load <- function(s) {
  z <- s$exercise_attributes
  read <- function(file) {
    if(!file.exists(file))stop('Explicit exercise attribute file is unavailable: ',file,call.=FALSE)
    readBin(file,'raw',n=file.info(file)$size)
  }
  lkt_gru_exercise_parse(read(z$artifact_file),read(z$provenance_file),z)
}
lkt_gru_exercise_saved <- function(saved,s) {
  if(is.null(saved))stop('Saved exercise attribute artifact is missing.',call.=FALSE)
  checked <- lkt_gru_exercise_parse(saved$artifact_raw,saved$provenance_raw,s$exercise_attributes)
  if(!identical(checked,saved))stop('Saved exercise attribute mapping changed.',call.=FALSE)
  checked
}
lkt_gru_exercise_check_rows <- function(data,s,saved,content=FALSE) {
  z <- s$exercise_attributes
  lkt_require_columns(data,c(z$item_col,z$kc_col),'Exercise attribute current question')
  ids <- match(as.character(data[[z$item_col]]),saved$item_ids)
  if(anyNA(ids) || anyNA(data[[z$kc_col]]) || any(as.character(data[[z$kc_col]])!=saved$kc_ids[ids]))
    stop('Current item is absent from the immutable exercise catalog or has a different KC.',call.=FALSE)
  if(content) {
    lkt_require_columns(data,c(z$question_col,z$answers_col),'Exercise attribute authentic catalog')
    for(i in seq_len(nrow(data))) {
      catalog <- saved$catalog[[ids[i]]]
      if(is.na(data[[z$question_col]][i]) || as.character(data[[z$question_col]][i])!=catalog$question ||
         is.na(data[[z$answers_col]][i]) || !as.character(data[[z$answers_col]][i])%in%unlist(catalog$accepted_answers,use.names=FALSE))
        stop('Exercise catalog question or accepted-answer provenance differs from source rows.',call.=FALSE)
    }
  }
  invisible(NULL)
}

lkt_gru_exercise_padding <- function(weight) {
  self <- NULL
  definition <- torch::nn_module(initialize=function(weight) {
    self$weight <- torch::nn_parameter(weight[1L,,drop=FALSE]$detach()$clone())
  },forward=function(ids) {
    # R indices1 are converted to native indices0, while nnf_embedding's
    # padding_idx is passed to LibTorch unchanged in torch0.17.0.
    torch::nnf_embedding(torch::torch_ones_like(ids),self$weight,padding_idx=0L)
  })
  definition(weight)
}
lkt_gru_exercise_module <- function(saved) {
  self <- NULL;B <- length(saved$ability_ids)
  definition <- torch::nn_module(initialize=function() {
    self$difficulty <- torch::nn_embedding(5L,8L)
    self$time <- torch::nn_embedding(5L,8L)
    self$ability <- torch::nn_embedding(B,8L)
    for(n in c('difficulty','time','ability'))torch::nn_init_xavier_uniform_(self[[n]]$weight)
    self$projection <- torch::nn_linear(24L,16L)
    torch::nn_init_zeros_(self$projection$weight);torch::nn_init_zeros_(self$projection$bias)
    self$register_buffer('difficulty_ids',torch::torch_tensor(saved$difficulty,dtype=torch::torch_long()))
    self$register_buffer('time_ids',torch::torch_tensor(saved$time,dtype=torch::torch_long()))
    self$register_buffer('ability_mask',torch::torch_tensor(saved$ability_mask))
  },forward=function(ids) {
    shape <- as.integer(ids$size());flat <- ids$reshape(c(-1L))
    d <- self$difficulty_ids[flat]$reshape(shape)
    t <- self$time_ids[flat]$reshape(shape)
    mask <- self$ability_mask[flat,,drop=FALSE]$reshape(c(shape,B))
    # The zero-membership row is exclusively padding, never an unknown question.
    count <- mask$sum(dim=3L,keepdim=TRUE)
    denominator <- torch::torch_where(ids$unsqueeze(3L)==1L,torch::torch_ones_like(count),count)
    ability <- torch::torch_matmul(mask,self$ability$weight)/denominator
    self$projection(torch::torch_cat(list(self$difficulty(d),ability,self$time(t)),dim=3L))
  })
  definition()
}
lkt_gru_exercise_vector <- function(net,ids,j,s) {
  projected <- net$exercise_attributes(ids)
  torch::torch_where(ids$unsqueeze(3L)==1L,net$embeddings[[j]](ids),projected)
}
lkt_gru_exercise_validate_weights <- function(net,weights,s) {
  expected <- net$state_dict()
  buffers <- paste0('exercise_attributes.',c('difficulty_ids','time_ids','ability_mask'))
  for(n in buffers) {
    value <- weights[[n]];target <- torch::as_array(expected[[n]]$cpu())
    if(!is.numeric(value) || !identical(dim(value),dim(target)) || !identical(as.numeric(value),as.numeric(target)))
      stop('Saved exercise attribute metadata buffer changed: ',n,'.',call.=FALSE)
  }
  learned <- grep('^exercise_attributes[.](difficulty|time|ability|projection)[.]',names(expected),value=TRUE)
  for(n in learned) {
    value <- weights[[n]];target <- torch::as_array(expected[[n]]$cpu())
    if(!is.numeric(value) || !identical(dim(value),dim(target)) || length(value)!=length(target) || any(!is.finite(value)))
      stop('Invalid saved exercise attribute parameter: ',n,'.',call.=FALSE)
  }
  j <- match(s$exercise_attributes$item_col,s$categorical_cols)-1L
  padding <- weights[[paste0('embeddings.',j,'.weight')]]
  if(!is.numeric(padding) || !identical(dim(padding),c(1L,16L)) || any(!is.finite(padding)) || any(padding!=0))
    stop('The retained exercise padding row must be exactly zero.',call.=FALSE)
  invisible(NULL)
}
