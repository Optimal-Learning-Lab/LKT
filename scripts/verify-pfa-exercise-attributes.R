# Frozen PFA/Cloze study gate. Generic attribute parsing and model math belong in
# LKT; only this explicitly selected study owns these data and prompt identities.
pfa_verify_exercise_attributes <- function(spec,settings,source_file) {
  fail <- function(message)stop('EAKT study preflight: ',message,call.=FALSE)
  string <- function(x)is.character(x) && length(x)==1L && !is.na(x) && nzchar(x)
  hash_value <- function(x)string(x) && grepl('^[a-f0-9]{64}$',x)
  same_number <- function(x,n)is.numeric(x) && length(x)==1L && is.finite(x) && x==n
  hash <- function(file)digest::digest(file=file,algo='sha256',serialize=FALSE)
  bound_file <- function(field,hash_field) {
    if(!string(spec[[field]]) || !hash_value(spec[[hash_field]]))fail(paste('require SPEC',field,'and',hash_field))
    file <- normalizePath(spec[[field]],winslash='/',mustWork=TRUE)
    if(!identical(hash(file),spec[[hash_field]]))fail(paste(field,'SHA256 mismatch.'))
    file
  }
  if(!identical(spec$paper_id,'pfa-90bae1222bc1') ||
     !identical(spec$mechanism_family,'LLM_exercise_attribute_representation') ||
     !identical(spec$kind,'mechanism'))fail('wrong selected paper or mechanism.')
  z <- settings$exercise_attributes
  if(is.null(z) || !identical(z$purpose,'study'))fail('real study requires a study-purpose artifact; synthetic test metadata are prohibited.')
  contract_file <- bound_file('contract_file','contract_sha256')
  plan_file <- bound_file('annotation_plan_file','annotation_plan_sha256')
  if(basename(contract_file)!='implementation-contract.json' || basename(plan_file)!='annotation-plan.json' ||
     dirname(contract_file)!=dirname(plan_file))fail('contract and annotation plan must be the explicit paired EAKT preparation files.')
  contract <- jsonlite::fromJSON(contract_file,simplifyVector=FALSE)
  plan <- jsonlite::fromJSON(plan_file,simplifyVector=FALSE)
  if(!string(spec$original_review_file))fail('require SPEC original_review_file.')
  review_file <- normalizePath(spec$original_review_file,winslash='/',mustWork=TRUE)
  review_sha256 <- hash(review_file)
  if(!identical(contract$paper_id,spec$paper_id) || !identical(plan$paper_id,spec$paper_id) ||
     !identical(contract$original_review_sha256,review_sha256) ||
     !identical(plan$original_review_sha256,review_sha256) || !same_number(plan$original_score,7))
    fail('preparation contract/plan differ from the immutable score-seven review.')
  source_sha256 <- '67fc8c70541c67be5e111f4bb8d72b08af305fa1bd1e48ddbd85f1104e0312c1'
  prompt_sha256 <- c(vocabulary='98714f2844bcad5ee14f0cbf71a9648e8dab172308c8a3f1e0a7cc82a0e2b226',
    solutions='2031ece58ecdd1dbd258ae9049155b00af57f3a0c96395066a6e5378d5c75ba4',
    attributes='5c333e43a4321a91dc0f61689a6d3f3c0e8599f18c54ed880fd2cdf5f865ee18')
  input <- plan$input_contract
  if(!same_number(input$expected_item_count,144) || !same_number(input$expected_kc_group_count,36) ||
     !same_number(input$unique_prompt_strings,72) || !same_number(input$unique_stripped_kc_statements,18) ||
     !identical(input$data_sha256,source_sha256) || !identical(z$source_data_sha256,source_sha256) ||
     !same_number(z$expected_item_count,144) || !same_number(z$expected_kc_count,36) ||
     !identical(z$item_col,input$item_id_column) || !identical(z$kc_col,input$kc_id_column) ||
     !identical(z$question_col,input$question_column) || !identical(z$answers_col,input$accepted_answers_column))
    fail('declared metadata differ from the frozen144-item/36-KC source contract.')
  if(!string(source_file) || !file.exists(source_file) || hash(source_file)!=source_sha256)
    fail('actual prepared-data bytes differ from the frozen source.')
  config <- plan$annotation_config
  if(!identical(config$model,'gpt-4o-2024-08-06') || !same_number(config$temperature,0) || !same_number(config$seed,1) ||
     !isTRUE(config$model_availability_verified) || !isTRUE(plan$validated_artifact_exists) || !isTRUE(plan$candidate_protocol_ready))
    fail('exact snapshot access and the completed validated artifact are not verified by the bound annotation plan.')
  if(!same_number(plan$total_requests_planned,73) || !is.list(plan$stages) || length(plan$stages)!=4L ||
     !identical(vapply(plan$stages,function(stage)as.numeric(stage$requests_planned),numeric(1)),c(1,36,36,0)))
    fail('annotation preparation differs from the fixed73-request four-stage pipeline.')
  if(!identical(contract$expected_parameter_count_expression,'43042 -2304 +80 +8*B +400 =41218+8*B'))
    fail('implementation contract changed the frozen parameter formula.')
  if(!identical(contract$input_artifact_sha256,z$artifact_sha256))
    fail('implementation contract has not frozen the actual annotation artifact hash.')
  for(stage in names(prompt_sha256)) {
    file <- file.path(dirname(plan_file),'prompts',paste0(stage,'.txt'))
    if(!file.exists(file) || hash(file)!=prompt_sha256[[stage]] ||
       !identical(z[[paste0(stage,'_prompt_sha256')]],prompt_sha256[[stage]]))
      fail(paste(stage,'prompt differs from the approved content-only prompt.'))
  }
  # Library validation is still strict, but uses declared catalog/hash bindings
  # rather than knowing anything about this particular research dataset.
  internal <- function(n)getFromNamespace(n,'LKT')
  saved <- internal('lkt_gru_exercise_load')(settings)
  if(!identical(saved$model,config$model) || length(saved$item_ids)!=144L || length(unique(saved$kc_ids))!=36L)
    fail('validated artifact snapshot or catalog coverage differs.')
  questions <- vapply(saved$catalog,`[[`,character(1),'question')
  statements <- vapply(saved$catalog,`[[`,character(1),'kc_statement')
  stripped <- sub('^[[:space:]]*[0-9]+[[:space:]]*[.):_-]?[[:space:]]*','',saved$kc_ids)
  answers <- lapply(saved$catalog,function(row)unlist(row$accepted_answers,use.names=FALSE))
  if(length(unique(questions))!=72L || length(unique(statements))!=18L || any(stripped!=statements) ||
     sum(lengths(answers)==1L)!=129L || sum(lengths(answers)==2L)!=15L)
    fail('72/18 text identities, numeric-prefix-only KC statements or129/15 complete answer records differ.')
  data <- readRDS(source_file)
  internal('lkt_gru_exercise_check_rows')(data,settings,saved,TRUE)
  if(!setequal(unique(as.character(data[[z$item_col]])),saved$item_ids))fail('source rows lack full144-item catalog coverage.')
  for(i in seq_along(saved$item_ids)) {
    observed <- unique(as.character(data[[z$answers_col]][as.character(data[[z$item_col]])==saved$item_ids[i]]))
    if(!setequal(observed,answers[[i]]))fail('catalog omitted or invented a recorded accepted-answer string.')
  }
  B <- length(saved$ability_ids);parameters <- 41218L+8L*B
  if(!same_number(plan$ability_count_B,B) || !same_number(contract$ability_count_B,B) ||
     !same_number(plan$trainable_parameter_count,parameters) || !same_number(spec$expected_parameter_count,parameters))
    fail('frozen vocabulary size or41218+8B parameter accounting differs.')
  list(schema=1L,paper_id=spec$paper_id,contract_file=contract_file,contract_sha256=spec$contract_sha256,
    annotation_plan_file=plan_file,annotation_plan_sha256=spec$annotation_plan_sha256,
    original_review_file=review_file,original_review_sha256=review_sha256,
    artifact_sha256=z$artifact_sha256,provenance_sha256=z$provenance_sha256,source_data_sha256=source_sha256,
    prompt_sha256=as.list(prompt_sha256),item_count=144L,kc_count=36L,ability_count_B=B,
    parameter_count=parameters,model=saved$model,purpose='study')
}
