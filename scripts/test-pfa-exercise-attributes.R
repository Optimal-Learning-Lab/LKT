# ROOT. Run in the authorized Big Blue candidate environment; no models are fit.
a <- commandArgs(TRUE);stopifnot(length(a)==1L)
root <- normalizePath(a[1],winslash='/',mustWork=TRUE)
library(LKT)
source(file.path(root,'scripts/verify-pfa-exercise-attributes.R'))
source(file.path(root,'tests/fixtures/gru-exercise-attributes-fixture.R'))
reject <- function(expr,pattern) {
 error <- tryCatch(force(expr),error=identity)
 stopifnot(inherits(error,'error'),grepl(pattern,conditionMessage(error),fixed=TRUE))
}
directory <- tempfile('synthetic-pfa-preflight-');dir.create(directory)
fixture <- test_gru_exercise_fixture(directory)
settings <- GRUModel(categorical_cols=c('item','skill'),zero_init_embedding_cols='item',exercise_attributes=fixture$controls)$settings
spec <- list(kind='mechanism',paper_id='pfa-90bae1222bc1',mechanism_family='LLM_exercise_attribute_representation',
 expected_parameter_count=41218L+8L*2L)
reject(pfa_verify_exercise_attributes(spec,settings,'missing.rds'),'synthetic test metadata are prohibited')
# A toy artifact cannot become this study merely by changing its purpose marker.
settings$exercise_attributes$purpose <- 'study'
reject(pfa_verify_exercise_attributes(spec,settings,'missing.rds'),'require SPEC contract_file')
write <- fixture$write
spec$original_review_file <- file.path(directory,'SYNTHETIC-review.json')
write(list(description='SYNTHETIC TEST REVIEW ONLY'),spec$original_review_file)
hash <- function(file)digest::digest(file=file,algo='sha256',serialize=FALSE)
review_sha256 <- hash(spec$original_review_file)
spec$contract_file <- file.path(directory,'implementation-contract.json')
spec$annotation_plan_file <- file.path(directory,'annotation-plan.json')
write(list(paper_id=spec$paper_id,original_review_sha256=review_sha256),spec$contract_file)
write(list(paper_id=spec$paper_id,original_review_sha256=review_sha256,original_score=7L,
 input_contract=list(expected_item_count=4L,expected_kc_group_count=2L,unique_prompt_strings=2L,
 unique_stripped_kc_statements=2L,data_sha256=fixture$controls$source_data_sha256)),spec$annotation_plan_file)
spec$contract_sha256 <- hash(spec$contract_file);spec$annotation_plan_sha256 <- hash(spec$annotation_plan_file)
reject(pfa_verify_exercise_attributes(spec,settings,'missing.rds'),'frozen144-item/36-KC source contract')
bad <- spec;bad$contract_sha256 <- paste(rep('0',64),collapse='')
reject(pfa_verify_exercise_attributes(bad,settings,'missing.rds'),'contract_file SHA256 mismatch')
bad <- spec;bad$annotation_plan_sha256 <- paste(rep('0',64),collapse='')
reject(pfa_verify_exercise_attributes(bad,settings,'missing.rds'),'annotation_plan_file SHA256 mismatch')
bad <- spec;bad$paper_id <- 'different-paper'
reject(pfa_verify_exercise_attributes(bad,settings,'missing.rds'),'wrong selected paper or mechanism')
unlink(directory,recursive=TRUE)
cat('PASS PFA rejects synthetic/toy study substitution, absent contract bindings, changed contract/plan bytes and wrong selected paper; no models executed.\n')
