# Explicitly synthetic test artifacts. This helper never annotates real content,
# invokes an LLM, or creates a study-ready catalog.
test_gru_exercise_fixture <- function(directory,n=4L,k=2L) {
 dir.create(directory,recursive=TRUE,showWarnings=FALSE)
 json <- function(x)as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null='null',digits=17))
 hash <- function(x)digest::digest(charToRaw(enc2utf8(x)),algo='sha256',serialize=FALSE)
 write <- function(x,path)writeBin(charToRaw(enc2utf8(json(x))),path)
 ids <- sprintf('item%03d',seq_len(n));kcs <- sprintf('kc%03d',1L+(seq_len(n)-1L)%%k)
 vocabulary <- list(list(ability_id='A',name='Toy A',definition='Synthetic ability A'),list(ability_id='B',name='Toy B',definition='Synthetic ability B'))
 values <- seq(0,1,length.out=n)
 items <- lapply(seq_len(n),function(i)list(item_id=ids[i],difficulty=values[i],expected_seconds=100*values[i],
  ability_ids=if(i%%2L)list('A') else list('A','B'),kc_id=kcs[i],
  difficulty_rank=1L+floor(5*(i-1L)/n),time_rank=1L+floor(5*(i-1L)/n)))
 catalog <- lapply(seq_len(n),function(i)list(item_id=ids[i],question=paste('Synthetic repeated question',i%%2L),
  kc_id=kcs[i],kc_statement=paste('Synthetic statement',kcs[i]),accepted_answers=if(i==1L)list('yes|y','YES') else list('yes|y')))
 reviews <- lapply(seq_len(n),function(i)list(item_id=ids[i],proposed_answer='yes',explanation='Synthetic test explanation',
  accepted_answer_match='yes',content_validation_complete=TRUE,corrections=list()))
 source_hash <- hash('SYNTHETIC TEST DATA ONLY')
 artifact <- list(model='gpt-4o-2024-08-06',vocabulary=vocabulary,items=items,source_data_sha256=source_hash,
  content_validation_complete=TRUE,frozen_utc='2026-11-10T00:00:00Z')
 artifact_file <- file.path(directory,'synthetic-attributes.json');write(artifact,artifact_file)
 prompts <- lapply(c('vocabulary','solutions','attributes'),function(stage) {
  text <- paste('SYNTHETIC TEST PROMPT',stage);list(text=text,sha256=hash(text))
 });names(prompts) <- c('vocabulary','solutions','attributes')
 raw <- function(stage,group,selected) {
  input <- list(items=lapply(catalog[selected],function(row)row[c('item_id','question','kc_id','kc_statement')]))
  if(stage=='attributes') {
   input$vocabulary <- vocabulary
   input$solutions <- lapply(reviews[selected],function(row)row[c('item_id','proposed_answer','explanation')])
  }
  output <- switch(stage,vocabulary=list(abilities=vocabulary),
   solutions=list(solutions=lapply(reviews[selected],function(row)c(row[c('item_id','proposed_answer','explanation')],list(ambiguous=FALSE,ambiguity_note='')))),
   attributes=list(attributes=lapply(items[selected],function(row)row[c('item_id','difficulty','expected_seconds','ability_ids')])))
  input <- json(input);output <- json(output)
  list(stage=stage,group_id=group,model='gpt-4o-2024-08-06',response_id=paste('synthetic',stage,group,sep='-'),
   input=input,input_sha256=hash(input),output=output,output_sha256=hash(output))
 }
 outputs <- list(raw('vocabulary','ALL',seq_len(n)))
 for(stage in c('solutions','attributes'))for(group in sort(unique(kcs),method='radix'))outputs[[length(outputs)+1L]] <- raw(stage,group,which(kcs==group))
 artifact_hash <- digest::digest(file=artifact_file,algo='sha256',serialize=FALSE)
 provenance <- list(schema=1L,purpose='synthetic_test',model='gpt-4o-2024-08-06',temperature=0L,seed=1L,seed_supported=TRUE,
  source_data_sha256=source_hash,artifact_sha256=artifact_hash,prompts=prompts,catalog=catalog,reviews=reviews,raw_outputs=outputs)
 provenance_file <- file.path(directory,'synthetic-provenance.json');write(provenance,provenance_file)
 controls <- list(artifact_file=artifact_file,artifact_sha256=artifact_hash,provenance_file=provenance_file,
  provenance_sha256=digest::digest(file=provenance_file,algo='sha256',serialize=FALSE),item_col='item',kc_col='skill',
  question_col='question',answers_col='answers',expected_item_count=n,expected_kc_count=k,source_data_sha256=source_hash,
  vocabulary_prompt_sha256=prompts$vocabulary$sha256,solutions_prompt_sha256=prompts$solutions$sha256,
  attributes_prompt_sha256=prompts$attributes$sha256,purpose='synthetic_test')
 list(controls=controls,artifact=artifact,provenance=provenance,catalog=catalog,json=json,hash=hash,write=write)
}
