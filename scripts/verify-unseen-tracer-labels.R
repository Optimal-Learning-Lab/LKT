# ROOT SMOKE_MODEL DATA OUTPUT_RDS. Runtime conformance only, no model fitting.
a<-commandArgs(TRUE);stopifnot(length(a)==4L)
stopifnot(!file.exists(a[4]))
root<-normalizePath(a[1],winslash='/')
source(file.path(root,'scripts/load-current-lkt.R'))
fit<-readRDS(a[2])$fit;d<-readRDS(a[3]);s<-fit$gru_settings
stopifnot(identical(s$adaptive_embedding_cols,c('KC..Default.','KC..Cluster.')))
close<-function(x,y)stopifnot(max(abs(x-y))<3e-6)
run<-LKTGRURuntime(fit);start<-LKTGRUState(fit)
shared<-serialize(fit$model$weights,NULL)
first_student<-as.character(d$Anon.Student.Id[which(d$fold==3L)[1L]])
probe<-head(d[d$Anon.Student.Id==first_student & d$fold==3L,,drop=FALSE],12L)
old<-LKTGRUStep(fit,start,probe,run)
renamed<-probe;renamed$Anon.Student.Id<-'previously-unseen-student'
for(col in s$categorical_cols) {
 renamed[[col]]<-paste0('never-trained-',col,'-',renamed[[col]])
 stopifnot(!any(renamed[[col]]%in%fit$model$preprocessing$vocabularies[[col]]))
}
new<-LKTGRUStep(fit,start,renamed,run);close(old$predictions,new$predictions)
# Introduce two genuinely new skills after fitting. The third trial reuses the
# first skill on a different new item; there is no fixed vocabulary requirement.
x<-probe[1:3,,drop=FALSE];x$Anon.Student.Id<-'online-new-student'
x$KC..Default.<-c('never-trained-item-A','never-trained-item-B','never-trained-item-C')
x$KC..Cluster.<-c('never-trained-skill-S','never-trained-skill-T','never-trained-skill-S')
x$CF..Time.<-c(0,60,120);x$Duration..sec.<-10;x$CF..ansbin.<-c(1L,0L,1L)
x$gru_item_age_seconds<-0;x$gru_mean_prior_spacing_seconds<-NA_real_
stopifnot(!any(x$KC..Cluster.%in%fit$model$preprocessing$vocabularies[['KC..Cluster.']]))
skills<-function(state)state$adaptive_embeddings$students[['online-new-student']][['KC..Cluster.']]
one<-LKTGRUStep(fit,start,x[1,,drop=FALSE],run)
stopifnot(identical(names(skills(one$state)),'never-trained-skill-S'),
 any(skills(one$state)[['never-trained-skill-S']]!=0))
candidates<-x[rep(2L,2L),,drop=FALSE]
candidates$KC..Cluster.<-c('never-trained-skill-S','never-trained-skill-T')
before<-serialize(one$state,NULL)
probabilities<-LKTGRUScore(fit,one$state,candidates,run)
stopifnot(identical(before,serialize(one$state,NULL)),all(is.finite(probabilities)),
 is.null(skills(one$state)[['never-trained-skill-T']]))
two<-LKTGRUStep(fit,one$state,x[2,,drop=FALSE],run)
stopifnot(setequal(names(skills(two$state)),c('never-trained-skill-S','never-trained-skill-T')),
 identical(skills(one$state)[['never-trained-skill-S']],skills(two$state)[['never-trained-skill-S']]),
 any(skills(two$state)[['never-trained-skill-T']]!=0))
three<-LKTGRUStep(fit,two$state,x[3,,drop=FALSE],run)
stopifnot(identical(skills(two$state)[['never-trained-skill-T']],skills(three$state)[['never-trained-skill-T']]),
 !identical(skills(two$state)[['never-trained-skill-S']],skills(three$state)[['never-trained-skill-S']]))
fresh<-x;fresh$Anon.Student.Id<-'another-new-student'
close(LKTGRUStep(fit,start,fresh,run)$predictions,LKTGRUStep(fit,three$state,fresh,run)$predictions)
stopifnot(identical(shared,serialize(fit$model$weights,NULL)))
result<-list(passed=TRUE,never_trained_skill_labels_accepted=TRUE,new_labels_remain_distinct=TRUE,
 model_sha256=digest::digest(file=a[2],algo='sha256'),
 new_skills_can_arrive_after_prior_answers=TRUE,reused_skill_updates_same_local_entry=TRUE,
 scoring_does_not_allocate_or_update_skills=TRUE,new_student_resets_items_and_skills=TRUE,
 consistent_renaming_maximum_prediction_difference=max(abs(old$predictions-new$predictions)),
 shared_weights_unchanged=TRUE,interpretation='Runtime conformance on the supplied saved model; no new fitting or AUC evaluation.')
saveRDS(result,a[4]);print(result)
