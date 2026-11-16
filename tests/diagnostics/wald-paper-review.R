# Explicit native diagnostic, not part of package checks. Run from any directory:
# Rscript tests/diagnostics/wald-paper-review.R inspect|calibrate|diagnose OUTPUT_DIRECTORY
args <- commandArgs(TRUE)
stopifnot(length(args)==2L)
phase <- args[1]; out <- normalizePath(args[2],winslash='/',mustWork=FALSE)
root <- 'C:/dev/LKT'
source(file.path(root,'scripts/load-current-lkt.R'))
setwd(file.path(dirname(root),'PolicyFactory2'))
library(data.table)
internal <- function(n)get(n,asNamespace('LKT'))
source('workflows/simulation/shared/lktsimulator_parallel.R')
source('workflows/simulation/paper_rerun_wald/calibration_v3.R')
source('workflows/simulation/paper_rerun_wald/real_model.R')
if(phase=='inspect') {
  x <- readRDS('../LKT-outputs/paper-v3-nonlinear-reestimate-20261113/ground_truth_fit.rds')
  print(names(x$model)); print(x$parameters);print(x$specification)
  print(x$model$model_specification);print(names(x$model$data))
  manifest <- jsonlite::fromJSON(system.file('research-source-manifest.json',package='LKT'))
  f <- manifest$files
  for(p in c('R/lkt-latency.R','R/lkt-latency-quantile.R','R/LKTfunctions.R'))
    print(data.frame(path=p,match=digest::digest(file=file.path(root,p),algo='sha256')==f$sha256[match(p,f$path)]))
  quit()
}
dir.create(out,recursive=TRUE,showWarnings=FALSE)
stopifnot(dir.exists(out))
sink(file.path(out,paste0(phase,'.log')),split=TRUE)
if(phase=='calibrate') {
  if(file.exists(file.path(out,'calibrated-model.rds')))stop('Refusing to replace completed calibration')
  config <- readLines('workflows/simulation/paper_rerun_wald/sweep_config.env')
  field <- function(k)sub(paste0('^',k,'='),'',grep(paste0('^',k,'='),config,value=TRUE))
  val <- prepare_paper_v3(field('data_file'),field('calibration_sha256'))
  # Refit linear coefficients only at the confirmed four fixed accuracy parameters.
  fit <- build_real_model(fit_latency=FALSE)
  saveRDS(fit,file.path(out,'accuracy-fixed.rds'))
  ld <- internal('lkt_prepare_latency_data')(fit$newdata)
  saveRDS(ld,file.path(out,'latency-input.rds'))
  set.seed(251115)
  latency <- internal('lkt_fit_latency_model')(ld,verbose=TRUE)
  failure <- internal('lkt_failure_latency')(fit$newdata)
  fit$model_specification[[2]] <- internal('lkt_latency_model_specification')('Wald',latency$model,failure)
  saveRDS(fit,file.path(out,'calibrated-model.rds'))
  saveRDS(list(release=lkt_release,parameters=paper_real_parameters,
    input_sha256=field('calibration_sha256'),rows=nrow(val),students=uniqueN(val$Anon.Student.Id),
    items=uniqueN(val$KC..Default.),latency=latency$model, failure=failure),file.path(out,'calibration-identity.rds'))
  quit()
}
fit <- readRDS(file.path(out,'calibrated-model.rds'))
lat <- fit$model_specification[[2]]
if(phase=='worker') {
  set.seed(811); value <- lat$predict(c(.2,.5,.8),c(-1,0,1),TRUE,return_components=TRUE)
  saveRDS(list(value=value,rng=.Random.seed),file.path(out,'fresh-worker.rds'));quit()
}
stopifnot(phase=='diagnose')
identity_record <- readRDS(file.path(out,'calibration-identity.rds'))
ld <- readRDS(file.path(out,'latency-input.rds'))
stopifnot(identical(identity_record$release,lkt_release),identical(identity_record$parameters,paper_real_parameters),
 identity_record$rows==57328L,identity_record$students==180L,identity_record$items==199L,
 identical(lat$calibration$rt,as.numeric(ld$clean_rt)),
 identical(lat$calibration$p,pmin(as.numeric(ld$clean_pred),.9999)),
 identical(lat$calibration$subject_id,as.character(ld$clean_subject_id)))
execution <- tempfile('execution-',tmpdir=out);dir.create(execution)
writeLines(execution,file.path(out,'latest-execution.txt'))
manifest <- jsonlite::fromJSON(system.file('research-source-manifest.json',package='LKT'))$files
source_paths <- c('R/lkt-latency.R','R/lkt-latency-quantile.R','R/LKTfunctions.R',
 'R/lkt-model-liblinear.R','R/lkt-feature-computation.R','R/lkt-prediction.R')
source_identity <- data.table(path=source_paths,
 source_sha256=vapply(file.path(root,source_paths),function(p)digest::digest(file=p,algo='sha256'),character(1)),
 installed_sha256=manifest$sha256[match(source_paths,manifest$path)])
fwrite(source_identity,file.path(out,'source-identity.csv'))
stopifnot(all(source_identity$source_sha256==source_identity$installed_sha256),
 digest::digest(file=attr(lkt_release,'archive'),algo='sha256')==lkt_release[['lkt_source_sha256']])
checks <- list()
check <- function(name,value) {checks[[name]] <<- value; cat(name,':',value,'\n')}
check('fitting_rows_below_simulator_probability_floor',sum(ld$clean_pred<.0001))
check('fitting_rows_above_Wald_probability_ceiling',sum(ld$clean_pred>.9999))
check('equal_student_centering_residual',abs(mean(lat$subject_effects)))
check('Wald_optimizer_convergence_code',lat$conv)
check('Wald_optimizer_max_gradient',lat$optimizer$max_gradient)
check('failure_mean_matches_empirical_total',identical(lat$failure_latency,
 mean(fit$newdata[CF..ansbin.==0 & is.finite(Duration..sec.) & Duration..sec.>0,Duration..sec.])))
set.seed(442)
imported <- internal('lkt_fit_latency_model')(ld,wald_calibration=lat$calibration)
fit_diagnostic_state <- .Random.seed
set.seed(442); invisible(rnorm(length(ld$clean_rt)))
check('fit_diagnostic_RNG_one_normal_per_retained_correct',identical(fit_diagnostic_state,.Random.seed))
check('verified_calibration_reimport',identical(imported$model$subject_effects,lat$subject_effects)&&
 identical(imported$model$k,lat$k)&&identical(imported$model$t0,lat$t0))
find_assignment <- function(x,name) {
 if(missing(x))return(NULL)
 if(is.call(x)&&identical(x[[1]],as.name('<-'))&&identical(x[[2]],as.name(name)))return(x[[3]])
 if(is.recursive(x))for(y in as.list(x)) {a<-find_assignment(y,name);if(!is.null(a))return(a)}
 NULL
}
driver_expression <- find_assignment(parse('workflows/simulation/paper_rerun_wald/run.R'),'calibration_record')
driver_env <- list2env(list(get_profile_label=function(x)'diagnostic',config_path='diagnostic',
 expected_lkt_version=lkt_release[['lkt_version']],run_provenance_id='diagnostic',
 real_model_spec=fit$model_specification,kt_model_spec=NULL,real_model=fit,kt_model=list(coefs=NULL)),parent=globalenv())
driver_error <- tryCatch({eval(driver_expression,driver_env);NA_character_},error=function(e)conditionMessage(e))
writeLines(driver_error,file.path(out,'paper-driver-expression.txt'))
check('paper_driver_calibration_record_valid',is.na(driver_error))
# Independent inverse-Gaussian CDF and bracketed root solver, expressed in
# conventional mean/shape coordinates; does not call LKT's transform/inverter.
# The ordinary-probability CDF is also checked by density quadrature below.
reference <- function(z,k=lat$k,lambda0=lat$lambda0,t0=lat$t0) {
  mu <- lambda0; shape <- k*lambda0; target <- pnorm(-abs(z),log.p=TRUE)
  logtail <- function(logx) {
    x <- exp(logx); a <- sqrt(shape/x)*(x/mu-1); b <- -sqrt(shape/x)*(x/mu+1)
    second <- 2*shape/mu+pnorm(b,log.p=TRUE)
    if(z<=0) {
      first <- pnorm(a,log.p=TRUE); hi <- max(first,second)
      hi+log(exp(first-hi)+exp(second-hi))
    } else {
      first <- pnorm(a,lower.tail=FALSE,log.p=TRUE)
      first+log(-expm1(second-first))
    }
  }
  t0+exp(uniroot(function(x)logtail(x)-target,log(mu)+c(-15,15),tol=1e-12)$root)
}
cases <- rbindlist(list(
  data.table(case='probability',p=c(1e-5,1e-4,.1,.5,.9,.9999,.99999),u=0,e=0,t0=lat$t0),
  data.table(case='student',p=.5,u=c(-2,0,2),e=0,t0=lat$t0),
  data.table(case='noise',p=.5,u=0,e=c(-8,-2,0,2,8,40),t0=lat$t0),
  data.table(case='shift',p=.5,u=0,e=0,t0=c(0,1.5))))
cases[,z:=-lat$beta1*qlogis(pmin(p,.9999))+u+e]
cases[,actual:=mapply(internal('wald_quantile_latency'),p,u,e,MoreArgs=list(beta1=lat$beta1,k=lat$k,lambda0=lat$lambda0),t0=t0)]
cases[,reference:=mapply(reference,z,t0=t0)]
cases[,`:=`(residual=actual-reference,capped=pmin(actual,lat$max_rt),cap=lat$max_rt)]
fwrite(cases,file.path(out,'controlled.csv'))
check('controlled_max_relative_error',max(abs(cases$residual)/cases$reference))
pdf <- function(x) sqrt(lat$k*lat$lambda0/(2*pi*x^3))*exp(-lat$k*(x-lat$lambda0)^2/(2*lat$lambda0*x))
quad_errors <- sapply(c(-2,-1,0,1,2),function(z) integrate(pdf,0,reference(z)-lat$t0,rel.tol=1e-10)$value-pnorm(z))
check('reference_density_quadrature_error',max(abs(quad_errors)))
check('activation_decreases_RT',all(diff(cases[case=='probability',actual])<=0))
check('student_increases_RT',all(diff(cases[case=='student',actual])>0))
check('noise_increases_RT',all(diff(cases[case=='noise',actual])>0))
set.seed(701); a <- lat$predict(c(.2,.5,.8),c(-1,0,1),TRUE,return_components=TRUE); state <- .Random.seed
set.seed(701); e <- rnorm(3);check('vector_one_normal_each',identical(e,a$trial_noise)&&identical(state,.Random.seed))
set.seed(701); b <- lat$predict(c(.2,.5,.8),c(-1,0,1),TRUE)
check('components_identical_RT_RNG',identical(a$rt,b)&&identical(state,.Random.seed))
before <- .Random.seed; lat$predict(c(.2,.5,.8));check('expected_latency_no_RNG',identical(before,.Random.seed))
check('expected_truncate_flag_ignored',identical(lat$predict(.1,3,FALSE,TRUE),lat$predict(.1,3,FALSE,FALSE)))
check('expected_latency_can_exceed_cap',lat$predict(.1,3,FALSE,TRUE)>lat$max_rt)
before <- .Random.seed; failed <- calculate_model_latency(fit,0,FALSE,student_latency_effect=2,return_details=TRUE)
check('failure_no_RNG',identical(before,.Random.seed)&&is.na(failed$Latency_Trial_Noise))
set.seed(803); capped <- calculate_model_latency(fit,0,TRUE,student_latency_effect=10,return_details=TRUE)
check('actual_success_cap_activated',capped$latency==lat$max_rt&&reference(capped$Latency_Latent_Score)>lat$max_rt)
set.seed(805); ordinary_adapter <- calculate_model_latency(fit,0,TRUE,student_latency_effect=.3,return_details=TRUE); adapter_rng <- .Random.seed
set.seed(805); role_adapter <- pf_wald_latency(lat,0,TRUE,.3,TRUE)
check('role_latency_adapter_equal',identical(ordinary_adapter,role_adapter)&&identical(adapter_rng,.Random.seed))
invalid <- lapply(list(NULL,NA_real_,-1,Inf),function(v) {
  bad <- fit;bad$model_specification[[2]]$failure_latency <- v
  tryCatch(calculate_model_latency(bad,0,FALSE,student_latency_effect=0),error=function(e)conditionMessage(e))
})
saveRDS(invalid,file.path(out,'invalid-failure-results.rds'));print(invalid)
check('invalid_failure_rejected',all(vapply(invalid,is.character,logical(1))))
check('fitted_shift_below_cap',lat$t0<lat$max_rt)
cal <- list(schema=1L,family='Wald',centering='equal_student',
 parameters=list(k=2,lambda0=8,beta=.23,tau=.8,t0=20,nll=0,weight=.02,uniform_upper=120),
 rt=c(2,4,8),p=c(.3,.5,.7),subject_id=rep('synthetic',3),subject_effects=c(synthetic=0),
 subject_effect_sd=c(synthetic=.1),n_free_parameters=4L,source='deliberately invalid diagnostic fixture')
badcap <- try(internal('import_wald_calibration')(cal,cal$rt,cal$p,cal$subject_id),silent=TRUE)
check('import_rejects_shift_above_cap',inherits(badcap,'try-error'))
if(!inherits(badcap,'try-error'))check('invalid_import_generates_below_shift',badcap$predict(.5,0,TRUE)<badcap$t0)
recycled <- suppressWarnings(try(lat$predict(c(.2,.5,.8),c(-1,1),TRUE),silent=TRUE))
check('predict_rejects_misaligned_effect_vector',inherits(recycled,'try-error'))
check('rejects_zero_one_probabilities',all(vapply(c(0,1),function(p)
 inherits(try(lat$predict(p,0,TRUE),silent=TRUE),'try-error'),logical(1))))
# Synthetic mask check isolates failure filtering from correct-response IQR.
fixture <- data.table(CF..ansbin.=c(rep(1,5),0,0,0),Duration..sec.=c(1,2,3,4,100,2,100,900),
  pred=.5,Anon.Student.Id='synthetic',fold=c(rep(1,7),2))
check('failure_mask_no_correct_IQR',internal('lkt_failure_latency')(fixture,1)==51)
check('missing_failure_fit_errors',inherits(try(internal('lkt_failure_latency')(fixture[CF..ansbin.==1]),silent=TRUE),'try-error'))
# Finite differences of the shifted transformation and marginal likelihood.
transform <- internal('wald_latency_transform'); par <- c(log(2),log(8),1.5); rt <- c(2,5,10,25)
tr <- transform(rt,exp(par[1]),exp(par[2]),par[3]); h <- 1e-5
grad <- sapply(1:3,function(j) {lo<-hi<-par;lo[j]<-lo[j]-h;hi[j]<-hi[j]+h
 (transform(rt,exp(hi[1]),exp(hi[2]),hi[3])$z-transform(rt,exp(lo[1]),exp(lo[2]),lo[3])$z)/(2*h)})
check('transform_derivative_max_error',max(abs(grad-tr$dz)))
ll <- internal('wald_latency_likelihood')(c(2,3,5,8,12,20),c(.3,.5,.7,.4,.6,.8),rep(c('a','b'),each=3))
theta <- c(log(2),.23,log(.8),qlogis(1.5/20)); v <- ll$centered(theta)
g <- sapply(1:4,function(j){lo<-hi<-theta;lo[j]<-lo[j]-h;hi[j]<-hi[j]+h;(ll$centered(hi)$nll-ll$centered(lo)$nll)/(2*h)})
check('likelihood_gradient_max_error',max(abs(g-v$gradient)))
support <- transform(c(.5,1.5,2),2,8,1.5)
check('engaged_support_zero_below_shift',all(is.infinite(support$log_jacobian[1:2])&support$log_jacobian[1:2]<0))
full_ll <- internal('wald_latency_likelihood')(ld$clean_rt,ld$clean_pred,ld$clean_subject_id)
full_theta <- c(log(lat$k),lat$beta1,log(lat$sigma_subject))
full_score <- full_ll$centered(full_theta,t0_fixed=lat$t0)
full_fd <- sapply(1:3,function(j) {lo<-hi<-full_theta;lo[j]<-lo[j]-1e-5;hi[j]<-hi[j]+1e-5
 (full_ll$centered(hi,t0_fixed=lat$t0)$nll-full_ll$centered(lo,t0_fixed=lat$t0)$nll)/2e-5})
fwrite(data.table(coordinate=c('log_k','beta','log_tau'),analytic=full_score$gradient,finite_difference=full_fd),file.path(out,'fitted-gradient.csv'))
check('fitted_gradient_finite_difference_error',max(abs(full_fd-full_score$gradient)))
ids <- sprintf('D%02d',1:3); items <- 1:6; map <- setNames(as.list(items),items)
trimstats <- function(v) {q<-quantile(v,c(.025,.975));v<-v[v>=q[1]&v<=q[2]];c(mean(v),sd(v))}
spec <- fit$model_specification[[1]]
source('workflows/simulation/paper_rerun_wald/single_subject_config_ppes.R')
val <- copy(fit$newdata)
tracer <- build_kt_model('fixed')
saveRDS(tracer,file.path(out,'tracer-fixed.rds'))
ab <- trimstats(spec[feature=='intercept'&component=='Anon.Student.Id',coefficient])
it <- trimstats(spec[feature=='intercept'&component=='KC..Default.',coefficient])
cohort <- build_cohort_traits(ids,items,ab[1],ab[2],it[1],it[2],lat$sigma_subject,25)
expanded <- build_cohort_traits(c('D04',rev(ids)),7:1,ab[1],ab[2],it[1],it[2],lat$sigma_subject,25)
check('cohort_order_and_expansion_stable',identical(as.data.frame(cohort$students),
 as.data.frame(expanded$students[match(ids,Anon.Student.Id)])))
schedule <- list(list(type='test',items=items,order='sequential'),
 list(type='practice',items=items,duration=60),list(type='retention',duration=86400),
 list(type='practice',items=items,duration=60),list(type='retention',duration=604800),
 list(type='test',items=items,order='sequential'))
run <- function(directory,cluster=NULL) iterate_students(3,list(type='OET',param=.7,tie_policy='first',practice_stop_policy='time_budget'),
 items,data.table(Anon.Student.Id=ids),schedule,tracer$model_specification,fit$model_specification,map,
 retain_probabilities=FALSE,progressive_prob_output_file=file.path(directory,'probabilities.csv'),
 cohort_traits=cohort,condition_id='wald_diagnostic_v1',trial_seed=25,student_output_dir=directory,
 parallel=!is.null(cluster),parallel_cluster=cluster)
set.seed(991); ordinary <- run(file.path(execution,'ordinary')); ordinary_rng <- .Random.seed
collect <- function(x)rbindlist(trial_source_map(x$data,identity),fill=TRUE)
plain <- collect(ordinary)
bound_identity <- readRDS(file.path(execution,'ordinary','identity.rds'))$identity
bound_real <- list(as.data.table(bound_identity$real$coefficients),lat)
modified_tracer <- copy(tracer$model_specification);modified_tracer[[1]][,coefficient:=coefficient+2]
fixed_trial <- function(kt)simulate_row_student(kt,bound_real,initialize_student(data.table(Anon.Student.Id=ids[1])),
 list(type='fixed_item',item_id=1),0,ids[1],items,map,items,
 student_latency_effect=cohort$students$Latency_Subject_Effect[1])$updated_student_data
set.seed(714); original_trial <- fixed_trial(tracer$model_specification);original_state <- .Random.seed
set.seed(714); altered_trial <- fixed_trial(modified_tracer)
check('latency_uses_ground_truth_not_tracer',identical(original_state,.Random.seed)&&
 identical(original_trial[,.(Real_logit,Real_probability,CF..ansbin.,Duration..sec.)],
 altered_trial[,.(Real_logit,Real_probability,CF..ansbin.,Duration..sec.)]))
audit <- new.env();audit$rows <- list();audit$latency <- list();audit$response_rng <- list()
trace('calculate_model_latency',tracer=quote(.audit_before <- .Random.seed),
 exit=quote(audit$latency[[length(audit$latency)+1L]] <- list(before=.audit_before,after=.Random.seed,correct=correct)),print=FALSE)
trace('simulate_row_student',tracer=quote({.audit_n<-nrow(student_data);.audit_old<-copy(student_data);.audit_response_before<-.Random.seed}),
 exit=quote({audit$response_rng[[length(audit$response_rng)+1L]] <- list(before=.audit_response_before,p=real_prob,correct=correct)
 audit$rows[[length(audit$rows)+1L]] <- data.table(student=as.character(student_id),
 trial=.audit_n+1L,session=session_id,item=practiced_item,prior_student=.audit_n,
 prior_item=sum(.audit_old$Item.Id==practiced_item),
 prior_success=sum(.audit_old$Item.Id==practiced_item & .audit_old$CF..ansbin.==1),
 prior_failure=sum(.audit_old$Item.Id==practiced_item & .audit_old$CF..ansbin.==0),
 raw_logit=real_res_item$logit,raw_probability=real_res_item$probability)}),print=FALSE)
set.seed(991); logged <- run(file.path(execution,'instrumented'));logged_rng <- .Random.seed
untrace('calculate_model_latency');untrace('simulate_row_student')
d <- collect(logged)
check('instrumented_identical_trials',identical(as.data.frame(plain),as.data.frame(d)))
check('instrumented_identical_final_RNG',identical(ordinary_rng,logged_rng))
# Test every observed latency call against precisely zero or one scalar normal.
saved <- .Random.seed
rng_ok <- vapply(audit$latency,function(x){assign('.Random.seed',x$before,.GlobalEnv);if(x$correct)rnorm(1);identical(.Random.seed,x$after)},logical(1))
assign('.Random.seed',saved,.GlobalEnv)
check('every_latency_RNG_accounted',all(rng_ok))
response_ok <- vapply(seq_along(audit$response_rng),function(i) {
 x<-audit$response_rng[[i]];assign('.Random.seed',x$before,.GlobalEnv)
 y<-rbinom(1,1,x$p)
 identical(y,x$correct)&&identical(.Random.seed,audit$latency[[i]]$before)
},logical(1))
assign('.Random.seed',saved,.GlobalEnv)
check('every_response_Bernoulli_accounted',all(response_ok))
trace_table <- cbind(rbindlist(audit$rows),d[,.(effective_probability=Real_probability,correct=CF..ansbin.,
 effect=Latency_Subject_Effect,noise=Latency_Trial_Noise,z=Latency_Latent_Score,u=Latency_Quantile_U,
 duration=Duration..sec.,review=CF..Review.Latency.,clock_before=Time,clock_after=Time+Duration..sec.,session_type=Session.Type)])
trace_table[,`:=`(speed_z=cohort$students$Latency.Z[match(student,ids)],speed_sd=lat$sigma_subject,
 k=lat$k,lambda0=lat$lambda0,t0=lat$t0,cap=lat$max_rt,failure_constant=lat$failure_latency,
 latency_noise_count=as.integer(correct),rng_verified=rng_ok,speed_effect_applied=as.logical(correct))]
trace_table[correct==0,`:=`(k=NA_real_,lambda0=NA_real_,t0=NA_real_,cap=NA_real_)]
trace_table[,`:=`(activation=ifelse(correct==1,-lat$beta1*qlogis(effective_probability),NA_real_),
 unshifted=NA_real_,uncapped=NA_real_,reference_uncapped=NA_real_,cap_active=NA)]
# Deterministically recover the pre-cap value from the captured score. This
# postprocessing never draws another variate or calls the stochastic predictor.
trace_table[correct==1,uncapped:=vapply(z,internal('wald_latency_from_score'),numeric(1),
 k=lat$k,lambda0=lat$lambda0,t0=lat$t0)]
trace_table[correct==1,reference_uncapped:=vapply(z,reference,numeric(1))]
trace_table[correct==1,`:=`(unshifted=uncapped-t0,cap_active=uncapped>cap)]
trace_table[,reconstructed:=ifelse(correct==1,pmin(reference_uncapped,cap),failure_constant)]
trace_table[,residual:=duration-reconstructed]
trace_table[,budget_remaining:=if(session_type[1]=='practice')60-(clock_after-min(clock_before)) else NA_real_,by=.(student,session)]
fwrite(trace_table,file.path(out,'trace.csv'),na='NA');saveRDS(trace_table,file.path(out,'trace.rds'))
saveRDS(audit$latency,file.path(out,'latency-rng-states.rds'))
check('trace_max_reconstruction_error',max(abs(trace_table$residual)))
check('production_components_identical_final',all(trace_table[correct==1,pmin(uncapped,cap)==duration]))
check('student_effect_stable',all(trace_table$effect==trace_table$speed_z*trace_table$speed_sd))
check('both_natural_response_branches',all(c(0,1)%in%trace_table$correct))
check('diagnostic_probability_bound_activations',sum(trace_table$raw_probability<.0001|trace_table$raw_probability>.9999))
check('success_above_shift',all(trace_table[correct==1,duration]>lat$t0))
check('failure_constant_and_no_noise',all(trace_table[correct==0,duration]==lat$failure_latency)&all(is.na(trace_table[correct==0,noise])))
clock_errors <- trace_table[, {
 gap <- clock_before[-1]-head(clock_after,-1)
 target <- ifelse(session[-1]==head(session,-1),0,
   ifelse(session[-1]==4,86400,ifelse(session[-1]==6,604800,0)))
 list(max_error=max(abs(gap-target)))
},by=student]
check('clock_and_retention_max_error',max(clock_errors$max_error))
check('duration_response_review_fields_agree',all(d$Duration..sec.==d$CF..Response.Time.)&&
 all(d$Duration..sec.==d$CF..End.Latency.)&&all(d$Duration..sec.==d$CF..Response.Latency.)&&
 all(d[CF..ansbin.==0,CF..Review.Latency.]==0)&&all(is.na(d[CF..ansbin.==1,CF..Review.Latency.])))
set.seed(991); replay <- run(file.path(execution,'replay'))
check('fixed_seed_replay',identical(as.data.frame(plain),as.data.frame(collect(replay)))&&identical(ordinary_rng,.Random.seed))
set.seed(991); resumed <- run(file.path(execution,'ordinary'))
check('completed_checkpoint_resume',identical(as.data.frame(plain),as.data.frame(collect(resumed)))&&identical(ordinary_rng,.Random.seed))
partial <- file.path(execution,'partial-resume');dir.create(partial)
stopifnot(all(file.copy(file.path(execution,'ordinary',c('identity.rds','student_000001.rds')),partial)))
set.seed(991); partial_result <- run(partial)
check('partial_checkpoint_resume',identical(as.data.frame(plain),as.data.frame(collect(partial_result)))&&identical(ordinary_rng,.Random.seed))
set.seed(991); expected_next <- rnorm(1);set.seed(991);invisible(run(file.path(execution,'ordinary')))
check('resume_next_draw_preserved',identical(expected_next,rnorm(1)))
worker_status <- system2(file.path(R.home('bin'),'Rscript.exe'),c(shQuote(file.path(root,'tests/diagnostics/wald-paper-review.R')),'worker',shQuote(out)),
 stdout=file.path(out,'fresh-worker-process.log'),stderr=file.path(out,'fresh-worker-process.log'))
stopifnot(worker_status==0L)
worker <- readRDS(file.path(out,'fresh-worker.rds'))
set.seed(811); expected <- lat$predict(c(.2,.5,.8),c(-1,0,1),TRUE,return_components=TRUE)
check('fresh_worker_saved_closure',identical(expected,worker$value)&&identical(.Random.seed,worker$rng))
cluster <- parallel::makePSOCKcluster(2,outfile=file.path(out,'parallel-workers.log'))
parallel::clusterEvalQ(cluster,{
 root <- 'C:/dev/LKT';source(file.path(root,'scripts/load-current-lkt.R'))
 setwd('C:/dev/PolicyFactory2');source('workflows/simulation/shared/lktsimulator_parallel.R');NULL
})
set.seed(991); parallel_result <- tryCatch(run(file.path(execution,'parallel'),cluster),finally=parallel::stopCluster(cluster))
check('parallel_worker_assignment',identical(as.data.frame(plain),as.data.frame(collect(parallel_result)))&&identical(ordinary_rng,.Random.seed))
saveRDS(checks,file.path(out,'checks.rds'));fwrite(data.table(check=names(checks),value=unlist(checks)),file.path(out,'checks.csv'))
print(lat[c('k','lambda0','beta1','sigma_subject','t0','max_rt','failure_latency','optimizer')])
print(trace_table[c(which(trace_table$correct==1)[1],which(trace_table$correct==0)[1])])
report <- c('# Wald latency review and diagnostic — 2026-11-15',
 '', '## Verdict',
 'The executed successful-response path uses one standard-normal latency variate and deterministic normal-score Wald inversion. It is not conventional inverse-Gaussian random sampling. See checks.csv for measured verification results. The paper sweep itself was not launched and its ordinary calibration-record expression has an unresolved variable (P1). Additional P2 contracts below prevent an unqualified overall pass.',
 '', '## Executed identity and calibration',
 paste('Central release:',lkt_release[['lkt_version']],'; archive SHA256:',lkt_release[['lkt_source_sha256']]),
 'The shared release loader selected and verified the installed namespace. source-identity.csv compares six reviewed source files with the installed manifest; the archive checksum was also verified. Unrelated working-tree edits were preserved. No installed package or release definition was changed.',
 'Input: SHA256 00497d1b92ffc569f5e64d1d4e2d08962f3e1803a4a6e8ba74e19bae95c481a3; 57,328 V3 responses, 180 participants, 199 items. The confirmed historical fits were accuracy-only; no matching verified V3 Wald fit was found in the inspected output locations.',
 'Refitted: ground-truth linear coefficients at the four fixed PPES parameters, followed by one secondary Wald fit with its default zero and positive shift starts. For the diagnostic scheduler, refitted PPES tracer linear coefficients at its fixed confirmed nonlinear parameters. No nonlinear accuracy optimization, full sweep, remote job, deployment, or historical result modification occurred.',
 paste('Fixed ground-truth nonlinear parameters:',paste(format(paper_real_parameters,digits=17),collapse=', ')),
 sprintf('Correct RTs before IQR: %d; retained: %d; students retained: %d.',length(ld$correct_rt),length(ld$clean_rt),lat$n_subjects),
 '', '```',capture.output(print(lat[c('k','lambda0','beta1','sigma_subject','sigma_trial','t0','max_rt','failure_latency','optimizer')])),'```',
 '', '## Mathematics and F0',
 'F0 is t0, an additive minimum-time shift in seconds. For effective ground-truth probability p, fixed student effect u=tau*s with s~N(0,1) drawn once per identity, and epsilon~N(0,1) drawn once for each correct response:',
 '`z = -beta*logit(p) + u + epsilon; RT_uncapped = t0 + Q_IG(Phi(z); mean=lambda0, shape=k*lambda0); RT = min(RT_uncapped, max_rt)`.',
 'The unshifted density is sqrt(k*lambda0/(2*pi*x^3))*exp[-k*(x-lambda0)^2/(2*lambda0*x)] for x>0. Fitting transforms x=RT-t0 and z=Phi^-1(F_IG(x)). Its engaged conditional density is phi(z+beta*logit(p)-u)*f_IG(x)/phi(z). The shared student effect is integrated before taking each student log likelihood. lambda0 is solved so the equally weighted mean posterior student effect is zero. tau is the between-student SD; trial SD is exactly one; subject_effect_sd is posterior uncertainty, not a second population multiplier.',
 'For RT<=t0, engaged log density is -Inf. Positive observations there can be represented by the fit-only 0.02/120 uniform density. Simulation is engaged-only. Generation adds t0 once and never scales it by noise or speed; empirical failure duration is not shifted again.',
 'The independent reference uses the conventional inverse-Gaussian CDF with a bracketed uniroot solver, not the production Newton inverter or production transformation. Ordinary reference quantiles are additionally checked by numerical integration of the density. Extreme z=40 is inverted using log survival probabilities even though the displayed Phi(z) rounds to one.',
 '', '## Findings',
 '**P1 — paper driver unresolved variable.** PolicyFactory2/workflows/simulation/paper_rerun_wald/run.R:355 uses final2_real_parameters when constructing calibration_record, but the current sourced definitions provide paper_real_parameters. Evaluation of the actual parsed expression fails; see paper-driver-expression.txt. An ordinary fresh paper run cannot reach simulation through this branch after fitting. No fix was applied.',
 '**P2 — invalid failure constants are not rejected by the ordinary generator.** shared/lktsimulator_parallel.R:3150 extracts failure_latency without checking scalar, finite, positive status; missing, NA, negative and infinite fixtures were tested. model_roles.R:416 has the same direct-helper gap, although pf_validate_source validates role construction. Practice may reject an invalid clock later, but that is not explicit calibration validation and the test loop lacks that same duration guard.',
 '**P2 — imported shift may exceed the cap.** R/lkt-latency-quantile.R:267 checks t0>=0 but not t0<max(rt); predict at :296 caps the shifted RT. A deliberately invalid synthetic import with t0=20 and max_rt=8 is accepted and can emit RT=8<t0. Ordinary direct fitting constrains the shift below maximum retained RT; this defect concerns imported calibration validation.',
 '**P2 — predictor recycles misaligned student effects.** R/lkt-latency-quantile.R:287-295 does not validate subject_effect length before adding it to the score. The diagnostic passes three probabilities and two effects. The lower-level wald_quantile_latency helper does validate lengths, but the saved stochastic closure bypasses it. Valid vector inputs still consume exactly one normal per probability.',
 '**P2 — expected-latency cap inconsistency.** R/lkt-latency-quantile.R:289-291 returns an uncapped quadrature expectation regardless of truncate. A high-effect example exceeds max_rt even with truncate=TRUE; sampled responses are capped. No individual response uses this deterministic branch in the inspected simulator.',
 '**Numerical fit qualification.** All four starts returned convergence code zero under the production relative-objective stopping tolerance. The selected boundary solution has maximum absolute gradient 0.9020227 (about 0.00002634 per retained response). Independent central finite differences agree with the analytic score to 8.94e-7; fitted-gradient.csv gives all three coordinates. This supports the derivative implementation but does not establish exact stationarity or a unique global optimum. No further optimization was performed.',
 '**Model-contract difference — lower probability bound.** LibLinear fits clamp predictions to [0.00001,0.99999]; Wald then caps only the upper bound at 0.9999. The simulator bounds the ground-truth response and latency input to [0.0001,0.9999]. Thus probabilities below 0.0001 have weaker positive latency activation in simulation than the fitting domain permits. For beta>0, moving from 0.00001 to 0.0001 reduces the score by approximately 2.302675*beta. This is a scientific behavior difference to review, not silently repair.',
 '', '## RNG, history, failure duration and clock',
 'The selected-item ground-truth prediction is computed from prior history before rbinom and before appending the current response. Scheduling-model probabilities select the item but do not generate its latency. The synthetic feature row has no observed current outcome; cor/icor are counted from previous responses. No random practice effect exists.',
 'Ability, latency speed and item difficulty use separate identity seed namespaces. Latency.Z is multiplied by sigma_subject once in build_cohort_traits, then passed unchanged into every student trial. Worker scheduling and cohort expansion do not define these traits. There is no configured ability/speed covariance.',
 'Every diagnostic trial checks the separate Bernoulli state transition and the latency state transition. Correct responses must match one rnorm(1); failures must leave latency RNG unchanged. Logged and ordinary runs are compared on all trial columns and final RNG state. The fitted-latency correlation diagnostic in R/lkt-latency.R:64 consumes one normal per retained correct observation; independent per-student streams prevent it from shifting simulation responses.',
 'Failure calibration is the mean of positive finite incorrect-response durations in the fitting mask, before any correct-response IQR screen. V3 preparation marks unavailable durations and those outside (0,120] missing. The simulator treats this timestamp-derived total as including review, records failure review latency as zero, and makes total duration and response latency fields equal the constant. Correct review latency is NA. Each trial advances the clock by its full duration. The export has no separate review-latency column, so this review verifies the code treatment and absence of double addition, not an independent decomposition of real response versus review time.',
 'Practice starts another trial only while the clock is below its deadline, then completes that trial. Negative remaining budget in trace.csv is the deliberate final-trial overrun, not a second review charge. Retention is added to the preceding session end. This diagnostic uses pretest, 60-second practice, one-day retention, 60-second practice, seven-day retention, and posttest; three anonymized students and six items.',
 'Checkpoints commit whole students. The tests cover fixed-seed replay, completed reuse, partial-cohort resumption with one committed student, preservation of the caller next draw, fresh-process saved predictor, and two-worker execution. They do not claim unsupported mid-student checkpoint continuation.',
 '', '## Measured checks', '', '```',capture.output(print(data.table(check=names(checks),value=unlist(checks)))),'```',
 sprintf('Integrated trace: %d naturally sampled responses, %d correct and %d incorrect; %d successful responses hit the cap. Maximum final-trial practice overrun: %.9f seconds.',
 nrow(trace_table),sum(trace_table$correct==1),sum(trace_table$correct==0),sum(trace_table$cap_active,na.rm=TRUE),
 max(0,-min(trace_table$budget_remaining,na.rm=TRUE))),
 '', '## Actual successful and failed examples', '', '```',
 capture.output(print(as.data.frame(trace_table[c(which(trace_table$correct==1)[1],which(trace_table$correct==0)[1])]))),'```',
 '', '## Artifacts and limits',
 'trace.csv contains all generating components, independent reconstruction, prior counts, duration, clocks and remaining budget. controlled.csv contains one-input-at-a-time and extreme-tail tests. latency-rng-states.rds retains per-call RNG evidence. checks.csv and diagnose.log give measured outcomes. The latest-execution.txt directory contains ordinary, instrumented, replay, resumed and parallel checkpoints.',
 'Reproducible entry point: C:/dev/LKT/tests/diagnostics/wald-paper-review.R. See its adjacent README.md. No scientific production source was changed. Full sweep execution, remote job state, all 864 conditions, every probability/parameter extreme, and mid-student checkpointing were not verified. Optimizer convergence and numerical gradient evidence are reported, not proof of a unique global likelihood maximum.')
writeLines(report,file.path(out,'report.md'))
