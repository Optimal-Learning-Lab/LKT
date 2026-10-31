lkt_gui_terms_text <- function(terms) {
  paste(utils::capture.output(utils::write.table(terms,sep='|',row.names=FALSE,quote=FALSE,na='')),collapse='\n')
}

lkt_gui_load_data <- function(dataset,data_file='') {
  if(identical(dataset,'samplelkt (built-in)')) {
    # Read from this loaded LKT namespace, not a second installation or an
    # attached workspace object. Fit preparation must not alter package data.
    return(data.table::copy(getExportedValue('LKT','samplelkt')))
  }
  if(!identical(dataset,'RDS file'))stop('Unknown dataset source.',call.=FALSE)
  if(!file.exists(data_file)||tolower(tools::file_ext(data_file))!='rds')
    stop('Select a model-ready RDS data file.',call.=FALSE)
  readRDS(data_file)
}

lkt_gui_confirm_overwrite <- function(path,action) {
  answer<-tcltk::tkmessageBox(parent=fgui::guiGet('MAIN'),title='Replace saved file?',
    type='yesno',default='no',icon='question',
    message=paste0('This file already exists:\n\n',path,
      '\n\nReplace it with the result of "',action,'"?'))
  identical(as.character(answer),'yes')
}

#' Configure LKT models with fgui
#'
#' Opening the interface does not fit, simulate, or submit anything. The action
#' field selects preview, save, or fit explicitly. Files are never overwritten.
#' Each line in the ordered terms editor is one model term. Nonlinear values
#' within a term use semicolons, and NA means estimate. Save real and tracing
#' configurations separately for the simulation configurator.
#' The dataset selector can use the bundled samplelkt dataset directly or read
#' an RDS file. The file field is used only when RDS file is selected.
#' @param config Initial LKTModelConfig, or NULL for an intercept example.
#' @param modal Wait for the window to close.
#' @param theme Soft blue or green presentation theme.
#' @param dataset Initial data source, RDS file or the built-in samplelkt.
#' @param data_file Initial RDS path, used only for the file source.
#' @param output_file Initial output filename. Blank input uses this default.
#'   Its parent directory is created when saving. Existing files require confirmation.
#' @param confirm_overwrite Function of path and action, used by an embedding
#'   application to confirm replacement. The default displays a Yes/No dialog
#'   with No selected. It is called before fitting when the target exists.
#' @return The initial fgui form state invisibly for a nonmodal window, or NULL
#'   after a modal window closes. Save actions persist the configuration.
#' @export
lkt_gui <- function(config=NULL,modal=TRUE,theme=c('blue','green'),
                    dataset=c('RDS file','samplelkt (built-in)'),data_file='',
                    output_file=file.path(getwd(),'lkt_model.rds'),
                    confirm_overwrite=lkt_gui_confirm_overwrite) {
  theme<-match.arg(theme)
  dataset<-match.arg(dataset)
  if(!is.character(output_file)||length(output_file)!=1L||is.na(output_file)||!nzchar(output_file))
    stop('Set a nonempty default output filename.')
  if(!is.function(confirm_overwrite))stop('confirm_overwrite must be a function.')
  default_output_file<-path.expand(output_file)
  if(!requireNamespace('fgui',quietly=TRUE))stop('Install the optional fgui package to open this interface.',call.=FALSE)
  if(is.null(config))config<-lkt_model_config(data.frame(component='KC..Default.',feature='intercept'),options=list(verbose=FALSE))
  lkt_config_arguments(config)
  bayesian<-new.env(parent=emptyenv());bayesian$options<-config$bayesian_options
  controller<-function(action='Preview',terms='',intercept=TRUE,backend='LibLinear',
    latency='none',folds='NA',wald_allowance=.02,dataset='RDS file',data_file='',output_file='',load_file='',
    bayesian_workflow='Participant split + prefit',training_fraction=.7,split_seed=1,
    participant_column='Anon.Student.Id',personalized='',particles=2048,particle_seed=1,
    ess_threshold=.01,ess_action='warn',store_trace='FALSE',
    glmm_prediction='conditional',glmm_unseen='error',epsilon=1e-4,cost=512,maxitv=100,factrv=1e12) {
    if(action=='Feature reference')return(paste(utils::capture.output(print(lkt_features(),row.names=FALSE)),collapse='\n'))
    if(action=='Load specification') {
      loaded<-readRDS(load_file);lkt_config_arguments(loaded)
      # A nested editor receives validated values without mutating the open form.
      lkt_gui(loaded,modal=TRUE,theme=theme,dataset=dataset,data_file=data_file,
        output_file=if(nzchar(output_file))output_file else default_output_file,confirm_overwrite=confirm_overwrite)
      return('Loaded specification editor closed.')
    }
    parsed<-utils::read.table(text=terms,sep='|',header=TRUE,quote='',comment.char='',
      colClasses='character',check.names=FALSE,fill=FALSE,blank.lines.skip=TRUE,na.strings=character())
    folds<-as.character(folds)
    f<-if(trimws(folds)=='NA')NA_real_ else as.numeric(strsplit(folds,',',fixed=TRUE)[[1L]])
    split_mode<-backend=='OnlineBayesian'&&bayesian_workflow=='Participant split + prefit'
    online_options<-bayesian$options
    if(backend=='OnlineBayesian') {
      online_options<-utils::modifyList(if(is.null(online_options))list() else online_options,
        list(subject_col=participant_column,n_particles=as.numeric(particles),seed=as.numeric(particle_seed),
          ess_threshold=as.numeric(ess_threshold),ess_action=ess_action,store_trace=as.character(store_trace)%in%c('TRUE','1')))
      online_options$beta_alpha_terms<-if(nzchar(trimws(personalized)))trimws(strsplit(personalized,',',fixed=TRUE)[[1L]]) else NULL
    }
    construct<-function(terms=parsed)lkt_model_config(terms,as.character(intercept)%in%c('TRUE','1'),backend,latency,f,
      options=utils::modifyList(config$options,c(list(epsilon=as.numeric(epsilon),cost=as.numeric(cost),
        maxitv=as.numeric(maxitv),factrv=as.numeric(factrv)),
        if(latency=='Wald')list(wald_contamination_weight=as.numeric(wald_allowance)) else list())),
      glmm_prediction=glmm_prediction,glmm_unseen=glmm_unseen,bayesian_options=online_options,
      bayesian_training=if(split_mode)list(training_fraction=as.numeric(training_fraction),split_seed=as.numeric(split_seed)) else NULL)
    if(action=='Predictor names') {
      # Explicit matrix-only inspection of shared features; no model is fitted
      # and no prior is inferred from the evaluation responses.
      cfg<-lkt_model_config(parsed,as.character(intercept)%in%c('TRUE','1'),
        backend='LibLinear',latency='none',options=list(verbose=FALSE))
      if(any(!is.finite(cfg$fixedpars))||any(cfg$fixedpars>=1&cfg$fixedpars%%1==0))
        stop('Fix all nonlinear feature parameters before inspecting Bayesian predictors.')
      args<-lkt_config_arguments(cfg);args$nosolve<-TRUE
      matrix<-do.call(LKT,c(list(data=data.table::copy(data.table::as.data.table(lkt_gui_load_data(dataset,data_file)))),args))
      return(paste(c('Coefficient names (feature construction only; no fit):',matrix$lassodata[[1L]],
        '\nUse an explicit prior mean for every coefficient and covariance for the personalized coefficients.'),collapse='\n'))
    }
    if(action=='Add term') {
      selection<-new.env(parent=emptyenv());selection$value<-NULL
      term_form<-function(component='KC..Default.',feature='logsuc',effect='Shared coefficient',
          fixed='',start='',interaction='',connector='+') {
        selection$value<-list(component=component,feature=feature,effect=effect,fixed=fixed,start=start,
          interaction=interaction,connector=connector)
        tcltk::tkdestroy(fgui::guiGet('MAIN'));NULL
      }
      lkt_form_gui(term_form,title='Add model term',modal=TRUE,theme=theme,
        choices=list(feature=lkt_features()$feature,effect=c('Shared coefficient','Separate fixed effects','Random intercept'),connector=c('+',':','*')),
        labels=list(fixed='Fixed parameters (NA estimates; semicolons)',start='Starting parameters (semicolons)'),
        submit_label='Add this term')
      selected<-selection$value
      if(is.null(selected))return('No term added.')
      selected<-lapply(selected,as.character)
      if(length(selected$feature)!=1L)stop('Select exactly one feature.')
      selected$feature<-paste0(selected$feature,switch(selected$effect,
        'Shared coefficient'='','Separate fixed effects'='$','Random intercept'='@'))
      term<-as.data.frame(selected[c('component','feature','fixed','start','interaction','connector')],stringsAsFactors=FALSE)
      updated<-construct(rbind(parsed,term))
      lkt_gui(updated,modal=TRUE,theme=theme,dataset=dataset,data_file=data_file,
        output_file=if(nzchar(output_file))output_file else default_output_file,confirm_overwrite=confirm_overwrite)
      return('Updated model editor closed.')
    }
    cfg<-construct()
    split_description<-if(split_mode)paste0('Participant split + hierarchical prefit: ',100*cfg$bayesian_training$training_fraction,
      '% training (rounded down to whole participants); remaining participants evaluated. Split seed: ',split_seed,
      '. Participant column: ',participant_column,'.\nCorrelated logistic mixed-model prefit estimates means and between-participant covariance. ',
      'Nonlinear feature parameters stay fixed as configured. Evaluation predicts before each response update.\n') else
      if(backend=='OnlineBayesian')'Supplied priors: NO participant split or prefit.\n' else ''
    preview<-paste('Dataset:',dataset,'\n',split_description,paste(utils::capture.output(print(lkt_config_call(cfg))),collapse='\n'))
    if(action=='Preview')return(preview)
    if(action=='Preview participant split') {
      if(!split_mode)stop('Select OnlineBayesian and Participant split + prefit first.')
      s<-lkt_bayesian_participant_split(lkt_gui_load_data(dataset,data_file),participant_column,cfg$bayesian_training)
      return(paste(split_description,paste(utils::capture.output(print(s$manifest,row.names=FALSE)),collapse='\n'),
        '\nNo prefit or evaluation has run.'))
    }
    if(!nzchar(trimws(output_file)))output_file<-default_output_file
    output_file<-path.expand(output_file)
    if(dir.exists(output_file))stop('The output filename names a directory.',call.=FALSE)
    if(file.exists(output_file)&&!isTRUE(confirm_overwrite(output_file,action)))
      return(paste('Cancelled. Existing file unchanged:',output_file))
    parent<-dirname(output_file)
    if(!dir.exists(parent)&&!dir.create(parent,recursive=TRUE,showWarnings=FALSE))
      stop('Cannot create the output directory: ',parent,call.=FALSE)
    if(action=='Save specification') {saveRDS(cfg,output_file);return(paste('Saved:',output_file,preview,sep='\n'))}
    if(action!='Run and save model')stop('Unknown action.',call.=FALSE)
    fit<-fit_lkt_config(lkt_gui_load_data(dataset,data_file),cfg);saveRDS(fit,output_file)
    if(backend=='OnlineBayesian')return(paste('Online Bayesian evaluation saved:',output_file,
      if(split_mode)paste0('\nTraining participants: ',sum(fit$participant_split$manifest$group=='training'),
        '; evaluation participants: ',sum(fit$participant_split$manifest$group=='evaluation'),
        '\nTraining rows: ',length(fit$participant_split$training_rows),'; evaluation rows: ',length(fit$participant_split$evaluation_rows),
        '\nEstimated prior means:\n',paste(utils::capture.output(print(fit$model$prior_mean)),collapse='\n'),
        '\nEstimated between-participant covariance:\n',paste(utils::capture.output(print(fit$model$prior_covariance)),collapse='\n')) else '',
      '\nSequential log score:',fit$loglike,'\nMinimum particle ESS:',min(fit$model$diagnostics$ess),
      '\nRows below ESS threshold:',sum(fit$model$diagnostics$low_ess),'\n',preview))
    paste('Fitted model saved:',output_file,'\nLog likelihood:',fit$loglike,'\n',preview)
  }
  formals(controller)$terms<-lkt_gui_terms_text(config$terms)
  formals(controller)$intercept<-config$intercept
  formals(controller)$backend<-config$backend
  formals(controller)$latency<-config$latency
  formals(controller)$folds<-paste(config$folds,collapse=',')
  formals(controller)$dataset<-dataset;formals(controller)$data_file<-data_file
  formals(controller)$output_file<-default_output_file
  formals(controller)$glmm_prediction<-config$glmm_prediction
  formals(controller)$glmm_unseen<-config$glmm_unseen
  if(config$backend=='OnlineBayesian') {
    formals(controller)$bayesian_workflow<-if(is.null(config$bayesian_training))'Supplied prior (no split)' else 'Participant split + prefit'
    if(!is.null(config$bayesian_training)) {
      formals(controller)$training_fraction<-config$bayesian_training$training_fraction
      formals(controller)$split_seed<-config$bayesian_training$split_seed
    }
    o<-config$bayesian_options
    for(n in names(list(participant_column=o$subject_col,particles=o$n_particles,particle_seed=o$seed,
      ess_threshold=o$ess_threshold,ess_action=o$ess_action,store_trace=as.character(o$store_trace)))) {
      value<-switch(n,participant_column=o$subject_col,particles=o$n_particles,particle_seed=o$seed,
        ess_threshold=o$ess_threshold,ess_action=o$ess_action,store_trace=as.character(o$store_trace))
      formals(controller)[[n]]<-value
    }
    formals(controller)$personalized<-paste(o$beta_alpha_terms,collapse=',')
  }
  for(n in intersect(c('epsilon','cost','maxitv','factrv'),names(config$options)))formals(controller)[[n]]<-config$options[[n]]
  if('wald_contamination_weight'%in%names(config$options))formals(controller)$wald_allowance<-config$options$wald_contamination_weight
  lkt_form_gui(controller,title='LKT model studio',modal=modal,theme=theme,
    subtitle=paste(attr(config,'gui_note'),
      'Configure ordered features, choose data, and save a reproducible model. Bayesian mode evaluates each response before updating its student posterior.'),
    sections=list(Model=c('terms','backend','intercept','latency'),Data=c('dataset','data_file','folds'),
      'Training and evaluation'=c('bayesian_workflow','training_fraction','split_seed','participant_column',
        'personalized','particles','particle_seed','ess_threshold','ess_action','store_trace'),
      'Fit settings'=c('epsilon','cost','maxitv','factrv','wald_allowance','glmm_prediction','glmm_unseen'),
      Files=c('output_file','load_file')),
    choices=list(action=c('Preview','Preview participant split','Save specification','Run and save model','Load specification','Feature reference','Add term','Predictor names'),
      bayesian_workflow=c('Participant split + prefit','Supplied prior (no split)'),
      ess_action=c('warn','error','ignore'),store_trace=c('FALSE','TRUE'),
      backend=c('LibLinear','GLMM','OnlineBayesian'),latency=c('none','Wald'),
      intercept=c('TRUE','FALSE'),glmm_prediction=c('conditional','fixed'),
      glmm_unseen=c('error','zero'),dataset=c('RDS file','samplelkt (built-in)')),
    files=c('data_file','load_file'),edit=list(terms=c(85,8,FALSE)),
    labels=list(terms='Ordered model terms (| columns; semicolon parameters)',backend='Accuracy model',
      intercept='Global intercept',folds='Fitting folds (Bayesian requires NA; participant split is separate)',
      bayesian_workflow='Bayesian workflow (OnlineBayesian only)',training_fraction='Training participant fraction (0 to 1)',
      split_seed='Participant split seed',participant_column='Participant ID column',
      personalized='Personalized coefficients (comma-separated; blank = all)',particles='Prior particles',
      particle_seed='Particle seed (separate from split)',ess_threshold='Low ESS threshold (fraction)',
      store_trace='Store posterior trace',
      wald_allowance='Wald fit contamination allowance',dataset='Dataset',data_file='Data file (RDS file source only)',output_file='New output file (.rds)',
      load_file='Specification to load (.rds)'),
    buttons=list('Supplied prior...'=function() {
      updated<-lkt_bayesian_gui(bayesian$options,theme)
      if(!is.null(updated))bayesian$options<-updated
      if(is.null(bayesian$options))'No supplied Bayesian prior configured.' else paste('Supplied prior ready (used only in Supplied prior mode):',
        length(bayesian$options$prior_mean),'coefficient means;',length(bayesian$options$beta_alpha_terms),'personalized coefficients;',
        bayesian$options$n_particles,'fixed particles. No model has been run.')
    }))
}
