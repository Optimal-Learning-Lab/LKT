# Sequence units own their state layout. No training or prediction loop may
# inspect a hidden-state axis; the GRU adapter is also the historical format reader.
lkt_sequence_kind <- function(s) if(is.null(s$unit))'gru' else s$unit
lkt_sequence_module_name <- function(s) if(lkt_sequence_kind(s)=='gru')'gru' else 'sequence'
lkt_sequence_module <- function(net,s) net[[lkt_sequence_module_name(s)]]
lkt_sequence_gru_settings <- function(s) {
  if(is.null(s$unit))return(s)
  if(s$unit!='gru')stop('GRU settings require a GRU unit.')
  options <- s$unit_options;drop <- s$layer_dropout
  s[c('unit','unit_options','layer_dropout')] <- NULL
  c(s,list(gru_dropout=drop),options)
}
lkt_sequence_build <- function(input_size,s) {
  if(lkt_sequence_kind(s)=='gru')lkt_gru_recurrent(input_size,lkt_sequence_gru_settings(s)) else lkt_mamba_network(input_size,s)
}
lkt_sequence_initial <- function(s,batch,like) {
  if(lkt_sequence_kind(s)=='gru')return(torch::torch_zeros(c(s$num_layers,batch,s$hidden_size),
    dtype=like$dtype,device=like$device))
  lkt_mamba_initial(s,batch,like)
}
lkt_sequence_read <- function(state,s) {
  if(lkt_sequence_kind(s)=='gru')return(state[s$num_layers,,,drop=FALSE]$permute(c(2,1,3)))
  state$read
}
# Architecture-specific batch axes are declared on leaves, so an LSTM or other
# unit can carry any number of tensors without changing these operations.
lkt_sequence_layout <- function(s) {
  if(lkt_sequence_kind(s)=='gru')return(2L)
  list(read=1L,layers=rep(list(list(conv=1L,ssm=1L)),s$num_layers))
}
lkt_sequence_state_map <- function(state,s,operation,indices=NULL,layout=lkt_sequence_layout(s)) {
  if(is.null(state))return(NULL)
  visit <- function(x,axis) {
    switch(operation,
      subset={index <- rep(list(quote(expr=)),length(x$size()));index[[axis]] <- indices
        do.call(`[`,c(list(x),index,list(drop=FALSE)))},
      detach=x$detach(),clone=x$clone(),pack=torch::as_array(x$detach()$cpu()),
      unpack=lkt_gru_tensor(x,s),stop('Unknown sequence state operation.'))
  }
  walk <- function(node,axes) {
    if(is.numeric(axes))return(visit(node,axes))
    if(length(node)!=length(axes) || !identical(names(node),names(axes)))stop('Sequence state layout changed.')
    Map(walk,node,axes)
  }
  walk(state,layout)
}
lkt_sequence_advance <- function(net,input,state,s) lkt_sequence_module(net,s)(input,state)

#' Selectable sequence memory for knowledge tracing
#'
#' @param unit Sequence architecture: gru or mamba (original Mamba-1).
#' @param ... Shared GRUModel encoder, prediction, data and training controls.
#'   Use layer_dropout instead of gru_dropout. Architecture controls belong in unit_options.
#' @param layer_dropout Dropout between sequence layers.
#' @param unit_options Named architecture settings. Mamba supports d_state,
#'   d_conv, expand, dt_rank, dt_min, dt_max, dt_init_floor, norm_epsilon,
#'   conv_bias and bias. GRU supports gru_bias, input_to_all_layers,
#'   additional_input_init and initial_model_file.
#' @return An LKTModel adapter. hidden_size is the readable memory width.
#' @export
SequenceModel <- function(unit=c('gru','mamba'),...,layer_dropout=.1,unit_options=list()) {
  unit <- match.arg(unit);common <- list(...)
  specific <- c('gru_bias','gru_dropout','input_to_all_layers','additional_input_init','initial_model_file')
  if(any(names(common) %in% c(specific,'unit','unit_options')))
    stop('Use layer_dropout and unit_options for architecture controls.',call.=FALSE)
  if(!is.list(unit_options) || (length(unit_options) &&
     (is.null(names(unit_options)) || anyDuplicated(names(unit_options)) || any(!nzchar(names(unit_options))))))
    stop('unit_options must be a named list.',call.=FALSE)
  defaults <- if(unit=='gru')list(gru_bias=TRUE,input_to_all_layers=FALSE,
    additional_input_init='random',initial_model_file=NULL) else
    list(d_state=16L,d_conv=4L,expand=2L,dt_rank='auto',dt_min=.001,dt_max=.1,
      dt_init_floor=.0001,norm_epsilon=1e-5,conv_bias=TRUE,bias=FALSE)
  lkt_gru_named(unit_options,names(defaults),'sequence unit options')
  options <- utils::modifyList(defaults,unit_options,keep.null=TRUE)
  settings <- do.call(GRUModel,c(common,list(gru_dropout=layer_dropout),if(unit=='gru')options else list()))$settings
  settings$unit <- unit;settings$unit_options <- options
  if(unit=='mamba') {
    for(n in c('d_state','d_conv','expand')) {
      v <- options[[n]]
      if(length(v)!=1L || !is.numeric(v) || !is.finite(v) || v<1 || v>.Machine$integer.max || v!=floor(v))stop('Invalid Mamba ',n,'.')
      options[[n]] <- as.integer(v)
    }
    if(!identical(options$dt_rank,'auto') && (length(options$dt_rank)!=1L ||
       !is.numeric(options$dt_rank) || !is.finite(options$dt_rank) || options$dt_rank<1 ||
       options$dt_rank>.Machine$integer.max || options$dt_rank!=floor(options$dt_rank)))stop('Invalid Mamba dt_rank.')
    for(n in c('dt_min','dt_max','dt_init_floor','norm_epsilon'))
      if(length(options[[n]])!=1L || !is.numeric(options[[n]]) || !is.finite(options[[n]]) || options[[n]]<=0)stop('Invalid Mamba ',n,'.')
    if(options$dt_min>options$dt_max || options$dt_init_floor>options$dt_max)stop('Invalid Mamba delta initialization range.')
    for(n in c('bias','conv_bias'))if(!is.logical(options[[n]]) || length(options[[n]])!=1L || is.na(options[[n]]))stop('Invalid Mamba ',n,'.')
    settings$unit_options <- options
  }
  settings$layer_dropout <- settings$gru_dropout
  settings[specific] <- NULL
  adapter <- LKTCustomModel('Sequence',function(request,verbose=FALSE)lkt_fit_gru(request,settings,verbose))
  adapter$settings <- settings
  adapter
}

lkt_sequence_settings <- function(s) {
  if(is.null(s$unit))return(lkt_gru_settings(s))
  do.call(SequenceModel,lkt_sequence_arguments(s))$settings
}
lkt_sequence_arguments <- function(s) {
  unit <- s$unit;options <- s$unit_options;drop <- s$layer_dropout
  s[c('unit','unit_options','layer_dropout')] <- NULL
  c(list(unit=unit,unit_options=options,layer_dropout=drop),s)
}

# Explicit translation for historical recipes; new saved settings have no
# duplicate architecture controls outside unit_options.
lkt_sequence_from_gru_settings <- function(s,unit) {
  controls <- c('gru_bias','input_to_all_layers','additional_input_init','initial_model_file')
  options <- if(unit=='gru')s[controls] else list()
  if(unit=='mamba' && (isTRUE(s$input_to_all_layers) || !is.null(s$initial_model_file)))
    stop('Mamba does not support GRU input expansion or warm starts.')
  drop <- s$gru_dropout;s[c(controls,'gru_dropout')] <- NULL
  do.call(SequenceModel,c(list(unit=unit,unit_options=options,layer_dropout=drop),s))$settings
}
