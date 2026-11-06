# Select integer widths using parameter counts only, before any model is trained.
# Uncombined groups keep their original widths. When both pairs are summed, their
# four source vectors use one common width; the answer encoder stays at 16.
fusion_settings <- function(base,prep,parameter_count) {
  stopifnot(base$feature_encoder=='mlp',base$inference_mode=='none',
    base$encoder_output_size==16L,identical(base$encoder_hidden_sizes,32L),
    all(prep$embedding_dims==16L),length(prep$vocabularies)==2L,
    base$hidden_size==32L,base$num_layers==3L,identical(base$head_hidden_sizes,128L),
    !base$input_to_all_layers,base$retrieval_heads==1L,base$retrieval_size==32L,
    !length(base$encoder_output_sizes),!length(base$retrieval_cue_cols))
  settings <- list(baseline=base);records <- list()
  downstream <- 3L*base$hidden_size+base$head_hidden_sizes[1L]+base$retrieval_size
  rows <- sum(lengths(prep$vocabularies)+1L)
  candidates <- 16:128
  for(condition in c('temporal_sum','item_sum','both_sum','both_concat')) {
    numeric <- condition!='item_sum';category <- condition!='temporal_sum'
    final_sum <- condition=='both_sum'
    count <- function(w) {
      time <- if(numeric)w else 16L;item <- if(category)w else 16L
      input <- if(final_sum)w else (if(numeric)time else 2L*time)+(if(category)item else 2L*item)
      parameter_count+2L*33L*(time-16L)+rows*(item-16L)+downstream*(input-64L)
    }
    # Ties go to the smaller representation. No training/validation outcomes are
    # consulted, and no dummy weights or extra networks are added to fill a budget.
    width <- candidates[which.min(abs(vapply(candidates,count,numeric(1))-parameter_count))]
    s <- base;s$numeric_fusion <- if(numeric)'sum' else 'concat'
    s$categorical_fusion <- if(category)'sum' else 'concat'
    s$input_fusion <- if(final_sum)'sum' else 'concat'
    s$encoder_output_sizes <- c(time=if(numeric)width else 16L,history=if(numeric)width else 16L,response=16L)
    s$embedding_dims <- if(category)as.integer(width) else 16L
    settings[[condition]] <- do.call(GRUModel,s)$settings
    records[[condition]] <- data.frame(condition=condition,width=width,
      time_width=s$encoder_output_sizes[['time']],history_width=s$encoder_output_sizes[['history']],
      item_width=s$embedding_dims,skill_width=s$embedding_dims,
      input_width=if(final_sum)width else (if(numeric)width else 32L)+(if(category)width else 32L),
      parameter_count=count(width),difference=count(width)-parameter_count,
      relative_difference=(count(width)-parameter_count)/parameter_count)
  }
  table <- rbind(data.frame(condition='baseline',width=16L,time_width=16L,history_width=16L,
    item_width=16L,skill_width=16L,input_width=64L,parameter_count=parameter_count,difference=0,relative_difference=0),
    do.call(rbind,records))
  stopifnot(max(abs(table$relative_difference))<.005)
  list(settings=settings,counts=table)
}
