# Draw the current default GRU design from its explicitly frozen checkpoint.
# Args: ROOT [SELECTION_RDS] [OUTPUT_DIRECTORY]. All network code
# stays in the installed LKT package; the ordinary R equations below audit the figure.
args <- commandArgs(TRUE)
root <- normalizePath(if(length(args))args[1] else '.',winslash='/',mustWork=TRUE)
selection_file <- if(length(args)>=2L)args[2] else file.path(root,'docs/figures/gru-base-model/selection.rds')
selection <- readRDS(selection_file)
output <- if(length(args)>=3L)args[3] else file.path(root,'docs/figures/gru-base-model')
source(file.path(root,'scripts/load-current-lkt.R'))
hash <- function(path)digest::digest(file=path,algo='sha256')
protocol <- readRDS(file.path(root,selection$study))
data_file <- file.path(root,selection$prepared_data)
checkpoint_file <- file.path(root,selection$checkpoint)
stopifnot(hash(attr(lkt_release,'archive'))==lkt_release[['lkt_source_sha256']],
  hash(data_file)==selection$prepared_data_sha256,hash(data_file)==protocol$data_sha256,
  hash(checkpoint_file)==selection$checkpoint_sha256,
  hash(file.path(root,selection$study))==selection$study_sha256,
  requireNamespace('svglite',quietly=TRUE),torch::torch_is_installed())
checkpoint <- readRDS(checkpoint_file);fit <- checkpoint$fit
selected <- list(seed=checkpoint$task$seed,run_id=selection$run_id)
s <- do.call(GRUModel,fit$gru_settings)$settings;prep <- fit$model$preprocessing;w <- fit$model$weights
expected <- do.call(GRUModel,protocol$settings)$settings;expected$seed <- as.integer(selected$seed)
n_items <- length(prep$vocabularies[[1L]]);n_skills <- length(prep$vocabularies[[2L]])
stopifnot(identical(s,expected),identical(do.call(GRUModel,fit$gru_settings)$settings,s),
  sum(vapply(w,length,integer(1)))==fit$parameter_count,
  s$feature_encoder=='mlp',s$inference_mode=='none',s$temporal_kl_beta==0,
  s$num_layers==3L,s$hidden_size==32L,identical(s$head_hidden_sizes,128L),
  identical(s$encoder_hidden_sizes,32L),s$encoder_output_size==16L,
  s$head_activation=='relu',s$encoder_activation=='relu',s$encoder_dropout==0,
  s$input_dropout==.1,s$gru_dropout==.1,s$head_dropout==.1,
  s$retrieval_heads==1L,s$retrieval_size==32L,s$retrieval_window==100L,
  s$retrieval_fusion=='replace',s$retrieval_decay=='none',s$retrieval_share_qk,
  s$retrieval_bias,s$retrieval_dropout==0,
  identical(s$retrieval_filter_cols,c('KC..Default.','KC..Cluster.')),
  identical(s$zero_init_embedding_cols,'KC..Default.'),!length(s$retrieval_cue_cols),
  !s$input_to_all_layers,
  identical(prep$numeric_names,c('time:log1p_gap_seconds','time:log1p_gru_item_age_seconds',
    'time:log1p_gru_mean_prior_spacing_seconds','time:available_gru_mean_prior_spacing_seconds',
    'lkt:(Intercept)','lkt:logitdecKC..Default.','lkt:ppesKC..Default.','lkt:logitdecAnon.Student.Id')))
shapes <- list('embeddings.0.weight'=c(n_items+1L,16L),'embeddings.1.weight'=c(n_skills+1L,16L),
  'time_encoder.0.0.weight'=c(32L,4L),'auxiliary_encoder.0.0.weight'=c(32L,4L),
  'response_encoder.0.0.weight'=c(32L,1L),'gru.weight_ih_l1'=c(96L,80L),
  'gru.weight_ih_l2'=c(96L,32L),'gru.weight_ih_l3'=c(96L,32L),
  'head.0.weight'=c(128L,96L),'head.3.weight'=c(1L,128L),
  'retrieval.query.weight'=c(32L,96L),'retrieval.value.weight'=c(32L,32L),
  'retrieval.output.weight'=c(32L,32L))
for(n in names(shapes))stopifnot(identical(dim(w[[n]]),shapes[[n]]))
stopifnot(!'retrieval.key.weight' %in% names(w))

# Reproduce every arrow's calculation with ordinary arrays, including the shared
# Q/K affine map, strict past mask, rolling window and post-answer memory values.
affine <- function(prefix,x)as.numeric(w[[paste0(prefix,'.weight')]]%*%x+w[[paste0(prefix,'.bias')]])
encoder <- function(prefix,x)affine(paste0(prefix,'.1'),pmax(0,affine(paste0(prefix,'.0.0'),x)))
sigmoid <- function(x)1/(1+exp(-x))
T <- 103L
numeric_values <- outer(seq_len(T),1:8,function(t,j)sin(t/13+j)/3)
ids <- cbind(2L+(seq_len(T)-1L)%%144L,2L+(seq_len(T)-1L)%%36L)
# Filtering uses raw IDs, separately from the learned vectors. These synthetic
# histories include both eligible and ineligible memories and cross the bank limit.
filter_ids <- array(as.character(ids),c(1L,T,2L))
y <- as.numeric(seq_len(T)%%3L!=0L)
x <- t(vapply(seq_len(T),function(t)c(encoder('time_encoder',numeric_values[t,1:4]),
  encoder('auxiliary_encoder',numeric_values[t,5:8]),w[['embeddings.0.weight']][ids[t,1],],
  w[['embeddings.1.weight']][ids[t,2],]),numeric(64)))
response <- t(vapply(y,function(y)encoder('response_encoder',y),numeric(16)))
previous <- matrix(0,3,32);events <- matrix(0,T,96);states <- matrix(0,T,32)
manual_logits <- numeric(T);manual_attention <- matrix(0,T,T)
for(t in seq_len(T)) {
  events[t,] <- c(previous[3,],x[t,]);read <- numeric(32)
  if(t>1L) {
    j <- seq.int(max(1L,t-s$retrieval_window),t-1L)
    j <- j[ids[j,1]==ids[t,1] | ids[j,2]==ids[t,2]]
  } else j <- integer()
  if(length(j)) {
    q <- affine('retrieval.query',events[t,])
    k <- sweep(events[j,,drop=FALSE]%*%t(w[['retrieval.query.weight']]),2,w[['retrieval.query.bias']],'+')
    v <- sweep(states[j,,drop=FALSE]%*%t(w[['retrieval.value.weight']]),2,w[['retrieval.value.bias']],'+')
    scores <- as.numeric(k%*%q)/sqrt(32)
    a <- exp(scores-max(scores));a <- a/sum(a)
    manual_attention[t,j] <- a;read <- affine('retrieval.output',as.numeric(a%*%v))
  }
  manual_logits[t] <- affine('head.3',pmax(0,affine('head.0',c(read,x[t,]))))
  # Consume the current answer only after its prediction. Torch's GRU applies
  # reset to the recurrent candidate affine term; matching that convention matters.
  u <- c(x[t,],response[t,])
  for(layer in 1:3) {
    a <- as.numeric(w[[paste0('gru.weight_ih_l',layer)]]%*%u+w[[paste0('gru.bias_ih_l',layer)]])
    b <- as.numeric(w[[paste0('gru.weight_hh_l',layer)]]%*%previous[layer,]+w[[paste0('gru.bias_hh_l',layer)]])
    reset <- sigmoid(a[1:32]+b[1:32]);update <- sigmoid(a[33:64]+b[33:64])
    candidate <- tanh(a[65:96]+reset*b[65:96])
    previous[layer,] <- (1-update)*candidate+update*previous[layer,];u <- previous[layer,]
  }
  states[t,] <- previous[3,]
}
# Compare full-history and split-history execution against the installed runtime.
# The split at 100 also verifies the retrieval bank crossing a BPTT boundary.
runtime <- LKTGRURuntime(fit);net <- runtime$network;ns <- asNamespace('LKT')
tensor <- function(z,long=FALSE)get('lkt_gru_tensor',ns)(z,s,long)
nums <- tensor(array(numeric_values,c(1,T,8)))
cats <- tensor(array(ids,c(1,T,2)),TRUE);answers <- tensor(array(y,c(1,T)))
torch::with_no_grad({
  actual_x <- torch::as_array(net$encode(nums,cats))[1,,]
  actual_response <- torch::as_array(net$response_encoder(answers$unsqueeze(3)))[1,,]
  actual <- net(nums,cats,answers,retrieval_ids=filter_ids)
  first <- net(nums[,1:100,,drop=FALSE],cats[,1:100,,drop=FALSE],answers[,1:100,drop=FALSE],retrieval_ids=filter_ids[,1:100,,drop=FALSE])
  last <- net(nums[,101:103,,drop=FALSE],cats[,101:103,,drop=FALSE],answers[,101:103,drop=FALSE],first$hidden,first$context,
    retrieval_ids=filter_ids[,101:103,,drop=FALSE])
  changed <- y;changed[53:T] <- 1-changed[53:T]
  different <- net(nums,cats,tensor(array(changed,c(1,T))),retrieval_ids=filter_ids)
})
errors <- c(encoded_inputs=max(abs(x-actual_x)),encoded_response=max(abs(response-actual_response)),
  attention_weights=max(abs(manual_attention-torch::as_array(actual$retrieval_attention)[1,1,,])),
  logits=max(abs(manual_logits-as.numeric(torch::as_array(actual$logits)))),
  final_gru_state=max(abs(previous-torch::as_array(actual$hidden)[,1,])),
  bank_event_records=max(abs(tail(events,100)-torch::as_array(actual$context$retrieval_keys)[1,,])),
  bank_post_answer_states=max(abs(tail(states,100)-torch::as_array(actual$context$retrieval_values)[1,,])),
  chunk_continuation=max(abs(as.numeric(torch::as_array(last$logits))-tail(manual_logits,3))),
  current_and_future_answer_effect=max(abs(as.numeric(torch::as_array(actual$logits))[1:53]-as.numeric(torch::as_array(different$logits))[1:53])))
stopifnot(all(errors<1e-5),errors[['current_and_future_answer_effect']]==0,
  actual$context$retrieval_keys$size(2)==100L,all(manual_attention[upper.tri(manual_attention,diag=TRUE)]==0),
  manual_attention[103,1]==0,manual_attention[103,2]==0)
eligible <- outer(ids[,1],ids[,1],'==') | outer(ids[,2],ids[,2],'==')
stopifnot(all(manual_attention[!eligible]==0),all(manual_attention[1:36,]==0),
  identical(actual$context$retrieval_ids,filter_ids[,4:103,,drop=FALSE]))

# Initialization is a training rule, not the trained table shown at inference.
# Verify both the all-zero start and subsequent item-specific learning directly.
torch::torch_manual_seed(s$seed)
initial_net <- get('lkt_gru_network',ns)(prep,s)
initial_item <- torch::as_array(initial_net$embeddings[[1L]]$weight)
initial_skill <- torch::as_array(initial_net$embeddings[[2L]]$weight)
stopifnot(all(initial_item==0),any(initial_skill[-1,,drop=FALSE]!=0),
  all(w[['embeddings.0.weight']][1,]==0),any(w[['embeddings.0.weight']][-1,,drop=FALSE]!=0),
  max(apply(w[['embeddings.0.weight']][-1,,drop=FALSE],2,stats::sd))>0)
d <- readRDS(data_file)
held <- which(d[[selection$phase_column]]==selection$evaluation_phase);groups <- split(held,as.character(d$Anon.Student.Id[held]))
idx <- head(groups[[which.max(lengths(groups))]],120L);stopifnot(length(idx)==120L)
real <- LKTGRUStep(fit,LKTGRUState(fit),d[idx,,drop=FALSE],runtime)
errors <- c(errors,real_held_out_history=max(abs(real$predictions-fit$prediction[idx])))
stopifnot(tail(errors,1)<3e-6,dim(real$state$inference_context[[1]]$retrieval_keys)[2]==100L)

dir.create(output,recursive=TRUE,showWarnings=FALSE)
# The figure and FGUI share this selected specification. Save a pointer to the
# historical trained checkpoint without modifying or copying its fitted weights.
cfg <- readRDS(file.path(root,'docs/figures/gru-base-model/model-specification.rds'))
cfg$gru_options <- s
stopifnot(identical(getFromNamespace('lkt_config_arguments','LKT')(cfg)$model$settings,s))
saveRDS(cfg,file.path(output,'model-specification.rds'))
selection$specification_sha256 <- hash(file.path(output,'model-specification.rds'))
saveRDS(selection,file.path(output,'selection.rds'))
jsonlite::write_json(selection,file.path(output,'selection.json'),auto_unbox=TRUE,pretty=TRUE)
write.csv(data.frame(check=names(errors),maximum_difference=unname(errors)),file.path(output,'accuracy-check.csv'),row.names=FALSE)
write.csv(data.frame(tensor=names(w),shape=vapply(w,function(z)if(is.null(dim(z)))as.character(length(z)) else paste(dim(z),collapse=' x '),character(1))),file.path(output,'weight-shapes.csv'),row.names=FALSE)
writeLines(c(paste('Checkpoint:',normalizePath(checkpoint_file,winslash='/')),paste('Checkpoint SHA256:',hash(checkpoint_file)),
  paste('Study SHA256:',hash(file.path(root,selection$study))),paste('Fitted release source SHA256:',protocol$release[['lkt_source_sha256']]),
  paste('Verification runtime source SHA256:',lkt_release[['lkt_source_sha256']]),
  paste('Drawing script SHA256:',hash(file.path(root,'scripts/draw-gru-retrieval-network.R'))),
  paste('User-frozen default design; exact saved seed:',selected$seed),
  paste('Restored epoch:',fit$selected_epoch),paste(format(fit$parameter_count,big.mark=','),'parameter entries, including reserved zero embedding rows.'),
  'PASS: full ordinary R calculation of encoders, embeddings, all three GRU layers, one-head attention and prediction head.',
  'PASS: strict past-only retrieval, same-item OR same-skill mask, empty-match zero read, 100-event window, chunk continuation and 120 real held-out responses.',
  'PASS: initial item table is all zero, skills start random, trained items are nonzero and differ across items.',
  capture.output(print(errors))),file.path(output,'provenance-and-verification.txt'))

# Retain the established input/encoder/memory/head colors. Draw prediction before
# observation, and explicitly label the bank write as belonging to the next trial.
W <- 1800;H <- 2280
palette <- list(ink='#183247',muted='#526477',blue='#246BA0',blue_fill='#EAF3FC',
  teal='#137C73',teal_fill='#E6F4F0',purple='#7655A3',purple_fill='#F2EDFA',
  orange='#B96024',orange_fill='#FFF1E6',red='#A34344',red_fill='#FBEFF0')
label <- function(x,y,text,size=12,bold=FALSE,color=palette$ink,just='centre')grid::grid.text(text,
  x=x/W,y=1-y/H,just=just,gp=grid::gpar(fontfamily='DejaVu Sans',fontsize=size,
    fontface=if(bold)'bold' else 'plain',col=color,lineheight=1.14))
box <- function(x,y,width,height,text,fill,color,size=12,bold=FALSE) {
  grid::grid.roundrect(x=x/W,y=1-y/H,width=width/W,height=height/H,r=grid::unit(5,'pt'),gp=grid::gpar(fill=fill,col=color,lwd=1.1))
  label(x,y,text,size,bold)
}
edge <- function(x,y,color=palette$teal,dashed=FALSE,arrow=TRUE)grid::grid.lines(x=x/W,y=1-y/H,
  arrow=if(arrow)grid::arrow(length=grid::unit(.075,'in'),type='closed') else NULL,
  gp=grid::gpar(col=color,fill=color,lwd=1.35,lineend='round',lty=if(dashed)'dashed' else 'solid'))
dot <- function(x,y,color=palette$blue)grid::grid.circle(x=x/W,y=1-y/H,r=grid::unit(2,'pt'),gp=grid::gpar(fill=color,col=color))
draw <- function() {
  grid::grid.newpage()
  label(900,36,'Default GRU design: filtered retrieval + zero item initialization',21,TRUE)
  label(900,77,paste0('Three GRU layers of 32 units  |  128-unit prediction head  |  ',format(fit$parameter_count,big.mark=','),' parameters for the illustrated vocabularies'),11)
  label(45,118,'A   Encode the inputs available before response t',15,TRUE,just='left')
  for(x in c(230,680,1130,1580)) {edge(c(x,x),c(247,267),palette$blue);edge(c(x,x),c(414,434),palette$blue)}
  box(230,192,400,110,'3 timing values + availability flag\nRecency; item age; prior mean spacing\n4 numeric coordinates',palette$blue_fill,palette$blue,11)
  box(680,192,400,110,'4 LKT predictors\nIntercept; student logitdec;\nstudent-item logitdec; student-item PPES',palette$blue_fill,palette$blue,11)
  box(1130,192,400,110,paste0('Current item ID\n',format(n_items,big.mark=','),' known items'),palette$blue_fill,palette$blue,13,TRUE)
  box(1580,192,400,110,paste0('Current skill-combination ID\n',format(n_skills,big.mark=','),' known combinations'),palette$blue_fill,palette$blue,13,TRUE)
  box(230,343,400,142,'Time encoder\nLinear 4 \u2192 32; ReLU\nLinear 32 \u2192 16\n16 learned time values',palette$orange_fill,palette$orange,13,TRUE)
  box(680,343,400,142,'History-feature encoder\nLinear 4 \u2192 32; ReLU\nLinear 32 \u2192 16\n16 learned history values',palette$orange_fill,palette$orange,13,TRUE)
  box(1130,343,400,142,paste0('Trainable item embedding: ',format(n_items+1L,big.mark=','),' \u00d7 16\nAll item vectors START AT ZERO\nThen learn 16 values per item\n',format(n_items,big.mark=','),' known + 1 reserved zero row'),palette$blue_fill,palette$blue,12,TRUE)
  box(1580,343,400,142,paste0('Skill embedding: ',format(n_skills+1L,big.mark=','),' \u00d7 16\nRandom start; then learn normally\n',format(n_skills,big.mark=','),' known + 1 reserved zero row\n16 values per skill combination'),palette$blue_fill,palette$blue,12)
  box(900,469,1600,70,'Concatenate 16 time + 16 history + 16 item + 16 skill = 64 values\nInput dropout 0.1 produces x(t), shared by retrieval, prediction and memory update',palette$blue_fill,palette$blue,13,TRUE)
  label(900,531,'Recency = time since the previous response (any item). Item age and prior spacing use this student-item exposure history, including studies.',10,color=palette$muted)
  # This blue bus is the same current-input vector at all three destinations.
  edge(c(100,20,20),c(469,469,1770),palette$blue,arrow=FALSE)
  for(y in c(655,1240,1770))dot(20,y)
  edge(c(20,65),c(655,655),palette$blue);edge(c(20,50),c(1240,1240),palette$blue);edge(c(20,150),c(1770,1770),palette$blue)

  label(45,582,'B   Filter earlier memories, then retrieve: one head, width 32',15,TRUE,just='left')
  edge(c(300,300),c(705,745),palette$red)
  edge(c(900,900),c(705,745),palette$red);edge(c(1450,1450),c(705,745),palette$red)
  box(300,655,470,100,'Current event e(t): 96 values\nPrevious h3(t\u22121): 32 + x(t): 64',palette$teal_fill,palette$teal,13,TRUE)
  box(1150,655,1110,100,'Past completed events from this participant\nNewest 100 globally: e(j): 96; post-answer h3(j): 32; raw item/skill IDs',palette$teal_fill,palette$teal,12)
  box(1150,724,1110,32,'FILTER: past item = current item OR past skill = current skill (compare raw IDs)',palette$teal_fill,palette$teal,11,TRUE)
  box(300,800,420,110,'Shared Q/K linear 96 \u2192 32\nQuery q(t): 32 values\nUses current e(t)',palette$red_fill,palette$red,13)
  box(900,800,420,110,'Shared Q/K linear 96 \u2192 32\nKey k(j): 32 values per event\nUses past e(j)',palette$red_fill,palette$red,13)
  box(1450,800,420,110,'Value linear 32 \u2192 32\nv(j): 32 values per event\nUses past post-answer h3(j)',palette$red_fill,palette$red,13)
  edge(c(300,300,370),c(855,960,960),palette$red)
  edge(c(900,900,650,650),c(855,885,885,900),palette$red)
  edge(c(1450,1450),c(855,900),palette$red)
  edge(c(930,1050),c(960,970),palette$red)
  box(650,960,560,120,'Scores q(t) \u00b7 k(j) / sqrt(32)\nPast 100 AND (same item OR skill)\nSoftmax over eligible events only\nZero weight on all other events',palette$red_fill,palette$red,12,TRUE)
  box(1350,970,600,140,'Weighted sum: \u03a3 a(t,j) v(j), 32 values\nOutput linear 32 \u2192 32\nRetrieved memory r(t): 32 values\nNo eligible memory gives r(t) = 0',palette$red_fill,palette$red,13,TRUE)
  label(45,1080,'Filter the last 100 global events, not the last 100 matches. Q/K share weights and bias; no attention decay or dropout.',11,color=palette$muted,just='left')

  label(45,1120,'C   Predict the current response',15,TRUE,just='left')
  edge(c(1350,1350,300,300),c(1040,1140,1140,1180),palette$red)
  edge(c(550,650),c(1240,1240),palette$purple);edge(c(1050,1140),c(1240,1240),palette$purple);edge(c(1360,1420),c(1240,1240),palette$purple)
  box(300,1240,500,120,'Retrieved r(t): 32 + current x(t): 64\nConcatenate 96 values',palette$purple_fill,palette$purple,13)
  box(850,1240,400,120,'Linear 96 \u2192 128\nReLU\nHead dropout 0.1',palette$purple_fill,palette$purple,14,TRUE)
  box(1250,1240,220,120,'Linear 128 \u2192 1\nOne logit',palette$purple_fill,palette$purple,12)
  box(1585,1240,330,120,'Sigmoid + clipping\np(correct at trial t)\n\u03b5 = 10\u207b\u2075',palette$purple_fill,palette$purple,13,TRUE)
  label(900,1343,'The current answer is still unknown. Retrieved memory replaces the direct h3(t\u22121) input to this prediction head.',12,color=palette$purple)

  label(45,1420,'D   Observe the answer, update all three GRU layers, then write the bank',15,TRUE,just='left')
  edge(c(350,350),c(1545,1560),palette$orange);edge(c(350,350),c(1660,1720),palette$orange)
  box(350,1505,460,80,'Observed response y(t)\n0 = incorrect; 1 = correct',palette$orange_fill,palette$orange,13)
  box(350,1610,460,100,'Response encoder\nLinear 1 \u2192 32; ReLU; linear 32 \u2192 16\n16 learned answer values',palette$orange_fill,palette$orange,12,TRUE)
  edge(c(550,685),c(1770,1770))
  edge(c(915,950),c(1770,1770));edge(c(1050,1085),c(1770,1770))
  edge(c(1315,1350),c(1770,1770));edge(c(1450,1485),c(1770,1770))
  box(350,1770,400,100,'Current x(t): 64 + encoded y(t): 16\n80 values into GRU layer 1',palette$teal_fill,palette$teal,12)
  for(j in 1:3) {
    xx <- c(800,1200,1600)[j]
    edge(c(xx,xx),c(1660,1715));edge(c(xx,xx),c(1825,1860))
    box(xx,1620,230,80,paste0('Previous h',j,'(t\u22121)\n32 state values'),palette$teal_fill,palette$teal,12)
    box(xx,1770,230,110,paste0('GRU layer ',j,'\n32 units\nh',j,'(t): 32 values'),palette$teal_fill,palette$teal,13,TRUE)
  }
  for(xx in c(1000,1400))box(xx,1770,100,65,'Dropout\n0.1',palette$teal_fill,palette$teal,11)
  box(900,1900,1600,80,'Keep all three GRU states for the next trial\nh1(t), h2(t), h3(t): 3 \u00d7 32 values',palette$teal_fill,palette$teal,13,TRUE)
  # The new top state is written only after y(t). The dashed loop denotes the
  # next trial's bank, never an unmasked shortcut into the current prediction.
  dot(1600,1843,palette$teal)
  edge(c(1600,1730,1730,1700),c(1843,1843,2050,2050))
  box(1070,2050,1260,100,'For the next trial: append [e(t): 96, new h3(t): 32, raw item/skill IDs]\nKeep the newest 100 global events; e(t) is pre-answer, h3(t) is post-answer',palette$teal_fill,palette$teal,13,TRUE)
  edge(c(1700,1788,1788,1705),c(2050,2050,655,655),palette$teal,dashed=TRUE)
  label(900,2150,'All encoders, embeddings, GRU layers, retrieval projections and prediction weights learn jointly from response-prediction loss.',11)
  label(900,2186,'At a new participant: zero GRU states and an empty bank; keep the trained embeddings. Retrieval feeds prediction only.',11)
  label(900,2222,'Timing values use log(1 + seconds) and training-only scaling. Undefined prior spacing stays missing in the data, with an availability flag.',10,color=palette$muted)
  label(900,2254,'Only completed scored responses enter the bank. Dropout is off during evaluation. States and bank carry forward; gradients detach at 100-event BPTT chunk boundaries.',10,color=palette$muted)
}
# Export native vectors for zooming and publication, plus a matching preview PNG.
width <- 18;height <- width*H/W;stem <- 'gru-base-network'
svglite::svglite(file.path(output,paste0(stem,'.svg')),width=width,height=height,bg='white');draw();grDevices::dev.off()
grDevices::cairo_pdf(file.path(output,paste0(stem,'.pdf')),width=width,height=height,family='DejaVu Sans');draw();grDevices::dev.off()
grDevices::png(file.path(output,paste0(stem,'.png')),width=width,height=height,units='in',res=140,type='cairo',bg='white');draw();grDevices::dev.off()
cat('PASS: base specification, zero initialization, independent full-network equations, skill filter, retrieval causality/window, chunk continuation, and real held-out replay.\n')
print(errors)
