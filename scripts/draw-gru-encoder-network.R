# Draw the validation-selected augmented encoder-only architecture from its saved
# weights. Args: LKT root, optional downloaded study folder, optional output folder.
# The prior figure and all fitted/checkpoint/submission records remain unchanged.
args <- commandArgs(TRUE)
root <- normalizePath(if(length(args))args[1L] else '.',winslash='/',mustWork=TRUE)
study <- if(length(args)>=2L)args[2L] else file.path(root,
  '../LKT-outputs/encoder-inference-20261103/development-review/results')
output <- if(length(args)>=3L)args[3L] else file.path(root,'docs/figures/gru-encoder-augmented')
source(file.path(root,'scripts/load-current-lkt.R'))
release_sha <- lkt_release[['lkt_source_sha256']]
hash <- function(path)digest::digest(file=path,algo='sha256')
stopifnot(hash(attr(lkt_release,'archive'))==lkt_release[['lkt_source_sha256']],
  hash(system.file('research-source-manifest.json',package='LKT'))==hash(file.path(root,'inst/research-source-manifest.json')),
  requireNamespace('svglite',quietly=TRUE),torch::torch_is_installed())
manifest <- jsonlite::fromJSON(file.path(root,'inst/research-source-manifest.json'))
for(i in seq_len(nrow(manifest$files)))stopifnot(hash(file.path(root,manifest$files$path[i]))==manifest$files$sha256[i])

# Use the architecture chosen on mean validation loss. For a concrete checkpoint,
# select its lowest-validation-loss seed, avoiding selection from test performance.
p <- readRDS(file.path(study,'study.rds'))
choices <- read.csv(file.path(study,'selection/development-primary.csv'))
choice <- choices[choices$family=='augmented' & choices$stage=='encoders',]
stopifnot(nrow(choice)==1L,choice$candidate=='g112',identical(p$release$archive_sha256,release_sha))
validation <- do.call(rbind,lapply(1:3,function(seed)read.csv(file.path(study,'validation',
  paste0('development__augmented__encoders__',choice$candidate,'__seed',seed,'.csv')))))
selected <- validation[order(validation$log_loss,validation$seed),][1L,]
checkpoint_file <- file.path(study,'models',paste0(selected$run_id,'.rds'))
checkpoint <- readRDS(checkpoint_file);fit <- checkpoint$fit
s <- fit$gru_settings;prep <- fit$model$preprocessing;w <- fit$model$weights
expected <- p$settings[[choice$candidate]];expected$seed <- as.integer(selected$seed)
stopifnot(identical(s,expected),identical(checkpoint$settings,s),checkpoint$task$run_id==selected$run_id,
  s$feature_encoder=='mlp',s$inference_mode=='none',s$temporal_kl_beta==0,
  identical(s$encoder_hidden_sizes,32L),s$encoder_output_size==16L,s$encoder_activation=='relu',s$encoder_dropout==0,
  s$num_layers==3L,s$hidden_size==32L,identical(s$head_hidden_sizes,128L),s$head_activation=='relu',
  s$input_dropout==.1,s$gru_dropout==.1,s$head_dropout==.1,
  identical(s$categorical_cols,c('KC..Default.','KC..Cluster.')),
  identical(prep$numeric_names,c('time:log1p_gap_seconds','lkt:(Intercept)',
    'lkt:logitdecKC..Default.','lkt:ppesKC..Default.','lkt:logitdecAnon.Student.Id')),
  identical(dim(w[['embeddings.0.weight']]),c(145L,16L)),
  identical(dim(w[['embeddings.1.weight']]),c(37L,16L)),
  identical(dim(w[['gru.weight_ih_l1']]),c(96L,80L)),
  identical(dim(w[['head.0.weight']]),c(128L,96L)),
  identical(dim(w[['head.3.weight']]),c(1L,128L)),
  sum(vapply(w,length,integer(1)))==40945L,fit$parameter_count==40945L)

# Compute every learned feature transformation and the GRU/head equations using
# ordinary R arrays, independently of Torch's forward implementation.
affine <- function(prefix,x)as.numeric(w[[paste0(prefix,'.weight')]] %*% x+w[[paste0(prefix,'.bias')]])
encoder <- function(prefix,x)affine(paste0(prefix,'.1'),pmax(0,affine(paste0(prefix,'.0.0'),x)))
numeric_values <- seq(-.3,.3,length.out=5)
manual_time <- encoder('time_encoder',numeric_values[1L])
manual_auxiliary <- encoder('auxiliary_encoder',numeric_values[2:5])
manual_response <- encoder('response_encoder',1)
manual_x <- c(manual_time,manual_auxiliary,w[['embeddings.0.weight']][2L,],w[['embeddings.1.weight']][3L,])
runtime <- LKTGRURuntime(fit);net <- runtime$network;ns <- asNamespace('LKT')
tensor <- function(x,long=FALSE)get('lkt_gru_tensor',ns)(x,s,long)
numeric <- tensor(array(numeric_values,c(1L,1L,5L)))
categorical <- tensor(array(c(2L,3L),c(1L,1L,2L)),TRUE)
previous <- array(sin(seq_len(96)/9)/5,c(3L,1L,32L))
torch::with_no_grad({
  actual_x <- as.numeric(torch::as_array(net$encode(numeric,categorical)))
  actual_response <- as.numeric(torch::as_array(net$response_encoder(tensor(array(1,c(1L,1L,1L))))))
  actual <- net(numeric,categorical,tensor(array(1,c(1L,1L))),tensor(previous))
  different <- net(numeric,categorical,tensor(array(0,c(1L,1L))),tensor(previous))
  actual_logit <- as.numeric(torch::as_array(actual$logits))
  actual_hidden <- torch::as_array(actual$hidden)
  other_logit <- as.numeric(torch::as_array(different$logits))
  other_hidden <- torch::as_array(different$hidden)
})
manual <- array(0,dim(previous));u <- c(manual_x,manual_response)
sigmoid <- function(x)1/(1+exp(-x))
for(layer in 1:3) {
  # GRU state dictionaries name the bias separately from the matrix prefix.
  # The explicit form below respects Torch's reset-after-recurrent-affine convention.
  a <- as.numeric(w[[paste0('gru.weight_ih_l',layer)]] %*% u+w[[paste0('gru.bias_ih_l',layer)]])
  b <- as.numeric(w[[paste0('gru.weight_hh_l',layer)]] %*% previous[layer,1,]+w[[paste0('gru.bias_hh_l',layer)]])
  r <- sigmoid(a[1:32]+b[1:32]);z <- sigmoid(a[33:64]+b[33:64])
  candidate <- tanh(a[65:96]+r*b[65:96])
  manual[layer,1,] <- (1-z)*candidate+z*previous[layer,1,];u <- manual[layer,1,]
}
manual_logit <- affine('head.3',pmax(0,affine('head.0',c(previous[3,1,],manual_x))))
errors <- c(feature_encoder=max(abs(manual_x-actual_x)),response_encoder=max(abs(manual_response-actual_response)),
  recurrent_state=max(abs(manual-actual_hidden)),prediction_head=abs(manual_logit-actual_logit),
  current_answer_prediction_effect=abs(actual_logit-other_logit),current_answer_memory_effect=max(abs(actual_hidden-other_hidden)))
stopifnot(all(errors[1:4]<3e-6),errors[5L]==0,errors[6L]>1e-7)
data_file <- file.path(study,'data/prepared_data.rds')
stopifnot(hash(data_file)==p$data_sha256)
d <- readRDS(data_file);idx <- which(d$fold==3L)
idx <- head(idx[d$Anon.Student.Id[idx]==d$Anon.Student.Id[idx[1L]]],120L)
replay <- LKTGRUStep(fit,LKTGRUState(fit),d[idx,,drop=FALSE],runtime)$predictions
errors <- c(errors,real_history_replay=max(abs(replay-fit$prediction[idx])))
stopifnot(tail(errors,1L)<3e-6)
dir.create(output,recursive=TRUE,showWarnings=FALSE)
write.csv(data.frame(check=names(errors),maximum_difference=unname(errors)),file.path(output,'accuracy-check.csv'),row.names=FALSE)
write.csv(data.frame(tensor=names(w),shape=vapply(w,function(x)if(is.null(dim(x)))as.character(length(x)) else
  paste(dim(x),collapse=' x '),character(1)),entries=vapply(w,length,integer(1))),file.path(output,'weight-shapes.csv'),row.names=FALSE)
writeLines(c(paste('Checkpoint:',normalizePath(checkpoint_file,winslash='/')),
  paste('Checkpoint SHA256:',hash(checkpoint_file)),paste('Original study SHA256:',hash(file.path(study,'study.rds'))),
  paste('Current release SHA256:',release_sha),paste('Installed library:',find.package('LKT')),
  paste('Drawing script SHA256:',hash(file.path(root,'scripts/draw-gru-encoder-network.R'))),
  paste('Candidate selected by mean validation loss:',choice$candidate),
  paste('Representative seed selected by validation loss:',selected$seed),
  paste('Restored epoch:',fit$selected_epoch),paste('Replayed real held-out responses:',length(idx)),
  'PASS: feature/response encoders, both embedding tables, three GRU layers, head, causal order, and real-history predictions.',
  'This is the best mean-AUC architecture on the original development test split so far. Full CV aggregation is pending.',
  capture.output(print(errors))),file.path(output,'provenance-and-verification.txt'))

# Keep the same colors, trial indexing and predict/update ordering as the original
# network figure. Amber boxes mark the newly added feature transformations.
W <- 1800;H <- 1810
palette <- list(ink='#183247',muted='#526477',blue='#246BA0',blue_fill='#EAF3FC',
  teal='#137C73',teal_fill='#E6F4F0',purple='#7655A3',purple_fill='#F2EDFA',
  orange='#B96024',orange_fill='#FFF1E6',border='#CBD5E1')
label <- function(x,y,text,size=12,bold=FALSE,color=palette$ink,just='centre')grid::grid.text(text,
  x=x/W,y=1-y/H,just=just,gp=grid::gpar(fontfamily='DejaVu Sans',fontsize=size,
    fontface=if(bold)'bold' else 'plain',col=color,lineheight=1.15))
box <- function(x,y,width,height,text,fill,color,size=12,bold=FALSE) {
  grid::grid.roundrect(x=x/W,y=1-y/H,width=width/W,height=height/H,r=grid::unit(5,'pt'),
    gp=grid::gpar(fill=fill,col=color,lwd=1.1));label(x,y,text,size,bold)
}
edge <- function(x,y,color=palette$teal)grid::grid.lines(x=x/W,y=1-y/H,
  arrow=grid::arrow(length=grid::unit(.075,'in'),type='closed'),
  gp=grid::gpar(col=color,fill=color,lwd=1.35,lineend='round'))
dot <- function(x,y,color=palette$teal)grid::grid.circle(x=x/W,y=1-y/H,r=grid::unit(2,'pt'),gp=grid::gpar(fill=color,col=color))
draw <- function(detail=TRUE) {
  grid::grid.newpage()
  label(900,38,'Augmented GRU with learned feature encoders',22,TRUE)
  label(900,77,paste0('Candidate g112  |  representative seed ',selected$seed,' (lowest validation loss)  |  restored epoch ',fit$selected_epoch,'  |  40,945 parameter entries'),11)
  label(45,118,'A   Encode inputs available before response t',15,TRUE,just='left')
  for(x in c(230,680,1130,1580)) {
    edge(c(x,x),c(235,255),palette$blue);edge(c(x,x),c(395,405),palette$blue)
  }
  box(230,185,400,100,'Log(1 + student time gap in seconds)\n1 standardized numeric value',palette$blue_fill,palette$blue,12)
  box(680,185,400,100,'4 standardized LKT inputs\nintercept; student-item logitdec;\nstudent-item PPES; student logitdec',palette$blue_fill,palette$blue,11)
  box(1130,185,400,100,'Current item ID: KC..Default.\n144 known items',palette$blue_fill,palette$blue,12,TRUE)
  box(1580,185,400,100,'Current skill ID: KC..Cluster.\n36 known skills',palette$blue_fill,palette$blue,12,TRUE)
  box(230,325,400,140,'Time encoder\nLinear 1 \u2192 32; ReLU\nLinear 32 \u2192 16\n16 learned time values',palette$orange_fill,palette$orange,13,TRUE)
  box(680,325,400,140,'History-feature encoder\nLinear 4 \u2192 32; ReLU\nLinear 32 \u2192 16\n16 learned history values',palette$orange_fill,palette$orange,13,TRUE)
  box(1130,325,400,140,'Learned item embedding\n145 \u00d7 16 table\n144 known + 1 reserved zero row\n16 item values',palette$blue_fill,palette$blue,12)
  box(1580,325,400,140,'Learned skill embedding\n37 \u00d7 16 table\n36 known + 1 reserved zero row\n16 skill values',palette$blue_fill,palette$blue,12)
  box(900,445,1560,80,'Concatenate: 16 time + 16 history + 16 item + 16 skill = 64 coordinates\nInput dropout 0.1 produces x(t), shared by prediction and memory update',palette$blue_fill,palette$blue,13,TRUE)
  label(900,516,'Each encoder has its own weights and biases. Its 16-value output is linear; ReLU is applied after the 32-unit hidden layer.',10,color=palette$muted)

  label(45,560,'B   Predict the current response',15,TRUE,just='left')
  edge(c(120,15,15,200),c(445,445,670,670),palette$blue);dot(15,670,palette$blue)
  edge(c(1380,1380,1785,1785,375,375),c(982,810,810,585,585,613))
  label(1035,570,'Previous top-layer memory',10,color=palette$teal)
  edge(c(550,645),c(670,670),palette$purple);edge(c(1065,1155),c(670,670),palette$purple)
  edge(c(1345,1405),c(670,670),palette$purple)
  box(375,670,350,115,'Previous h3(t\u22121): 32\n+ current x(t): 64\n96 coordinates',palette$purple_fill,palette$purple,13)
  box(855,670,420,115,'Linear 96 \u2192 128 (with bias)\nReLU\nHead dropout 0.1',palette$purple_fill,palette$purple,13,TRUE)
  box(1250,670,190,115,'Linear 128 \u2192 1\n(with bias)\nOne logit',palette$purple_fill,palette$purple,12)
  box(1565,670,320,115,'Sigmoid + clipping\np(correct at trial t)\n\u03b5 = 10\u207b\u2075',palette$purple_fill,palette$purple,13,TRUE)
  label(900,780,'The current answer is still unknown here. The head reads the previous top-layer memory, just as in the original model.',11,color=palette$purple)

  label(45,845,'C   Observe and encode the answer, then update memory',15,TRUE,just='left')
  edge(c(15,15,190),c(670,1210,1210),palette$blue)
  edge(c(355,355),c(943,980),palette$orange);edge(c(355,355),c(1120,1155),palette$orange)
  box(355,905,330,75,'Observed response y(t)\n0 = incorrect; 1 = correct',palette$orange_fill,palette$orange,12)
  box(355,1050,330,140,'Response encoder\nLinear 1 \u2192 32; ReLU\nLinear 32 \u2192 16\n16 learned answer values',palette$orange_fill,palette$orange,12,TRUE)
  edge(c(520,590),c(1210,1210))
  edge(c(770,795),c(1210,1210));edge(c(895,940),c(1210,1210))
  edge(c(1120,1155),c(1210,1210));edge(c(1255,1290),c(1210,1210));edge(c(1470,1540),c(1210,1210))
  for(x in c(680,1030,1380))edge(c(x,x),c(1058,1155))
  for(x in c(782,1135,1510)) {edge(c(x,x),c(1210,1325));dot(x,1210)}
  label(90,1185,'x(t): 64',11,color=palette$blue)
  box(355,1210,330,110,'Concatenate predictors + answer\n64 + 16 = 80 coordinates',palette$teal_fill,palette$teal,12)
  for(j in 1:3) {
    x <- c(680,1030,1380)[j]
    box(x,1020,180,75,paste0('h',j,'(t\u22121)\n32 previous values'),palette$teal_fill,palette$teal,12)
    box(x,1210,180,110,paste0('GRU layer ',j,'\n32 units\nOwn state'),palette$teal_fill,palette$teal,12,TRUE)
  }
  for(x in c(845,1205))box(x,1210,100,70,'Dropout\n0.1',palette$teal_fill,palette$teal,11)
  label(785,1140,'h1(t): 32',10,color=palette$teal);label(1135,1140,'h2(t): 32',10,color=palette$teal)
  box(1600,1210,120,80,'h3(t)\n32 values',palette$teal_fill,palette$teal,12,TRUE)
  box(1060,1360,1240,70,'Retain all three states for the next trial: [h1(t), h2(t), h3(t)] = 3 \u00d7 32 values',palette$teal_fill,palette$teal,13)
  label(900,1420,'Each new participant starts with zero memory. Updated memory affects the next prediction, p(t+1).',11)
  label(900,1453,'All encoders, embeddings, GRU layers and head weights are learned jointly by backpropagation; continuation keeps weights fixed.',10,color=palette$muted)
  label(900,1486,'Encoder dropout is zero. Input, between-GRU-layer and head dropout are 0.1 during training and disabled during evaluation.',10,color=palette$muted)
  if(!detail)return(invisible(NULL))
  label(45,1570,'D   Changes from the original augmented GRU figure',15,TRUE,just='left')
  box(320,1660,500,110,'Current predictor vector\n37 raw/embedded coordinates\n\u2192 64 learned/embedded coordinates',palette$orange_fill,palette$orange,12)
  box(900,1660,500,110,'First GRU layer input\n37 predictors + 1 raw answer = 38\n\u2192 64 predictors + 16 answer values = 80',palette$orange_fill,palette$orange,12)
  box(1480,1660,500,110,'Prediction head\nBefore: 69 \u2192 128 \u2192 1\nNow: 96 \u2192 128 \u2192 1',palette$orange_fill,palette$orange,13)
  label(900,1750,'The GRU remains 3 \u00d7 32. There is no separate student-inference network, Gaussian sampling, or KL penalty in this model.',11,color=palette$muted)
  label(900,1785,'New encoder blocks are amber. Exact dimensions, equations and held-out predictions verified against the saved LKT 2.0.0.9011 checkpoint.',10,color=palette$muted)
}
for(detail in c(FALSE,TRUE)) {
  H <- if(detail)1810 else 1510
  stem <- if(detail)'gru-encoder-network' else 'gru-encoder-network-overview'
  width <- 18;height <- width*H/W
  svglite::svglite(file.path(output,paste0(stem,'.svg')),width=width,height=height,bg='white')
  draw(detail);grDevices::dev.off()
  grDevices::cairo_pdf(file.path(output,paste0(stem,'.pdf')),width=width,height=height,family='DejaVu Sans')
  draw(detail);grDevices::dev.off()
  grDevices::png(file.path(output,paste0(stem,'.png')),width=width,height=height,units='in',res=140,type='cairo',bg='white')
  draw(detail);grDevices::dev.off()
}
cat('PASS: exact encoder-only augmented diagram, all forward equations and real-history replay.\n')
print(errors)
