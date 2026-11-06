# Draw the chosen 128-unit augmented GRU from its unmodified saved checkpoint.
# Args: LKT root, optional saved grid checkpoint, optional figure output folder.
# This reads existing weights; it does not fit or change a model.
args <- commandArgs(TRUE)
root <- normalizePath(if(length(args))args[1L] else '.',winslash='/',mustWork=TRUE)
checkpoint_file <- if(length(args)>=2L)args[2L] else file.path(root,
  '../LKT-outputs/gru-head-widths-largerawsample-bigblue-20261102/models/augmented__head128__seed1.rds')
output <- if(length(args)>=3L)args[3L] else file.path(root,'docs/figures/gru-head128')
# Select the sole supported installation explicitly. Standard R libraries supply
# dependencies only; checking the loaded path prevents an alternate LKT installation.
source(file.path(root,'scripts/load-current-lkt.R'))
release_sha <- lkt_release[['lkt_source_sha256']]
hash <- function(path)digest::digest(file=path,algo='sha256')
stopifnot(hash(attr(lkt_release,'archive'))==lkt_release[['lkt_source_sha256']],
  hash(system.file('research-source-manifest.json',package='LKT'))==hash(file.path(root,'inst/research-source-manifest.json')),
  requireNamespace('svglite',quietly=TRUE),torch::torch_is_installed())
manifest <- jsonlite::fromJSON(file.path(root,'inst/research-source-manifest.json'))
for(i in seq_len(nrow(manifest$files)))stopifnot(identical(
  digest::digest(file.path(root,manifest$files$path[i]),algo='sha256',file=TRUE),manifest$files$sha256[i]))
stopifnot('DejaVu Sans' %in% systemfonts::system_fonts()$family)
dir.create(output,recursive=TRUE,showWarnings=FALSE)

# Check the original checkpoint, data and resolved settings against its frozen plan.
# This is the user's chosen 128-unit architecture, not the grid's 256-unit winner.
checkpoint <- readRDS(checkpoint_file)
fit <- checkpoint$fit
experiment <- dirname(dirname(checkpoint_file))
protocol_file <- file.path(experiment,'head_width_protocol.rds');protocol <- readRDS(protocol_file)
stopifnot(checkpoint$run_id=='augmented__head128__seed1',
  identical(checkpoint$protocol_sha256,digest::digest(protocol_file,algo='sha256',file=TRUE)),
  identical(fit$gru_settings,protocol$settings[[checkpoint$run_id]]),
  identical(digest::digest(checkpoint_file,algo='sha256',file=TRUE),
    'dc553ba7bd721c6287bd37eac385c66358f27c7f0fd4350dc2ac9b2956a52bc5'))
model <- fit$model;s <- fit$gru_settings;prep <- model$preprocessing;weights <- model$weights
numeric_width <- length(prep$numeric_names)
embedding_width <- sum(prep$embedding_dims)
feature_width <- numeric_width+embedding_width
known_items <- length(prep$vocabularies[[1L]]);known_skills <- length(prep$vocabularies[[2L]])
stopifnot(s$num_layers==3L,s$hidden_size==32L,identical(s$head_hidden_sizes,128L),
  s$inference_mode=='none',s$seed==1L,fit$selected_epoch==24L,
  s$head_activation=='relu',s$categorical_encoding=='embedding',s$use_lkt_features,
  s$standardize,identical(s$history_group_cols,s$subject_col),s$time_units=='seconds',
  identical(prep$numeric_names,c('time:log1p_gap_seconds','lkt:(Intercept)',
    'lkt:logitdecKC..Default.','lkt:ppesKC..Default.','lkt:logitdecAnon.Student.Id')),
  identical(s$categorical_cols,c('KC..Default.','KC..Cluster.')),
  identical(unname(prep$embedding_dims),c(16L,16L)),
  s$gru_bias,s$head_bias,numeric_width==5L,embedding_width==32L,known_items==144L,known_skills==36L,
  s$input_dropout==.1,s$gru_dropout==.1,s$head_dropout==.1,s$clip_epsilon==1e-5,
  identical(dim(weights[['embeddings.0.weight']]),c(145L,16L)),
  identical(dim(weights[['embeddings.1.weight']]),c(37L,16L)),
  identical(dim(weights[['gru.weight_ih_l1']]),c(96L,38L)),
  identical(dim(weights[['head.0.weight']]),c(128L,69L)),
  identical(dim(weights[['head.3.weight']]),c(1L,128L)),
  sum(vapply(weights,length,integer(1)))==fit$parameter_count,fit$parameter_count==31585,
  identical(getFromNamespace('lkt_gru_settings','LKT')(s),s))

# Verify the drawing's mathematical cell against the current R implementation.
# Nonzero prior states exercise all three recurrent layers, with dropout off.
backend <- asNamespace('LKT')
runtime <- LKT::LKTGRURuntime(fit)
net <- runtime$network
tensor <- function(x)backend$lkt_gru_tensor(x,s)
numeric <- tensor(array(seq(-.3,.3,length.out=numeric_width),c(1L,1L,numeric_width)))
categorical <- backend$lkt_gru_tensor(array(c(2L,3L),c(1L,1L,2L)),s,long=TRUE)
previous <- array(sin(seq_len(3L*32L)/9)/5,c(3L,1L,32L))
answers <- tensor(array(1,c(1L,1L)))
hidden <- tensor(previous)
torch::with_no_grad({
  x <- as.numeric(torch::as_array(net$encode(numeric,categorical)))
  manual_x <- c(as.numeric(torch::as_array(numeric)),weights[['embeddings.0.weight']][2L,],
    weights[['embeddings.1.weight']][3L,])
  stopifnot(max(abs(x-manual_x))<1e-7)
  actual <- net(numeric,categorical,answers,hidden)
  actual_hidden <- torch::as_array(actual$hidden)
  actual_logit <- as.numeric(torch::as_array(actual$logits))
  unanswered_otherwise <- net(numeric,categorical,tensor(array(0,c(1L,1L))),hidden)
  other_logit <- as.numeric(torch::as_array(unanswered_otherwise$logits))
  other_hidden <- torch::as_array(unanswered_otherwise$hidden)
})
manual <- array(0,dim(previous));u <- c(x,1)
sigmoid <- function(x)1/(1+exp(-x))
for(layer in 1:3) {
  # Torch packs reset, update, and candidate coefficients in that order.
  # The reset gate multiplies the complete recurrent candidate projection.
  a <- as.numeric(weights[[paste0('gru.weight_ih_l',layer)]] %*% u+
    weights[[paste0('gru.bias_ih_l',layer)]])
  c <- as.numeric(weights[[paste0('gru.weight_hh_l',layer)]] %*% previous[layer,1,]+
    weights[[paste0('gru.bias_hh_l',layer)]])
  r <- sigmoid(a[1:32]+c[1:32]);z <- sigmoid(a[33:64]+c[33:64])
  n <- tanh(a[65:96]+r*c[65:96])
  manual[layer,1,] <- (1-z)*n+z*previous[layer,1,]
  u <- manual[layer,1,]
}

# The prediction head uses the old top-layer state, not the newly updated one.
# Changing the current answer must change memory without changing its prediction.
head_input <- c(previous[3,1,],x)
head <- pmax(0,as.numeric(weights[['head.0.weight']] %*% head_input+weights[['head.0.bias']]))
manual_logit <- as.numeric(weights[['head.3.weight']] %*% head+weights[['head.3.bias']])
state_error <- max(abs(manual-actual_hidden))
logit_error <- abs(manual_logit-actual_logit)
answer_prediction_effect <- max(abs(actual_logit-other_logit))
answer_memory_effect <- max(abs(actual_hidden-other_hidden))
stopifnot(state_error<3e-6,logit_error<3e-6,answer_prediction_effect==0,answer_memory_effect>1e-7,
  max(abs(weights[['embeddings.0.weight']][1L,]))==0,
  max(abs(weights[['embeddings.1.weight']][1L,]))==0)

# Confirm that the current maintained implementation also replays real saved-model
# predictions, not just synthetic tensor examples. No weights or artifacts are changed.
data_file <- file.path(experiment,'prepared_data.rds')
stopifnot(identical(digest::digest(data_file,algo='sha256',file=TRUE),protocol$data_sha256))
d <- readRDS(data_file);test_rows <- which(d$fold==3L)
student <- as.character(d$Anon.Student.Id[test_rows[1L]])
rows <- head(test_rows[as.character(d$Anon.Student.Id[test_rows])==student],120L)
replay <- LKT::LKTGRUStep(fit,LKT::LKTGRUState(fit),d[rows,,drop=FALSE],runtime=runtime)
replay_error <- max(abs(replay$predictions-fit$prediction[rows]))
stopifnot(replay_error<3e-6)
audit <- data.frame(check=c('Known item identities','Known skill identities','Reserved rows per embedding table',
  'Item embedding width','Skill embedding width',
  'Numeric input columns','Encoded feature width','First-layer update input width',
  'GRU layers','Units per GRU layer','Prediction-head input width','Head hidden units',
  'Parameter entries including reserved rows','Maximum manual-GRU state error',
  'Maximum manual-head logit error','Current-answer effect on current logit',
  'Current-answer effect on updated memory','Real held-out response replay rows','Maximum held-out replay error'),
  value=c(known_items,known_skills,1,16,16,numeric_width,feature_width,feature_width+1,
    s$num_layers,s$hidden_size,s$hidden_size+feature_width,s$head_hidden_sizes,
    fit$parameter_count,state_error,logit_error,answer_prediction_effect,answer_memory_effect,length(rows),replay_error))
write.csv(audit,file.path(output,'gru-network-audit.csv'),row.names=FALSE)
write.csv(data.frame(coordinate=seq_along(prep$numeric_names),source=prep$numeric_names,
  training_center=unname(prep$center),training_scale=unname(prep$scale)),
  file.path(output,'gru-network-numeric-inputs.csv'),row.names=FALSE)
write.csv(data.frame(tensor=names(weights),shape=vapply(weights,function(w)
  if(is.null(dim(w)))as.character(length(w)) else paste(dim(w),collapse=' x '),character(1)),
  entries=vapply(weights,length,integer(1))),file.path(output,'gru-network-weight-shapes.csv'),row.names=FALSE)
writeLines(c('Selected augmented GRU: diagram accuracy checks',
  capture.output(print(audit,row.names=FALSE)),
  'PASS: all dimensions and equations agree with the saved model.',
  paste0('PASS: current LKT ',lkt_release[['lkt_version']],' replays the unmodified historical checkpoint within 3e-6.')),
  file.path(output,'gru-network-audit.txt'))
writeLines(c(paste('Checkpoint:',normalizePath(checkpoint_file,winslash='/')),
  paste('Checkpoint SHA256:',digest::digest(checkpoint_file,algo='sha256',file=TRUE)),
  paste('Historical training LKT version:',fit$software_versions$LKT),
  paste('Diagram runtime LKT:',as.character(utils::packageVersion('LKT'))),
  paste('Active installation:',find.package('LKT')),
  paste('Current release archive SHA256:',release_sha),
  paste('Original protocol SHA256:',checkpoint$protocol_sha256),
  paste('Drawing script SHA256:',digest::digest(file.path(root,'scripts/draw-gru-network.R'),algo='sha256',file=TRUE)),
  'Numeric column order:',paste0('  ',prep$numeric_names),
  'PASS: exact saved weights/settings, both embeddings, parameter count, Torch GRU equations, head equation, current-answer causality, and real-history replay.'),
  file.path(output,'gru-network-provenance.txt'))

# One drawing function supplies all three formats, keeping layout and labels
# consistent between the editable SVG, print PDF, and preview PNG.
W <- 1800;H <- 2130
palette <- list(ink='#183247',muted='#526477',border='#CBD5E1',
  blue='#246BA0',blue_fill='#EAF3FC',teal='#137C73',teal_fill='#E6F4F0',
  purple='#7655A3',purple_fill='#F2EDFA',orange='#B96024',orange_fill='#FFF1E6')
label <- function(x,y,text,size=12,bold=FALSE,color=palette$ink,just='centre') {
  grid::grid.text(text,x=x/W,y=1-y/H,just=just,
    gp=grid::gpar(fontfamily='DejaVu Sans',fontsize=size,fontface=if(bold)'bold' else 'plain',
      col=color,lineheight=1.15))
}
box <- function(x,y,w,h,text,fill,color=palette$border,size=12,bold=FALSE) {
  grid::grid.roundrect(x=x/W,y=1-y/H,width=w/W,height=h/H,r=grid::unit(5,'pt'),
    gp=grid::gpar(fill=fill,col=color,lwd=1.1))
  label(x,y,text,size,bold)
}
edge <- function(x,y,color=palette$teal,arrow=TRUE,dashed=FALSE) {
  grid::grid.lines(x=x/W,y=1-y/H,
    arrow=if(arrow)grid::arrow(length=grid::unit(.075,'in'),type='closed') else NULL,
    gp=grid::gpar(col=color,fill=color,lwd=1.35,lty=if(dashed)'dashed' else 'solid',lineend='round'))
}
dot <- function(x,y,color=palette$teal)grid::grid.circle(x=x/W,y=1-y/H,
  r=grid::unit(2,'pt'),gp=grid::gpar(fill=color,col=color))

draw <- function(detail=TRUE) {
  grid::grid.newpage()
  label(900,38,'LKT augmented GRU with a 128-unit prediction head',22,TRUE)
  label(900,77,'Saved model: augmented__head128__seed1  |  restored epoch 24  |  31,585 parameter entries',12)
  label(45,118,'A   Inputs available before response t',15,TRUE,just='left')

  # Each categorical ID performs a learned table lookup. The two independent
  # 16-value embeddings join the five numerics in the actual saved encoder order.
  edge(c(850,850),c(223,250),palette$blue)
  edge(c(1430,1430),c(223,250),palette$blue)
  edge(c(290,290,450,450),c(335,380,380,395),palette$blue)
  edge(c(850,850,900,900),c(350,380,380,395),palette$blue)
  edge(c(1430,1430,1350,1350),c(350,380,380,395),palette$blue)
  box(290,240,490,190,paste0('5 numeric coordinates (in saved order)\nlog(1 + student time gap in seconds)\nintercept; student-item logitdec;\nstudent-item PPES; student logitdec\nStandardize using training-set statistics'),palette$blue_fill,palette$blue,12)
  box(850,185,480,75,'Current item ID: KC..Default.\n144 known items',palette$blue_fill,palette$blue,12,TRUE)
  box(1430,185,480,75,'Current skill ID: KC..Cluster.\n36 known skills',palette$blue_fill,palette$blue,12,TRUE)
  box(850,300,480,100,'Learned item embedding: 16 values\nTable: 145 rows \u00d7 16 columns\n144 known rows + 1 reserved zero row',palette$blue_fill,palette$blue)
  box(1430,300,480,100,'Learned skill embedding: 16 values\nTable: 37 rows \u00d7 16 columns\n36 known rows + 1 reserved zero row',palette$blue_fill,palette$blue)
  box(900,435,1200,80,'Concatenate: 5 numeric + 16 item + 16 skill = 37 coordinates\nInput dropout 0.1 produces x(t), shared by prediction and memory update',palette$blue_fill,palette$blue,13,TRUE)
  label(900,498,'Embeddings are trained jointly with the network. The centered intercept coordinate is zero; no problem-text embedding is used.',10,color=palette$muted)

  # The head reads the PREVIOUS top-layer state. Keep this path separate from
  # the answer-conditioned state update so the diagram makes causality explicit.
  label(45,532,'B   Predict the current response',15,TRUE,just='left')
  edge(c(300,15,15,200),c(435,435,640,640),palette$blue)
  dot(15,640,palette$blue)
  edge(c(1380,1380,1785,1785,375,375),c(823,770,770,555,555,583))
  label(1035,540,'Previous top-layer memory',10,color=palette$teal)
  edge(c(550,645),c(640,640),palette$purple)
  edge(c(1065,1155),c(640,640),palette$purple)
  edge(c(1345,1405),c(640,640),palette$purple)
  box(375,640,350,115,'Previous h3(t\u22121): 32\n+ current x(t): 37\n69 coordinates',palette$purple_fill,palette$purple,13)
  box(855,640,420,115,'Linear 69 \u2192 128 (with bias)\nReLU\nHead dropout 0.1',palette$purple_fill,palette$purple,13,TRUE)
  box(1250,640,190,115,'Linear 128 \u2192 1\n(with bias)\nOne logit',palette$purple_fill,palette$purple,12)
  box(1565,640,320,115,'Sigmoid + clipping\np(correct at trial t)\n\u03b5 = 10\u207b\u2075',palette$purple_fill,palette$purple,13,TRUE)
  label(900,744,'Current response y(t) does not enter this prediction. No separate inference network or Gaussian latent is present.',11,color=palette$purple)

  label(45,800,'C   Observe the answer, then update all three GRU layers',15,TRUE,just='left')
  edge(c(15,15,190),c(640,1015,1015),palette$blue)
  label(90,992,'x(t): 37',11,color=palette$blue)
  edge(c(355,355),c(923,960),palette$orange)
  edge(c(520,590),c(1015,1015))
  edge(c(770,795),c(1015,1015));edge(c(895,940),c(1015,1015))
  edge(c(1120,1155),c(1015,1015));edge(c(1255,1290),c(1015,1015))
  edge(c(1470,1540),c(1015,1015))
  for(x in c(680,1030,1380))edge(c(x,x),c(898,960))
  for(x in c(782,1135,1510)) {edge(c(x,x),c(1015,1115));dot(x,1015)}
  box(355,885,330,75,'Observed response y(t)\n0 = incorrect; 1 = correct',palette$orange_fill,palette$orange,12)
  box(355,1015,330,110,'Concatenate [x(t), y(t)]\n37 + 1 = 38 coordinates',palette$teal_fill,palette$teal,13)
  for(j in 1:3) {
    x <- c(680,1030,1380)[j]
    box(x,860,180,75,paste0('h',j,'(t\u22121)\n32 previous values'),palette$teal_fill,palette$teal,12)
    box(x,1015,180,110,paste0('GRU layer ',j,'\n32 units\nOwn state'),palette$teal_fill,palette$teal,12,TRUE)
  }
  for(x in c(845,1205))box(x,1015,100,70,'Dropout\n0.1',palette$teal_fill,palette$teal,11)
  label(785,947,'h1(t): 32',10,color=palette$teal)
  label(1135,947,'h2(t): 32',10,color=palette$teal)
  box(1600,1015,120,80,'h3(t)\n32 values',palette$teal_fill,palette$teal,12,TRUE)
  box(1060,1150,1240,70,'Retain all three states for the next trial: [h1(t), h2(t), h3(t)] = 3 \u00d7 32 values',palette$teal_fill,palette$teal,13)
  label(900,1210,'Each new participant starts with zero states. Updated memory can affect the NEXT prediction, p(t+1).',11)
  label(900,1242,'Dropout is active only during training. Binary cross-entropy trains the logits; all learned weights stay fixed during continuation.',10,color=palette$muted)
  if(!detail)return(invisible(NULL))

  label(45,1320,'D   Inside each GRU layer: exact Torch gate equations',15,TRUE,just='left')
  label(45,1359,'Layer input u(t) has 38 coordinates in layer 1 and 32 in layers 2 and 3. Every layer has its own weights and biases.',12,just='left')
  edge(c(195,240),c(1490,1490));edge(c(195,240),c(1730,1730))
  edge(c(580,700),c(1440,1440));edge(c(580,700),c(1600,1600));edge(c(580,700),c(1760,1760))
  edge(c(1000,1135),c(1440,1440))
  edge(c(1000,1060,1060,850,850),c(1600,1600,1680,1680,1710))
  edge(c(1000,1100,1100,1135),c(1760,1760,1575,1575))
  edge(c(110,110,1290,1290),c(1780,1890,1890,1615))
  edge(c(1445,1485),c(1520,1520));dot(110,1780)
  box(110,1490,170,100,'Layer input\nu(t)',palette$blue_fill,palette$blue,13,TRUE)
  box(110,1730,170,100,'Previous state\nh(t\u22121): 32',palette$teal_fill,palette$teal,12)
  box(410,1610,340,390,'Separate learned affine maps\nfor g in {reset r, update z, new n}\n\na_g = W_ig u(t) + b_ig\n\nc_g = W_hg h(t\u22121) + b_hg\n\nEach a_g and c_g has 32 values',palette$blue_fill,palette$blue,12)
  box(850,1440,300,100,'UPDATE gate\nz(t) = sigmoid(a_z + c_z)',palette$teal_fill,palette$teal,13)
  box(850,1600,300,100,'RESET gate\nr(t) = sigmoid(a_r + c_r)',palette$teal_fill,palette$teal,13)
  box(850,1760,300,100,'CANDIDATE state\nn(t) = tanh(a_n + r(t) \u2299 c_n)',palette$teal_fill,palette$teal,12)
  box(1290,1520,310,190,'Mix candidate and old state\n\nh(t) = (1\u2212z(t)) \u2299 n(t)\n          + z(t) \u2299 h(t\u22121)',palette$teal_fill,palette$teal,13)
  box(1560,1520,150,100,'New state\nh(t): 32',palette$teal_fill,palette$teal,13,TRUE)
  label(1067,1417,'z(t)',10,color=palette$teal)
  label(958,1699,'r(t)',10,color=palette$teal)
  label(1055,1782,'n(t)',10,color=palette$teal)
  label(785,1870,'Old state: identity carry path',11,color=palette$teal)
  label(900,1940,'\u2299 means elementwise multiplication. The reset gate multiplies the full recurrent candidate projection, including its bias.',11)
  label(900,1977,'All embedding, GRU and head weights are learned jointly through backpropagation. The diagram shows their forward use.',11,color=palette$muted)
  label(900,2014,'The head consumes the previous top-layer state; it never consumes the current answer or the newly updated state.',11,color=palette$muted)
  label(900,2051,paste0('Architecture from the original 2.0.0.9009 checkpoint; dimensions, equations and real-history replay verified using maintained LKT ',lkt_release[['lkt_version']],'.'),10,color=palette$muted)
  label(900,2088,'Source: R/lkt-model-gru.R and R/lkt-gru-prediction.R. Original fitted weights and historical results were not modified.',10,color=palette$muted)
}

# Export an overview plus a full version with cell details. All formats use the
# same drawing and exact dimensions; the original 72-item figure remains untouched.
for(detail in c(FALSE,TRUE)) {
  H <- if(detail)2130 else 1260
  stem <- if(detail)'gru-network' else 'gru-network-overview'
  width_inches <- 18;height_inches <- width_inches*H/W
  svglite::svglite(file.path(output,paste0(stem,'.svg')),width=width_inches,height=height_inches,bg='white')
  draw(detail);grDevices::dev.off()
  grDevices::cairo_pdf(file.path(output,paste0(stem,'.pdf')),width=width_inches,height=height_inches,family='DejaVu Sans')
  draw(detail);grDevices::dev.off()
  grDevices::png(file.path(output,paste0(stem,'.png')),width=width_inches,height=height_inches,
    units='in',res=140,type='cairo',bg='white')
  draw(detail);grDevices::dev.off()
}
cat('PASS embeddings, network dimensions, Torch gate equations, head equation, answer causality and real-history replay.\n')
print(audit)
cat('Figure exports:',normalizePath(output,winslash='/'),'\n')
