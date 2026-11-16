# Generic structural figure; no dataset or fitted checkpoint is loaded.
# Args: ROOT OUTPUT [LIBRARY] [consolidated|default]
# The optional library is an explicit, isolated verification build, never an
# alternate runtime selected automatically. Without it use the central release.
args <- commandArgs(TRUE);stopifnot(length(args) %in% 2:4)
layout <- if(tail(args,1L) %in% c('consolidated','default'))tail(args,1L) else 'detailed'
has_library <- length(args)>=3L && !args[3L] %in% c('consolidated','default')
consolidated <- layout %in% c('consolidated','default')
default_layout <- layout=='default'
root <- normalizePath(args[1],winslash='/',mustWork=TRUE)
output <- normalizePath(args[2],winslash='/',mustWork=FALSE)
if(has_library) {
  .libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
  library('LKT',lib.loc=normalizePath(args[3],winslash='/',mustWork=TRUE),character.only=TRUE)
} else source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-simplified-alternative.R'))
source(file.path(root,'scripts/gru-previous-default.R'))
s <- if(default_layout)GRUModel()$settings else gru_simplified_alternative()$settings
figure_title <- if(default_layout)'Default GRU network: connected view' else 'GRU alternative: connected, consolidated view'
stopifnot(s$hidden_size==32L,s$num_layers==3L,identical(s$head_hidden_sizes,128L),
  s$numeric_encoder=='combined',identical(s$encoder_output_sizes,c(numeric=32L)),
  s$response_encoding=='binary',s$retrieval_value_mode=='raw',s$retrieval_heads==1L,
  s$retrieval_size==32L,s$retrieval_dropout==0,s$encoder_dropout==0,
  s$input_dropout==.1,s$gru_dropout==.1,s$head_dropout==.1,
  s$inference_mode=='none',!s$input_to_all_layers,s$retrieval_share_qk,
  s$retrieval_bias,s$gru_bias,s$head_bias,is.null(s$joint_encoder_output_size),
  s$numeric_fusion=='concat',s$categorical_fusion=='concat',s$input_fusion=='concat',
  identical(s$embedding_dims,16L),identical(s$encoder_hidden_sizes,32L),
  identical(s$retrieval_filter_cols,c('KC..Default.','KC..Cluster.')),
  identical(s$zero_init_embedding_cols,'KC..Default.'),s$retrieval_window==100L,
  s$retrieval_fusion=='replace',s$retrieval_decay=='none',!length(s$retrieval_cue_cols))
stopifnot(requireNamespace('svglite',quietly=TRUE))
numeric_count <- if(default_layout)6L else 7L
stopifnot(if(default_layout)is.null(s$time_col) else identical(s$time_col,'CF..Time.'))
numeric_weight_count <- 32L*numeric_count
fixed_entries <- 40193L-32L*(7L-numeric_count)
numeric_input_label <- paste0(numeric_count,' numeric inputs; no intercept\n',
  if(numeric_count==7L)'Recency; ' else '',
  'item age; prior mean spacing; spacing-availability flag\n',
  'Student logitdec; student-item logitdec; student-item PPES')
numeric_encoder_label <- paste0('Shared numeric encoder: Linear ',numeric_count,' \u2192 32; ReLU\n32 biases')
dir.create(output,recursive=TRUE,showWarnings=FALSE)
# Small structural schemas check the complete tensor-entry formula for arbitrary
# category counts. They initialize networks only; there is no fit or data input.
network <- getFromNamespace('lkt_gru_network','LKT')
weights <- getFromNamespace('lkt_gru_weights','LKT')
make_prep <- function(items,skills,intercept=FALSE,numeric_count=7L)list(
  numeric_names=c(paste0('time:',seq_len(numeric_count-3L)),if(intercept)'lkt:(Intercept)',paste0('lkt:',1:3)),
  embedding_dims=c(16L,16L),vocabularies=list(
    KC..Default.=paste0('i',seq_len(items)),KC..Cluster.=paste0('s',seq_len(skills))))
for(counts in list(c(2L,3L),c(7L,5L))) {
  net <- network(make_prep(counts[1],counts[2],numeric_count=numeric_count),s)
  stopifnot(sum(vapply(weights(net),length,integer(1)))==fixed_entries+16L*sum(counts+1L))
  old <- gru_previous_default()$settings
  old_net <- network(make_prep(counts[1],counts[2],TRUE),old)
  stopifnot(sum(vapply(weights(old_net),length,integer(1)))==43345L+16L*sum(counts+1L))
}
# Only architecture, not synthetic vocabulary sizes, is exported as provenance.
saveRDS(s,file.path(output,'architecture-settings.rds'),version=3)
connection_counts <- data.frame(connection=c('numeric input to hidden','numeric hidden to output',
  'shared query/key','retrieval output','head hidden','head logit','GRU layer 1 input',
  'GRU layer 2 input','GRU layer 3 input','each GRU recurrence'),
  weights=c(numeric_weight_count,1024L,3072L,1024L,12288L,128L,6240L,3072L,3072L,3072L),
  biases=c(32L,32L,32L,32L,128L,1L,192L,192L,192L,0L),
  copies=c(rep(1L,9L),3L))
# Each GRU's two 96-entry bias vectors are counted once on its input row.
stopifnot(sum((connection_counts$weights+connection_counts$biases)*connection_counts$copies)==fixed_entries)
write.csv(connection_counts,file.path(output,'connection-parameters.csv'),row.names=FALSE)
writeLines(c(if(default_layout)'Current default GRU architecture; no training or dataset loaded.' else 'Generic simplified GRU alternative; no training or dataset loaded.',
  'Weights on connectors; biases in receiving boxes. Reused weights count once.',
  'I and S denote learned training-vocabulary item and skill-combination categories.',
  'Embedding tables also have one reserved, fixed-zero row each.',
  paste0('Learnable scalar entries: ',format(fixed_entries,big.mark=','),' + 16(I + S).'),
  paste0('Stored tensor entries including reserved rows: ',format(fixed_entries,big.mark=','),' + 16(I + S + 2).'),
  paste0('Previous separate-encoder design: 43,345 + 16(I + S + 2) stored entries; reduction ',43345L-fixed_entries,'.'),
  paste(numeric_count,'inputs assume the stated three-feature LKT recipe and',numeric_count-3L,'timing coordinates.'),
  'interc=FALSE (configuration intercept=FALSE) removes the global intercept.',
  paste('Source draw script SHA256:',digest::digest(file=file.path(root,'scripts/draw-gru-simplified-network.R'),algo='sha256')),
  if(consolidated)paste('Consolidated layout SHA256:',digest::digest(file=file.path(root,'scripts/gru-consolidated-layout.R'),algo='sha256')),
  paste('Source model SHA256:',digest::digest(file=file.path(root,'R/lkt-model-gru.R'),algo='sha256')),
  paste('Verified runtime:',getNamespaceInfo(asNamespace('LKT'),'path'))),
  file.path(output,'provenance-and-verification.txt'))
W <- 1800;H <- 2600
p <- list(ink='#183247',muted='#526477',blue='#246BA0',blue_fill='#EAF3FC',
  teal='#137C73',teal_fill='#E6F4F0',purple='#7655A3',purple_fill='#F2EDFA',
  orange='#B96024',orange_fill='#FFF1E6',red='#A34344',red_fill='#FBEFF0')
layout_checks <- list()
label <- function(x,y,text,size=12,bold=FALSE,color=p$ink,just='centre')grid::grid.text(text,
  x=x/W,y=1-y/H,just=just,gp=grid::gpar(fontfamily='DejaVu Sans',fontsize=size,
    fontface=if(bold)'bold' else 'plain',col=color,lineheight=1.12))
box <- function(x,y,width,height,text,fill,color,size=12,bold=FALSE) {
  grid::grid.roundrect(x=x/W,y=1-y/H,width=width/W,height=height/H,r=grid::unit(5,'pt'),
    gp=grid::gpar(fill=fill,col=color,lwd=1.1))
  grob <- grid::textGrob(text,gp=grid::gpar(fontfamily='DejaVu Sans',fontsize=size,
    fontface=if(bold)'bold' else 'plain',lineheight=1.12))
  extent <- c(grid::convertWidth(grid::grobWidth(grob),'in',valueOnly=TRUE),
    grid::convertHeight(grid::grobHeight(grob),'in',valueOnly=TRUE))
  capacity <- c(width/W*20,height/H*20*H/W)
  stopifnot(all(extent+0.12<capacity))
  layout_checks[[length(layout_checks)+1L]] <<- data.frame(text=gsub('\n',' / ',text),
    width_in=extent[1],height_in=extent[2],box_width_in=capacity[1],box_height_in=capacity[2])
  label(x,y,text,size,bold)
}
edge <- function(x,y,color=p$teal,dashed=FALSE,arrow=TRUE)grid::grid.lines(x=x/W,y=1-y/H,
  arrow=if(arrow)grid::arrow(length=grid::unit(.07,'in'),type='closed') else NULL,
  gp=grid::gpar(col=color,fill=color,lwd=1.25,lineend='round',lty=if(dashed)'dashed' else 'solid'))
tag <- function(x,y,text='0',color=p$muted,size=10) {
  g <- grid::textGrob(text,gp=grid::gpar(fontfamily='DejaVu Sans',fontsize=size))
  grid::grid.rect(x=x/W,y=1-y/H,width=grid::grobWidth(g)+grid::unit(7,'pt'),
    height=grid::grobHeight(g)+grid::unit(4,'pt'),gp=grid::gpar(fill='white',col=NA))
  label(x,y,text,size,color=color)
}
draw <- function() {
  grid::grid.newpage()
  label(900,34,'GRU alternative: one numeric encoder + consolidated retrieval',21,TRUE)
  label(900,72,'Generic architecture  |  Three GRU layers of 32 units  |  One 128-unit prediction head',12)
  label(55,117,'A   Encode the predictors available before response t',15,TRUE,just='left')
  box(420,205,700,125,numeric_input_label,p$blue_fill,p$blue,12,TRUE)
  box(1080,205,360,90,'Current item ID',p$blue_fill,p$blue,14,TRUE)
  box(1510,205,430,90,'Current skill-combination ID',p$blue_fill,p$blue,13,TRUE)
  edge(c(420,420),c(267.5,335),p$orange);tag(420,298,paste(numeric_weight_count,'weights'),p$orange)
  box(420,377,700,84,numeric_encoder_label,p$orange_fill,p$orange,13,TRUE)
  edge(c(420,420),c(419,470),p$orange);tag(420,445,'1,024 weights',p$orange)
  box(420,512,700,84,'Numeric output: Linear 32 \u2192 32\n32 biases',p$orange_fill,p$orange,13,TRUE)
  for(xx in c(1080,1510))edge(c(xx,xx),c(250,360),p$blue)
  tag(1080,301,'16 \u00d7 I learned table values',p$blue)
  tag(1510,301,'16 \u00d7 S learned table values',p$blue)
  box(1080,440,360,160,'Item embedding\n16 values per item\nStarts at zero; trainable\nReserved row stays zero',p$blue_fill,p$blue,12,TRUE)
  box(1510,440,430,160,'Skill-combination embedding\n16 values per combination\nRandom start; trainable\nReserved row stays zero',p$blue_fill,p$blue,12)
  for(xx in c(420,1080,1510)) {
    edge(c(xx,xx),c(if(xx==420)554 else 520,600),p$blue)
    tag(xx,577,'0')
  }
  box(900,645,1600,90,'Concatenate 32 numeric + 16 item + 16 skill = 64 values\nInput dropout 0.1 produces x(t), shared by retrieval, prediction and memory update',p$blue_fill,p$blue,13,TRUE)
  label(900,721,'Connector labels count weights; receiving boxes list biases. I and S are learned category counts, not fixed dataset sizes.',11,color=p$muted)
  edge(c(100,22,22),c(645,645,2120),p$blue,arrow=FALSE)
  for(y in c(850,1550,2120)) {
    edge(c(22,if(y==850)65 else if(y==1550)55 else 100),c(y,y),p$blue)
    tag(if(y==850)44 else if(y==1550)39 else 62,y,'0',size=9)
  }
  label(55,777,'B   Filter and retrieve completed memories: one head, width 32',15,TRUE,just='left')
  box(300,850,470,100,'Current event e(t): 96 values\nPrevious h3(t\u22121): 32 + x(t): 64\nConcatenation: 0 parameters',p$teal_fill,p$teal,12,TRUE)
  box(1155,850,1100,100,'Past completed events from this participant\nNewest 100 global events: e(j), post-answer h3(j), raw item/skill IDs',p$teal_fill,p$teal,12)
  box(1155,927,1100,42,'Keep same item OR same skill-combination ID; compare exact raw IDs (0 parameters)',p$teal_fill,p$teal,11,TRUE)
  edge(c(300,300),c(900,1015),p$red);tag(300,976,'3,072 weights*',p$red)
  edge(c(930,930),c(948,1015),p$red);tag(930,978,'3,072 shared weights*',p$red)
  edge(c(1500,1500),c(948,1015),p$red);tag(1500,978,'0')
  box(300,1067,430,104,'Shared Q/K linear 96 \u2192 32\nQuery q(t); 32 shared biases*',p$red_fill,p$red,13)
  box(930,1067,470,104,'Same Q/K linear 96 \u2192 32\nKey k(j); same 32 biases*',p$red_fill,p$red,13)
  box(1500,1067,390,104,'Raw values v(j) = h3(j)\n32 values; no value map',p$red_fill,p$red,12,TRUE)
  edge(c(300,300,370),c(1119,1215,1215),p$red);tag(300,1170,'0')
  edge(c(930,930,650,650),c(1119,1150,1150,1170),p$red);tag(790,1150,'0')
  edge(c(1500,1500),c(1119,1170),p$red);tag(1500,1146,'0')
  box(650,1230,560,120,'Scores q(t) \u00b7 k(j) / sqrt(32)\nPast 100 AND (same item OR skill)\nSoftmax over eligible events only\n0 parameters',p$red_fill,p$red,12,TRUE)
  edge(c(930,1100),c(1230,1230),p$red);tag(1015,1230,'0')
  box(1400,1230,600,120,'Weighted mean: m(t) = \u03a3 a(t,j) h3(j)\n32 values; 0 parameters',p$red_fill,p$red,13,TRUE)
  edge(c(1400,1400),c(1290,1355),p$red);tag(1400,1322,'1,024 weights',p$red)
  box(1400,1405,600,100,'Single output linear 32 \u2192 32; 32 biases\nRetrieved r(t): 32 values\nNo eligible memory: force r(t) = 0',p$red_fill,p$red,12,TRUE)
  label(55,1360,'* Q/K is one shared map: 3,072 weights + 32 biases total.',11,color=p$muted,just='left')
  label(55,1397,'No learned attention decay; no attention dropout.',11,color=p$muted,just='left')
  label(55,1468,'C   Predict the current response; the answer is still unknown',15,TRUE,just='left')
  edge(c(1400,1400,280,280),c(1455,1487,1487,1500),p$red);tag(845,1487,'0')
  box(280,1550,450,100,'r(t): 32 + x(t): 64\nConcatenate 96 values\n0 parameters',p$purple_fill,p$purple,13)
  edge(c(505,640),c(1550,1550),p$purple);tag(572,1550,'12,288 weights',p$purple,9)
  box(850,1550,420,100,'Linear 96 \u2192 128; 128 biases\nReLU; head dropout 0.1',p$purple_fill,p$purple,13,TRUE)
  edge(c(1060,1155),c(1550,1550),p$purple);tag(1108,1550,'128 weights',p$purple,9)
  box(1265,1550,220,100,'Linear 128 \u2192 1\n1 bias; one logit',p$purple_fill,p$purple,12)
  edge(c(1375,1450),c(1550,1550),p$purple);tag(1412,1550,'0')
  box(1590,1550,280,100,'Sigmoid + clipping\np(correct at trial t)\n0 parameters',p$purple_fill,p$purple,12,TRUE)
  label(900,1650,'Retrieved memory replaces direct h3(t\u22121) input to the head. Prediction never uses the current answer.',12,color=p$purple)
  label(55,1720,'D   Observe the binary answer, update memory, then write the bank',15,TRUE,just='left')
  box(330,1815,460,90,'Observed response y(t)\n0 = incorrect; 1 = correct\nNo response encoder',p$orange_fill,p$orange,13,TRUE)
  edge(c(330,330),c(1860,2065),p$orange);tag(330,1945,'0: raw binary value',p$orange)
  for(j in 1:3) {
    xx <- c(780,1180,1580)[j]
    box(xx,1825,250,95,paste0('Previous h',j,'(t\u22121)\n32 state values'),p$teal_fill,p$teal,12)
    edge(c(xx,xx),c(1872.5,2055));tag(xx,1938,'3,072 recurrent weights',p$teal,9)
    box(xx,2120,250,130,paste0('GRU layer ',j,'; 32 units\nh',j,'(t): 32 values\n192 biases (two sets of 96)'),p$teal_fill,p$teal,12,TRUE)
    edge(c(xx,xx),c(2185,2240));tag(xx,2214,'0')
  }
  box(330,2120,460,110,'Current x(t): 64 + raw y(t): 1\n65 values into GRU layer 1\nConcatenation: 0 parameters',p$teal_fill,p$teal,12)
  edge(c(560,655),c(2120,2120));tag(607,2090,'6,240 weights',p$teal,9)
  for(xx in c(980,1380)) {
    edge(c(xx-75,xx+75),c(2120,2120));tag(xx,2070,'3,072 weights',p$teal,9)
    box(xx,2120,140,65,'Dropout\n0.1; 0 params',p$teal_fill,p$teal,10)
  }
  box(900,2285,1600,90,'Keep h1(t), h2(t), h3(t) for the next trial: 3 \u00d7 32 state values\nAppend [pre-answer e(t): 96, post-answer h3(t): 32, raw IDs] to the newest-100-event bank; 0 parameters',p$teal_fill,p$teal,12,TRUE)
  edge(c(1700,1778,1778,1705),c(2285,2285,850,850),p$teal,dashed=TRUE);tag(1778,1950,'0')
  label(900,2380,'All weights learn jointly from response-prediction loss. The seven-input encoder replaces the separate time/history networks.',12)
  label(900,2425,'At a new participant: zero GRU states and an empty bank; retain trained embeddings. Retrieval feeds prediction only.',11)
  label(900,2468,'Timing uses log(1 + seconds); numeric scaling and vocabularies use training participants only. Missing spacing has an availability flag.',10,color=p$muted)
  label(900,2510,'Only completed scored responses update memory. Dropout is off for evaluation; gradients detach at 100-event BPTT chunk boundaries.',10,color=p$muted)
  label(900,2555,'Architecture widths are fixed for this alternative. Item and skill-combination table sizes depend on the training vocabulary.',10,color=p$muted)
}
stem <- 'gru-simplified-network'
if(consolidated) {
  source(file.path(root,'scripts/gru-consolidated-layout.R'),local=TRUE)
  W <- 1800;H <- 2300;draw <- draw_gru_consolidated
  stem <- if(default_layout)'gru-default-network' else 'gru-consolidated-network'
}
width <- 20;height <- width*H/W
svglite::svglite(file.path(output,paste0(stem,'.svg')),width=width,height=height,bg='white');draw();grDevices::dev.off()
grDevices::cairo_pdf(file.path(output,paste0(stem,'.pdf')),width=width,height=height,family='DejaVu Sans');draw();grDevices::dev.off()
grDevices::png(file.path(output,paste0(stem,'.png')),width=width,height=height,units='in',res=120,type='cairo',bg='white');draw();grDevices::dev.off()
write.csv(do.call(rbind,layout_checks),file.path(output,'layout-check.csv'),row.names=FALSE)
cat('PASS: generic architecture, both symbolic parameter formulas, connection counts and text bounds. No fit performed.\n')
