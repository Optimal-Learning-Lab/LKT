# Connected view: merge parts of the same calculation, but retain explicit
# arrows for information passed between calculations. Unlabeled arrows carry
# information without learned parameters; dashed arrows cross to the next trial.
draw_gru_consolidated <- function() {
  grid::grid.newpage()
  diagram_nodes <<- character();diagram_edges <<- list()
  node <- function(id,x,y,width,height,text,fill,color,size=12,bold=FALSE) {
    diagram_nodes <<- c(diagram_nodes,id)
    box(x,y,width,height,text,fill,color,size,bold)
  }
  wire <- function(from,to,x,y,color=p$teal,weights=NULL,tx=NULL,ty=NULL,dashed=FALSE,size=10) {
    diagram_edges[[length(diagram_edges)+1L]] <<- list(from=from,to=to,
      weights=if(is.null(weights))'information only' else weights,next_trial=dashed)
    edge(x,y,color,dashed=dashed)
    if(!is.null(weights))tag(tx,ty,weights,color,size)
  }
  dot <- function(x,y)grid::grid.circle(x=x/W,y=1-y/H,r=grid::unit(2,'pt'),
    gp=grid::gpar(fill=p$blue,col=p$blue))
  label(900,35,figure_title,22,TRUE)
  label(900,77,'Labeled arrows: learned weights. Unlabeled arrows: information flow. Biases are listed with their outputs.',12,color=p$muted)
  label(65,112,'A   Encode current predictors and concatenate their representations',15,TRUE,just='left')
  node('numeric_inputs',420,200,700,120,numeric_input_label,p$blue_fill,p$blue,12,TRUE)
  node('item_id',1080,200,360,90,'Current item ID',p$blue_fill,p$blue,13,TRUE)
  node('skill_id',1510,200,430,90,'Current skill-combination ID',p$blue_fill,p$blue,13,TRUE)
  node('numeric_hidden',420,365,700,90,numeric_encoder_label,p$orange_fill,p$orange,13,TRUE)
  wire('numeric_inputs','numeric_hidden',c(420,420),c(260,320),p$orange,paste(numeric_weight_count,'weights'),420,290)
  node('predictors',900,550,1620,180,paste0('Current predictors x(t): concatenate 32 numeric + 16 item + 16 skill = 64 values\n',
    'Numeric output: Linear 32 \u2192 32; 32 biases\n',
    'Item embedding: 16 values per item, initialized at zero; skill-combination embedding: 16 values, random start\n',
    'Both tables train; reserved rows stay zero. Apply input dropout 0.1 after concatenation.\n',
    'Carry exact raw item and skill-combination IDs alongside x(t).'),p$blue_fill,p$blue,12,TRUE)
  wire('numeric_hidden','predictors',c(420,420),c(410,460),p$orange,'1,024 weights',420,435)
  wire('item_id','predictors',c(1080,1080),c(245,460),p$blue,'16 \u00d7 I table values',1080,350)
  wire('skill_id','predictors',c(1510,1510),c(245,460),p$blue,'16 \u00d7 S table values',1510,350)
  # Crossings without dots are not joins on this explicit current-input bus.
  edge(c(90,35,35),c(550,550,1900),p$blue,arrow=FALSE)
  for(y in c(830,1430,1900))dot(35,y)
  label(65,710,'B   Retrieve completed memories: one attention head, width 32',15,TRUE,just='left')
  node('current_event',300,830,470,110,'Current event e(t): 96 values\nPrevious h3(t\u22121): 32 + x(t): 64\nCarry raw item/skill IDs alongside e(t).',p$teal_fill,p$teal,12,TRUE)
  wire('predictors','current_event',c(35,65),c(830,830),p$blue)
  node('bank',1155,830,1100,130,paste0('Past completed events from this participant; newest 100 globally\n',
    'Stored e(j): 96, post-answer h3(j): 32, exact raw item/skill IDs\n',
    'FILTER: past item = current item OR past skill-combination ID = current ID'),p$teal_fill,p$teal,12,TRUE)
  wire('predictors','bank',c(1510,1510),c(640,765),p$blue)
  tag(1510,681,'current raw IDs',p$blue)
  node('attention',900,1120,1620,200,paste0('Attention and weighted mean m(t): 32 values\n',
    'Current q(t) and past k(j): shared Linear 96 \u2192 32; 32 shared biases\n',
    'Scores q(t) \u00b7 k(j) / sqrt(32); softmax over eligible past events only\n',
    'Weighted mean \u03a3 a(t,j) h3(j) of their raw post-answer states; no separate value map'),p$red_fill,p$red,13,TRUE)
  wire('current_event','attention',c(300,300),c(885,1020),p$red,'3,072 weights*',300,955)
  wire('bank','attention',c(1000,1000),c(895,1020),p$red,'3,072 shared weights*',1000,955)
  wire('bank','attention',c(1570,1570),c(895,1020),p$teal)
  tag(1570,955,'raw h3(j)',p$teal)
  label(65,1263,'* Q/K map shared: 3,072 weights + 32 biases (count once).',11,color=p$muted,just='left')
  label(1050,1263,'No attention decay or attention dropout.',11,color=p$muted,just='left')
  label(65,1315,'C   Predict the current response; the answer is still unknown',15,TRUE,just='left')
  node('prediction_input',300,1430,470,140,paste0('Prediction input: 96 values\n',
    'Retrieved r(t): Linear 32 \u2192 32; 32 biases\n',
    'No eligible memory: force r(t) = 0\n',
    'Concatenate r(t): 32 + x(t): 64'),p$purple_fill,p$purple,12,TRUE)
  wire('attention','prediction_input',c(900,900,300,300),c(1220,1340,1340,1360),p$red,'1,024 weights',650,1340)
  wire('predictors','prediction_input',c(35,65),c(1430,1430),p$blue)
  node('prediction_hidden',850,1430,400,140,'Prediction hidden layer: 128\nLinear 96 \u2192 128; 128 biases\nReLU; head dropout 0.1',p$purple_fill,p$purple,13,TRUE)
  wire('prediction_input','prediction_hidden',c(535,650),c(1430,1430),p$purple,'12,288 weights',592,1430,size=9)
  node('probability',1480,1430,500,140,'Correctness probability p(correct at trial t)\nLinear 128 \u2192 1; 1 bias\nSigmoid and clipping',p$purple_fill,p$purple,13,TRUE)
  wire('prediction_hidden','probability',c(1050,1230),c(1430,1430),p$purple,'128 weights',1140,1430)
  label(65,1580,'D   Observe the answer, update all three layers, then store the completed event',15,TRUE,just='left')
  node('answer',300,1690,470,90,'Observed response y(t)\n0 = incorrect; 1 = correct',p$orange_fill,p$orange,13,TRUE)
  node('interaction',300,1900,470,110,'Completed interaction: 65 values\nConcatenate x(t): 64 + raw y(t): 1\nNo response encoder',p$teal_fill,p$teal,12,TRUE)
  wire('answer','interaction',c(300,300),c(1735,1845),p$orange)
  wire('predictors','interaction',c(35,65),c(1900,1900),p$blue)
  for(j in 1:3) {
    xx <- c(800,1200,1600)[j]
    node(paste0('previous_',j),xx,1690,250,90,paste0('Previous h',j,'(t\u22121)\n32 state values'),p$teal_fill,p$teal,12)
    node(paste0('gru_',j),xx,1900,250,130,paste0('GRU layer ',j,': 32 units\n192 biases; h',j,'(t): 32',
      if(j<3)'\nThen dropout 0.1\nto the next layer' else '\nTop completed state'),p$teal_fill,p$teal,12,TRUE)
    wire(paste0('previous_',j),paste0('gru_',j),c(xx,xx),c(1735,1835),p$teal,'3,072 recurrent weights',xx,1790,size=9)
    if(j==1)wire('interaction','gru_1',c(535,675),c(1900,1900),p$teal,'6,240 weights',605,1900,size=9)
    if(j<3)wire(paste0('gru_',j),paste0('gru_',j+1),c(xx+125,xx+275),c(1900,1900),p$teal,'3,072 weights',xx+200,1900,size=9)
  }
  node('retained',900,2130,1620,120,paste0('Retain all three states h1(t), h2(t), h3(t) for the next trial\n',
    'Append [cached pre-answer e(t): 96, post-answer h3(t): 32, raw item/skill IDs] to the bank\n',
    'Keep the newest 100 global completed events; retained GRU states are before between-layer dropout.'),p$teal_fill,p$teal,12,TRUE)
  for(j in 1:3) {
    xx <- c(800,1200,1600)[j]
    wire(paste0('gru_',j),'retained',c(xx,xx),c(1965,2070))
  }
  # Cache the pre-answer event for the current trial's post-answer bank write.
  wire('current_event','retained',c(65,55,55,400,400),c(850,850,2030,2030,2070),p$teal)
  tag(400,2030,'cache e(t) + raw IDs',p$teal)
  # Explicit next-trial feedback reconnects recurrent and retrieval memory.
  wire('retained','bank',c(1710,1785,1785,1705),c(2130,2130,830,830),p$teal,dashed=TRUE)
  wire('retained','current_event',c(90,10,10,300,300),c(2150,2150,750,750,775),p$teal,dashed=TRUE)
  tag(210,750,'next trial: h3(t)',p$teal,size=9)
  edge(c(1710,1750,1750,800),c(2100,2100,1620,1620),p$teal,dashed=TRUE,arrow=FALSE)
  for(j in 1:3) {
    xx <- c(800,1200,1600)[j]
    wire('retained',paste0('previous_',j),c(xx,xx),c(1620,1645),p$teal,dashed=TRUE)
  }
  label(900,2235,'Unlabeled arrows pass information; dashed arrows carry state or bank records to the next trial. Crossings without dots are not joins.',11,color=p$muted)
  label(900,2270,'I and S are learned category counts. Training-only scaling/vocabularies; dropout off for evaluation; new participants start with zero states and an empty bank.',10,color=p$muted)
  # Audit the actual drawing graph. Repeated names cannot replace connections.
  stopifnot(!anyDuplicated(diagram_nodes),all(vapply(diagram_edges,function(e)
    e$from %in% diagram_nodes && e$to %in% diagram_nodes,logical(1))))
  reachable <- function(start,directed=TRUE) {
    reached <- start
    repeat {
      next_nodes <- unique(unlist(lapply(diagram_edges,function(e) {
        c(if(e$from %in% reached)e$to,if(!directed && e$to %in% reached)e$from)
      })))
      extended <- union(reached,next_nodes)
      if(length(extended)==length(reached))return(reached)
      reached <- extended
    }
  }
  stopifnot(setequal(reachable('numeric_inputs',FALSE),diagram_nodes),
    all(c('current_event','prediction_input','interaction','probability','retained','bank') %in% reachable('predictors')),
    'probability' %in% reachable('answer'))
  current_edges <- Filter(function(e)!e$next_trial,diagram_edges)
  reached <- 'answer'
  repeat {
    extended <- union(reached,unlist(lapply(current_edges,function(e)if(e$from %in% reached)e$to)))
    if(length(extended)==length(reached))break
    reached <- extended
  }
  stopifnot(!'probability' %in% reached)
  jsonlite::write_json(list(nodes=diagram_nodes,edges=diagram_edges,
    checks=list(all_nodes_connected=TRUE,explicit_predictor_branches=TRUE,
      answer_reaches_only_future_predictions=TRUE)),
    file.path(output,'connectivity-check.json'),auto_unbox=TRUE,pretty=TRUE)
}
