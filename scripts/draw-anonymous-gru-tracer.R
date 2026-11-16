# MODEL_RDS OUTPUT_DIRECTORY. Draw from an actual smoke or fitted tracer artifact.
# This is a state/parameter contrast, not a complete execution-order diagram.
a<-commandArgs(TRUE);stopifnot(length(a)==2L)
x<-readRDS(a[1]);fit<-x$fit;s<-fit$gru_settings
skills<-identical(s$categorical_cols,c('KC..Default.','KC..Cluster.'))
stopifnot(s$embedding_adaptation=='per_student',s$plasticity=='constant',s$inference_mode=='none',
 (skills || identical(s$categorical_cols,'KC..Default.')),length(fit$model$preprocessing$numeric_names)==6L,
 s$hidden_size==32L,s$num_layers==3L,identical(s$head_hidden_sizes,128L),
 identical(s$retrieval_filter_cols,s$categorical_cols),identical(s$adaptive_embedding_cols,s$categorical_cols))
out<-a[2];dir.create(out,recursive=TRUE,showWarnings=FALSE)
draw<-function() {
 par(mar=rep(0,4),family='sans',xaxs='i',yaxs='i');plot.new();plot.window(c(0,1),c(0,1))
 ink<-'#172A3A';edge<-'#617584';neutral<-'#EDF2F6';local<-'#E8F2EE';new<-'#FFF0D7'
 text(.5,.965,if(skills)'Same GRU, fresh student, item and skill state' else 'Same GRU, fresh student and item state',cex=1.5,font=2,col=ink)
 text(.5,.918,'Six baseline values retained; no Lasso inputs or new proficiency-summary network',cex=.98,col=ink)
 box<-function(x,y,w,h,label,fill=neutral,cex=1.04) {
  rect(x-w/2,y-h/2,x+w/2,y+h/2,col=fill,border='#A7B6C2',lwd=1.15)
  text(x,y,label,cex=cex,col=ink)
 }
 text(.5,.857,'SHARED WEIGHTS: LEARNED ON TRAINING STUDENTS, RETAINED FOR TEST STUDENTS',font=2,cex=.99,col=ink)
 box(.14,.752,.23,.13,'Numeric-input\nprocessing network')
 box(.38,.752,.21,.13,'GRU connections\n3 layers of 32')
 box(.62,.752,.21,.13,'Retrieval\nconnections')
 box(.86,.752,.23,.13,'Prediction connections\n+ constant output rate',cex=.99)
 text(.5,.644,'The shared weights operate on each student\'s own temporary state',cex=1.12,col=ink)
 arrows(.5,.615,.5,.565,length=.08,lwd=1.5,col=edge)
 text(.5,.535,'RESET THESE AT EVERY NEW STUDENT: IN BOTH TRAINING AND TESTING',font=2,cex=1.02,col=ink)
 box(.18,.375,.29,.235,'EXISTING STUDENT STATE\n\nGRU memory + retrieval\nstart at zero / empty\n\nUpdate after observed answers',fill=local,cex=.99)
 box(.50,.375,.29,.235,if(skills)'CHANGED ITEM / SKILL VECTORS\n\nA vector per item and marked skill\nBoth start at zero for this student\n\nLearn only from this student' else
  'CHANGED ITEM CONNECTIONS\n\nA local vector for each item\nstarts at zero for this student\n\nLearn only from this student',fill=new,cex=.96)
 box(.82,.375,.29,.235,'EXISTING OUTPUT LEARNING\n\nStudent output adjustments\nstart at zero\n\nUpdate at the learned constant rate',fill=local,cex=.99)
 text(.5,.218,'Predict first, observe the answer, update personal state, then predict the next answer',cex=1.04,col=ink)
 box(.5,.13,.80,.095,if(skills)'RETAINED: supplied skill labels and item-or-skill retrieval matching\nREPLACED: pretrained item/skill vectors with fresh, student-local vectors' else
  'REMOVED: supplied skill lookup and skill-based retrieval matching\nStudent ID selects the personal state; there is no pretrained student embedding',
  fill=if(skills)local else '#F8ECEB',cex=.97)
 text(.5,.042,'Six values: student/item response histories, PPES memory, item age, prior spacing and spacing availability.',cex=.85,col=edge)
}
png(file.path(out,'anonymous-gru-tracer.png'),width=1800,height=1200,res=165)
draw();dev.off()
svg(file.path(out,'anonymous-gru-tracer.svg'),width=10.91,height=7.27)
draw();dev.off()
writeLines(c('Figure verified against actual tracer settings and six-input preprocessing.',
 paste('Source artifact:',normalizePath(a[1],winslash='/')),
 if(skills)'Orange: item and skill lookups replaced by student-local vectors; supplied grouping and retrieval matching retained.' else
  'Orange: item lookup replaced by student-local vectors. Red: supplied skill information removed.',
 'Green: existing student-local state and constant output learning, retained.',
 'Gray: shared learned parameters retained across students; fixed during evaluation.'),file.path(out,'provenance.txt'))
