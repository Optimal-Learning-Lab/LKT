library(LKT)
frame <- getFromNamespace('lkt_gru_feature_frame','LKT')
unsupported <- c('logitdecevol','errordec','diffrelcor1','diffrelcor2','diffcor1','diffcor2',
  'diffcorComp','diffincorComp','diffallComp','diffincor1','diffincor2','diffall1','diffall2')
registry <- lkt_features()
d <- data.frame(Anon.Student.Id=rep(c('s1','s2'),each=12),KC..Default.=rep(c('a','b'),12),
  CF..ansbin.=rep(c(1,0,0,1),6),CF..Time.=rep(seq(0,by=100,length.out=12),2),
  CF..reltime.=rep(seq(0,by=100,length.out=12),2),value=seq_len(24))
for(f in setdiff(registry$feature,unsupported)) {
  recipe <- list(components=if(f=='numer')'value' else 'KC..Default.',features=f,
    fixedpars=rep(.3,registry$param_count[match(f,registry$feature)]),
    interacts=NA_character_,connectors=character(),interc=TRUE,maketimes=FALSE)
  a <- frame(d,recipe);a <- model.matrix(a$formula,a$data)
  changed <- d;changed$CF..ansbin.[6:24] <- 1-changed$CF..ansbin.[6:24]
  b <- frame(changed,recipe);b <- model.matrix(b$formula,b$data)
  stopifnot(isTRUE(all.equal(a[1:6,,drop=FALSE],b[1:6,,drop=FALSE])))
  pending <- d[1:6,];pending$CF..ansbin.[6] <- NA_real_
  b <- frame(pending,recipe);b <- model.matrix(b$formula,b$data)
  stopifnot(isTRUE(all.equal(a[1:6,,drop=FALSE],b,check.attributes=FALSE)))
}
# A supplied cache must never override reconstructed causal spacing.
recipe <- list(components='KC..Default.',features='recency',fixedpars=.3,
  interacts=NA_character_,connectors=character(),interc=TRUE,maketimes=FALSE)
cached <- d;cached$KC..Default.spacing <- 1234567
a <- frame(d,recipe);b <- frame(cached,recipe)
stopifnot(identical(model.matrix(a$formula,a$data),model.matrix(b$formula,b$data)))
cat('PASS all 40 supported LKT feature families preserve current/future-answer and pending-trial causality; spacing caches are recomputed\n')
