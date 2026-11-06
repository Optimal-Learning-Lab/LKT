source('scripts/ednet-author-protocol.R')
fail <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=identity),'error'))
d <- data.frame(Anon.Student.Id=rep(letters[1:6],each=2),ktst_validation_fold=rep(-1:4,each=2))
for(k in 0:4) {
  roles <- ednet_author_roles(d,k)
  stopifnot(all(roles[d$ktst_validation_fold==-1]==3L),all(roles[d$ktst_validation_fold==k]==2L))
}
bad <- d;bad$ktst_validation_fold[2] <- 0L;fail(ednet_author_roles(bad,0L));fail(ednet_author_roles(d,5L))
# Match weighted whole-student bootstrap against literal replicated rows,
# including tied predictions and unequal student history lengths.
y <- c(0,1,1,0,1,0,0,1);who <- c(1,1,2,2,2,3,3,3)
p <- cbind(c(.2,.8,.7,.3,.7,.6,.4,.6),c(.5,.5,.6,.4,.6,.3,.3,.7))
set.seed(41);before <- .Random.seed
actual <- ednet_author_bootstrap(y,p,who,draws=30L,seed=9L);stopifnot(identical(before,.Random.seed))
set.seed(9)
expected <- vapply(1:30,function(i) {
  counts <- tabulate(sample.int(3,3,replace=TRUE),nbins=3)
  rows <- rep(seq_along(y),counts[who])
  mean(vapply(1:2,function(k)as.numeric(pROC::auc(y[rows],p[rows,k],quiet=TRUE,direction='<')),numeric(1)))
},numeric(1))
stopifnot(max(abs(expected-actual))<1e-12)
ref <- '../LKT-outputs/ednet-ktst-base-20261105/reference/kt_set_transformers-e6331a6f45e597f286d1272fbb96e7bb67bcaa52'
tasks <- ednet_author_tasks(ref)
stopifnot(identical(tasks$seed,c(90L,56L,96L,50L,95L)))
cat('PASS published KTST fold/seed selection, whole-student roles and independent weighted bootstrap\n')
