# ROOT. Independent analytical accounting for synthetic known-catalog cardinalities.
a<-commandArgs(TRUE);stopifnot(length(a)==1L)
root<-normalizePath(a[1],winslash='/',mustWork=TRUE)
library(LKT);stopifnot(torch::torch_is_installed())
source(file.path(root,'tests/fixtures/gru-exercise-attributes-fixture.R'))
f<-function(n)getFromNamespace(n,'LKT')
directory<-tempfile('synthetic-parameter-counts-')
fixture<-test_gru_exercise_fixture(directory,144L,36L)
s<-GRUModel(categorical_cols=c('item','skill'),zero_init_embedding_cols='item',
 plasticity='constant',plasticity_time_col='clock',dtype='float64')$settings
attrs<-f('lkt_gru_exercise_load')(utils::modifyList(s,list(exercise_attributes=fixture$controls),keep.null=TRUE))
p<-list(vocabularies=list(item=attrs$item_ids,skill=sort(unique(attrs$kc_ids),method='radix')),
 embedding_dims=c(16L,16L),numeric_names=c('time:a','time:b','time:available_b','lkt:a','lkt:b'),
 center=rep(0,5L),scale=rep(1,5L),engineered=NULL)
# Compute dimensions independently of named tensor shapes and module constructors.
h<-32L;e<-16L;numeric_input<-5L;numeric_width<-32L;input_width<-numeric_width+2L*e
expected<-c(embeddings=(144L+1L)*e+(36L+1L)*e,
 numeric_encoder=(numeric_input+1L)*32L+(32L+1L)*numeric_width,
 gru=3L*h*(input_width+1L)+3L*h*h+2L*3L*h+2L*(2L*3L*h*h+2L*3L*h),
 head=(h+input_width+1L)*128L+129L,
 retrieval=(h+input_width+1L)*32L+(h+1L)*h,plasticity=1L)
stopifnot(sum(expected)==43042L)
count<-function(net,prefix=NULL) {
 named<-net$named_parameters()
 if(!is.null(prefix))named<-named[startsWith(names(named),paste0(prefix,'.'))]
 sum(vapply(named,function(x)as.numeric(x$numel()),numeric(1)))
}
base<-f('lkt_gru_network')(p,s)
for(block in names(expected))stopifnot(count(base,block)==expected[[block]])
stopifnot(count(base)==sum(expected))
keen<-s;keen$latent_denoising<-TRUE
net<-f('lkt_gru_network')(p,keen)
stopifnot(count(net,'latent_denoiser')==2L*(h*h+h),count(net)==sum(expected)+2112L)
bayes<-s;bayes$plasticity<-'bayesian_prototypes'
net<-f('lkt_gru_network')(p,bayes)
stopifnot(count(net,'plasticity')==1L+20L,count(net)==sum(expected)+20L)
eakt<-s;eakt$exercise_attributes<-fixture$controls;ep<-p;ep$exercise_attributes<-attrs
net<-f('lkt_gru_network')(ep,eakt);B<-length(attrs$ability_ids)
stopifnot(count(net,'exercise_attributes')==2L*5L*8L+B*8L+24L*16L+16L,
 count(net,'embeddings')==16L+(36L+1L)*16L,
 count(net)==41218L+8L*B)
unlink(directory,recursive=TRUE)
cat('PASS independent analytical parameter counts: baseline43042, Keen45154, Bayes43062, EAKT41218+8B; all independent conditions.\n')
