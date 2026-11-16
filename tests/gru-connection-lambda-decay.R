library(LKT)
f<-function(n)getFromNamespace(n,'LKT')
if(!requireNamespace('torch',quietly=TRUE) || !torch::torch_is_installed()) {
 if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch required')
} else {
self<-NULL
kind<-torch::nn_module(initialize=function() {
 self$gru<-torch::nn_gru(1L,1L,batch_first=TRUE)
 self$gate_fatigue<-f('lkt_gru_connection_module')(self$gru)
 self$plasticity<-f('lkt_gru_plasticity_module')('constant')
})
for(optname in c('adamw','adam'))for(zero_gradient in c(TRUE,FALSE)) {
 s<-GRUModel(gate_fatigue=TRUE,gate_fatigue_mode='connection',gate_fatigue_time_col='clock',
   plasticity='constant',plasticity_time_col='clock',optimizer=optname)$settings
 net<-kind()$to(dtype=torch::torch_float64());old<-kind()$to(dtype=torch::torch_float64())
 old$load_state_dict(net$state_dict())
 newopt<-f('lkt_gru_optimizer')(net,s)
 oldopt<-do.call(switch(optname,adamw=torch::optim_adamw,adam=torch::optim_adam,sgd=torch::optim_sgd),
   c(list(params=old$parameters,lr=s$learning_rate,weight_decay=s$weight_decay),s$optimizer_options))
 stopifnot(length(newopt$param_groups)==2L,newopt$param_groups[[1]]$weight_decay==s$weight_decay,
   newopt$param_groups[[2]]$weight_decay==0)
 before<-f('lkt_gru_weights')(net)
 loss<-function(m) {ans<-m$parameters[[1]]$pow(2)$sum()*0;for(p in m$parameters)ans<-ans+p$pow(2)$sum()*if(zero_gradient)0 else .1;ans}
 loss(net)$backward();loss(old)$backward();newopt$step();oldopt$step()
 nw<-f('lkt_gru_weights')(net);ow<-f('lkt_gru_weights')(old)
 for(n in setdiff(names(nw),'gate_fatigue.lambda'))if(!isTRUE(all.equal(nw[[n]],ow[[n]],tolerance=1e-13)))stop(sprintf('Mismatch %s zero=%s %s maxdiff=%.16g',optname,zero_gradient,n,max(abs(nw[[n]]-ow[[n]]))))
 if(zero_gradient)stopifnot(identical(nw[['gate_fatigue.lambda']],before[['gate_fatigue.lambda']]),
   all(ow[['gate_fatigue.lambda']]<before[['gate_fatigue.lambda']]))
}
cat('PASS lambda alone excludes decay; all other updates match the previous optimizer for AdamW (the matched study optimizer) and Adam.\n')
}
