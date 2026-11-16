library(LKT)
if(!requireNamespace('torch',quietly=TRUE)||!torch::torch_is_installed()) {
  if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required')
} else {
  convert <- getFromNamespace('lkt_gru_tensor','LKT')
  for(dtype in c('float32','float64'))for(long in c(FALSE,TRUE)) {
    s <- list(dtype=dtype,device='cpu')
    for(shape in list(3L,c(1L,1L),c(1L,3L),c(3L,1L),c(1L,1L,3L),c(2L,3L),c(2L,1L,3L))) {
      x <- array(seq_len(prod(shape)),shape)
      if(!long)storage.mode(x) <- 'double'
      expected <- serialize(x,NULL)
      value <- convert(x,s,long)
      stopifnot(identical(as.numeric(torch::as_array(value)),as.numeric(x)))
      value$fill_(0)
      stopifnot(identical(serialize(x,NULL),expected))
      # The tensor must also outlive a temporary R input, independently of the
      # collector's timing. GC here exercises ownership, not prediction logic.
      retained <- local(convert(array(as.numeric(seq_len(prod(shape))),shape),s,long))
      invisible(gc())
      stopifnot(identical(as.numeric(torch::as_array(retained)),as.numeric(seq_len(prod(shape)))))
    }
  }
  cat('PASS owned tensor storage, multidimensional layout, mutation isolation and temporary-input lifetime\n')
}
