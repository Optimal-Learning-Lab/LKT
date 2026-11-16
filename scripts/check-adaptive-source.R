# Non-installing development checks against the maintained source, with dependencies
# from the central release. Never used by a simulator or research launcher.
a <- commandArgs(TRUE);root <- normalizePath(a[1],winslash='/')
source(file.path(root,'scripts/load-current-lkt.R'))
cat('Torch gradient API\n');print(args(torch::autograd_grad))
print(grep('enable_grad|with_grad',getNamespaceExports('torch'),value=TRUE))
env <- new.env(parent=asNamespace('LKT'))
files <- strsplit(read.dcf(file.path(root,'DESCRIPTION'))[1,'Collate'],'[[:space:]]+')[[1]]
files <- gsub("'",'',files);files <- files[nzchar(files)]
for(f in files)sys.source(file.path(root,'R',f),envir=env)
# Exported functions resolve through the loaded source environment for this check.
attach(env,name='LKT development verification')
if(length(a)>1L)sys.source(a[2],envir=env)
