# ROOT: update the candidate source manifest, retaining prior entries and adding
# maintained runtime, manual and native-test sources from the single source tree.
a <- commandArgs(TRUE);stopifnot(length(a)==1L)
root <- normalizePath(a[1],winslash='/');.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
path <- file.path(root,'inst/research-source-manifest.json')
old <- jsonlite::fromJSON(path)
source(file.path(dirname(root),'PolicyFactory2/workflows/simulation/shared/lkt_release.R'))
current <- lkt_read_release(file.path(dirname(root),'PolicyFactory2/workflows/simulation/shared/lkt-release.env'))
files <- sort(unique(c(old$files$path,
  paste0('R/',list.files(file.path(root,'R'),pattern='[.]R$',recursive=TRUE)),
  paste0('man/',list.files(file.path(root,'man'),pattern='[.]Rd$',recursive=TRUE)),
  paste0('tests/',list.files(file.path(root,'tests'),pattern='[.]R$',recursive=TRUE)))))
stopifnot(all(file.exists(file.path(root,files))))
manifest <- list(version=read.dcf(file.path(root,'DESCRIPTION'))[1,'Version'],base=current[['lkt_version']],
  files=data.frame(path=files,sha256=vapply(file.path(root,files),function(p)digest::digest(file=p,algo='sha256'),character(1)),row.names=NULL))
jsonlite::write_json(manifest,path,auto_unbox=TRUE,pretty=TRUE)
