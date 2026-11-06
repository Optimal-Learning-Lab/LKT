# Create a hash-verified package-build input from the maintained source tree.
# R's Windows builder copies the whole workspace before honoring .Rbuildignore;
# prune ignored directories first so research data and live locks never enter it.
args <- commandArgs(TRUE);stopifnot(length(args)==2L)
root <- normalizePath(args[1],winslash='/',mustWork=TRUE)
out <- normalizePath(args[2],winslash='/',mustWork=FALSE)
stopifnot(!dir.exists(out),file.exists(file.path(root,'DESCRIPTION')))
if(tolower(out)==tolower(root) || startsWith(tolower(out),paste0(tolower(root),'/')))
  stop('Package verification output must be outside the maintained source tree.')
sample_files <- c('largerawsample.rda','samplelkt.rda')
if(!identical(sort(list.files(file.path(root,'data'),all.files=TRUE,no..=TRUE)),sample_files))
  stop('data/ must contain only the two intentional package samples; put bulk inputs in LKT-data.')
dir.create(out,recursive=TRUE);target <- file.path(out,'LKT');dir.create(target)
vc <- get('.vc_dir_names',asNamespace('tools'))
files <- character();excluded <- character()
walk <- function(relative='') {
  directory <- file.path(root,relative)
  entries <- list.files(directory,all.files=TRUE,no..=TRUE)
  for(name in entries) {
    path <- if(nzchar(relative))paste(relative,name,sep='/') else name
    source <- file.path(root,path);isdir <- dir.exists(source)
    ignore <- tools:::inRbuildignore(path,root) ||
      (isdir && (name %in% c(vc,'check','chm') || grepl('([Oo]ld|[.]Rcheck)$',name))) ||
      (!isdir && grepl('^LKT_[0-9.-]+[.](tar[.]gz|zip)$',path))
    if(ignore){excluded <<- c(excluded,path);next}
    if(isdir)walk(path) else files <<- c(files,path)
  }
}
walk()
hash <- function(path)digest::digest(file=path,algo='sha256')
for(path in files) {
  destination <- file.path(target,path);dir.create(dirname(destination),recursive=TRUE,showWarnings=FALSE)
  stopifnot(file.copy(file.path(root,path),destination),hash(file.path(root,path))==hash(destination))
}
write.csv(data.frame(path=files,sha256=vapply(file.path(root,files),hash,character(1))),file.path(out,'source-checksums.csv'),row.names=FALSE)
writeLines(excluded,file.path(out,'excluded-paths.txt'))
writeLines(c('Generated package verification input; not a maintained source tree.',
  paste('Authoritative source:',root),'Do not edit this input. Regenerate it from the authoritative source.'),file.path(out,'README.txt'))
cat('PASS',length(files),'source files copied and hash checked; ignored trees pruned before R CMD build.\n')
