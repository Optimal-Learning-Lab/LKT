# Stable bridge to the single release definition; this contains no release pins.
# `root` is the LKT source root supplied by the calling research script.
lkt_release_file <- Sys.getenv('LKT_RELEASE_FILE',
  unset=file.path(dirname(root),'PolicyFactory2/workflows/simulation/shared/lkt-release.env'))
source(file.path(dirname(lkt_release_file),'lkt_release.R'),local=TRUE)
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
lkt_release <- lkt_use_current(lkt_release_file)
# Explicit overrides must agree; never use them to select a second installation.
for(key in c('LKT_GRU_LIBRARY','LKT_STUDY_LIBRARY')) {
  requested <- Sys.getenv(key)
  if(nzchar(requested) && normalizePath(requested,winslash='/',mustWork=TRUE) !=
     lkt_release[['lkt_library']])stop(key,' disagrees with the central release.')
}
