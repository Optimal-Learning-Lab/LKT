# Precompiled vignettes that are slow

#remove old version
list<-list.files(path ="vignettes/",pattern="(*)+(.png)", full.names=TRUE)
file.remove(list)
file.remove("vignettes/Examples.Rmd")

#can build here for last changes
library(knitr)
knit("vignettes/Examples.Rmd.orig", "vignettes/Examples.Rmd")

list<-list.files(pattern="(*)+(.png)", full.names=TRUE)
file.copy(from = list,to ="vignettes/")
file.remove(list)
devtools::build_vignettes()
