# Precompiled vignettes that are slow
# Must manually move image files from LKT/ to LKT/vignettes/ after knit

#remove old version
list<-list.files(path ="vignettes/",pattern="(*)+(.png)", full.names=TRUE)
file.remove(list)
file.remove("vignettes/Examples.Rmd")

library(knitr)
knit("vignettes/Examples.Rmd.orig", "vignettes/Examples.Rmd")

list<-list.files(pattern="(*)+(.png)", full.names=TRUE)
file.copy(from = list,to ="vignettes/")
file.remove(list)
build_vignettes()
