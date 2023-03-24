# Precompiled vignettes that are slow
# Must manually move image files from LKT/ to LKT/vignettes/ after knit

library(knitr)
knit("vignettes/Examples.Rmd.orig", "vignettes/Examples.Rmd")

list<-list.files(pattern="(*)+(.png)", full.names=TRUE)
file.copy(from = list,to ="vignettes/")
file.remove(list)

