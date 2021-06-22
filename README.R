# LKT [![](https://cranlogs.r-pkg.org/badges/LKT)](https://cran.r-project.org/package=LKT)

# Please see the manual and vignette.

# Will be creating examples here.

library(LKT)
library(data.table)
datafile<-"C:\\Users\\ppavl\\OneDrive - The University of Memphis\\IES Data\\ds1465_tx_All_Data_64_2016_0720_222352.txt" # CHANGE THIS VALUE TO THE DataShop export file IN YOUR R WORKING DIRECTORY
val<-read.table(colClasses = c("Anon.Student.Id"="character"),datafile,sep="\t", header=TRUE,quote="\"")
val= setDT(val)
val<-val[order(val$Anon.Student.Id, val$Time),]
val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,-1))
val<-val[val$CF..ansbin==0 | val$CF..ansbin.==1,]
val$CF..Time.<-as.numeric(as.POSIXct(as.character(val$Time),format="%Y-%m-%d %H:%M:%S"))
val$Duration..sec.<-(val$CF..End.Latency.+val$CF..Review.Latency.+500)/1000
val <- computeSpacingPredictors(val, "KC..Default.")


#AFM fixed effect version
modelob <- LKT(
  data = val, interc=TRUE,
  components = c("Anon.Student.Id","KC..Default.","KC..Default."),
  features = c("intercept", "intercept", "lineafm"))

#PFA fixed effect version
modelob <- LKT(
  data = val, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "linesuc","linefail"))
val$pred<-modelob$prediction

#PFA fixed effect version
modelob <- LKT(
  data = val, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "diffcorComp","linefail"))

#RPFA
modelob <- LKT(
  data = val, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "propdec2","linefail"),
  fixedpars=c(.9))

#Recency tracing with logitdec
modelob <- LKT(
  data = val, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "logitdec","recency"),
  fixedpars=c(.9,.5))

#Recency tracing with propdec2
modelob <- LKT(
  data = val, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "propdec2","recency"),
  fixedpars=c(NA,NA))


#PPE
modelob <- LKT(
  data = val, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "ppe","logitdec"),
  fixedpars=c(0.3491901,0.2045801,1e-05,0.9734477,0.4443027))


#base4
modelob <- LKT(
  data = val, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "base4","logitdec"),
  seedpars=c(0.1890747,0.6309054,0.05471752,0.99999,0.2160748))

#full adaptive PPE
modelob <- LKT(
  data = val, interc=TRUE,elastic="cv.glmnet",
  components = c("Anon.Student.Id",  "KC..Default.", "KC..Default."),
  features = c("logitdec", "ppe","logitdec"),
  fixedpars=c(.96, 0.3491901,0.2045801,1e-05,0.9734477,0.4443027))


#Using other features
#See LKT paper
#See computefeatures function in the main R code for package
#https://github.com/Optimal-Learning-Lab/LKT/blob/master/R/LKTfunctions.R

#Covariates

#glmnet

#Random effects (intercepts only, solved with lmer)

#Crossvalidation
#make folds
unq = sample(unique(val$Anon.Student.Id))
sfold = rep(1:5,length.out=length(unq))
val$fold = rep(0,length(val[,1]))
for(i in 1:5){val$fold[which(val$Anon.Student.Id %in% unq[which(sfold==i)])]=i}

#AFM minus student intercept
modelob <- LKT(
  data = val, interc=TRUE,
  components = c("KC..Default.","KC..Default."),
  features = c("intercept", "lineafm"),
  cv = TRUE)
modelob$cv_res
