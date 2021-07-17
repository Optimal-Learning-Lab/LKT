LKT [![](https://cranlogs.r-pkg.org/badges/LKT)](https://cran.r-project.org/package=LKT)
========================================================================================

Examples are here, but also please see the manual and vignette.
===============================================================

Load data
=========

    library(LKT)

    ## Warning: package 'LKT' was built under R version 4.0.5

    # data.table is the base data type
    library(data.table)
    datafile<-"C:\\Users\\ppavl\\OneDrive - The University of Memphis\\IES Data\\ds1465_tx_All_Data_64_2016_0720_222352.txt" # CHANGE THIS VALUE TO THE DataShop export file IN YOUR R WORKING DIRECTORY
    val<-read.table(colClasses = c("Anon.Student.Id"="character"),datafile,sep="\t", header=TRUE,quote="\"")
    # val<-val[1:2000,]
    # make it a datatable
    val= setDT(val)

    #make sure it is ordered in the way the code expects
    val<-val[order(val$Anon.Student.Id, val$Time),]

    #create a binanry response column to predict and extract only data with a valid value
    val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,-1))
    val<-val[val$CF..ansbin==0 | val$CF..ansbin.==1,]

    # get the times of each trial in seconds from 1970
    val$CF..Time.<-as.numeric(as.POSIXct(as.character(val$Time),format="%Y-%m-%d %H:%M:%S"))

    # create durations
    val$Duration..sec.<-(val$CF..End.Latency.+val$CF..Review.Latency.+500)/1000

    # this function needs times and durations but you don't need it if you don't want to model time effects
    val <- computeSpacingPredictors(val, "KC..Default.") #allows recency, spacing, forgetting features to run

AFM fixed effect version
========================

    modelob <- LKT(
      data = val, interc=TRUE,
      components = c("Anon.Student.Id","KC..Default.","KC..Default."),
      features = c("intercept", "intercept", "lineafm"))

    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## lineafm KC..Default.      
    ## lineafmKC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.281072 
    ## LogLike logistic: -27307.37706128

PFA fixed effect version
========================

    modelob <- LKT(
      data = val, interc=TRUE,
      components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
      features = c("intercept", "intercept", "linesuc","linefail"))

    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## linesuc KC..Default.      
    ## linefail KC..Default.      
    ## linefailKC..Default.+linesucKC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.283685 
    ## LogLike logistic: -27208.15355237

    # have to have prior predictions in data to do the next model in and adaptive system
    #   this needs to be added to the data as it is collected
    val$pred<-modelob$prediction

PFA random effect version too slow for seminar and mostly just to allow
comparison models (impractical) modelob &lt;- LKT( data = val,
interc=TRUE, components = c(“Anon.Student.Id”, “KC..Default.”,
“KC..Default.”, “KC..Default.”), features = c(“intercept@”, “intercept”,
“linesuc”,“linefail”))

\#PFA using difficulty sensitive predictors (composite model requiring
pred from prior model)

    modelob <- LKT(
      data = val, interc=TRUE,
      components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
      features = c("intercept", "intercept", "diffcorComp","linefail"))

    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## diffcorComp KC..Default.      
    ## linefail KC..Default.      
    ## linefailKC..Default.+diffcorCompKC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.287451 
    ## LogLike logistic: -27065.0885832

RPFA
====

    modelob <- LKT(
      data = val, interc=TRUE,
      components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
      features = c("intercept", "intercept", "propdec2","linefail"),
      fixedpars=c(.9))

    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.9     
    ## linefail KC..Default.      
    ## linefailKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.304175 
    ## LogLike logistic: -26429.83587366

Recency tracing with logitdec
=============================

    modelob <- LKT(
      data = val, interc=TRUE,
      components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
      features = c("intercept", "intercept", "logitdec","recency"),
      fixedpars=c(.9,.5))

    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## logitdec KC..Default. 0.9     
    ## recency KC..Default. 0.5     
    ## recencyKC..Default.+logitdecKC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.330196 
    ## LogLike logistic: -25441.49445317

Recency tracing with RPFA propdec2 feature
==========================================

    modelob <- LKT(
      data = val, interc=TRUE,
      components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
      features = c("intercept", "intercept", "propdec2","recency"),
      fixedpars=c(NA,NA))

    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.5     
    ## recency KC..Default. 0.5     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.349507 
    ## LogLike logistic: -24708.00405216 
    ## step par values =0.5,0.5
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.501     
    ## recency KC..Default. 0.5     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.349508 
    ## LogLike logistic: -24707.96286984 
    ## step par values =0.501,0.5
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.499     
    ## recency KC..Default. 0.5     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.349505 
    ## LogLike logistic: -24708.05746159 
    ## step par values =0.499,0.5
    ## 
    ## Solving iterates a bunch of times
    ## 
    ##
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.5325893128318     
    ## recency KC..Default. 0.326971232690059     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.26268674 
    ## step par values =0.5325893,0.3269712
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.5325893128318     
    ## recency KC..Default. 0.324971232690059     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.2739021 
    ## step par values =0.5325893,0.3249712

PPE
===

    modelob <- LKT(
      data = val, interc=TRUE,
      components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
      features = c("intercept", "intercept", "ppe","logitdec"),
      fixedpars=c(0.3491901,0.2045801,1e-05,0.9734477,0.4443027))

    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## ppe KC..Default. 0.3491901 0.2045801 1e-05 0.9734477  
    ## logitdec KC..Default. 0.4443027     
    ## logitdecKC..Default.+ppeKC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.350696 
    ## LogLike logistic: -24662.82575816

base4
=====

    modelob <- LKT(
      data = val, interc=TRUE,
      components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
      features = c("intercept", "intercept", "base4","logitdec"),
      fixedpars=c(0.1890747,0.6309054,0.05471752,.5,0.2160748))

    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## base4 KC..Default. 0.1890747 0.6309054 0.05471752 0.5  
    ## logitdec KC..Default. 0.2160748     
    ## logitdecKC..Default.+base4KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.317364 
    ## LogLike logistic: -25928.89340413

\#Using other features \#See LKT paper \#See computefeatures function in
the main R code for package
\#<a href="https://github.com/Optimal-Learning-Lab/LKT/blob/master/R/LKTfunctions.R" class="uri">https://github.com/Optimal-Learning-Lab/LKT/blob/master/R/LKTfunctions.R</a>

Covariates
==========

    modelob <- LKT(
      data = val, interc=TRUE,
      components = c("Anon.Student.Id","KC..Default.","KC..Default."),
      features = c("logitdec", "logitdec", "lineafm"),fixedpars=c(.9,.8),
      covariates = c(NA,NA,"Level..Unitname."))

    ## logitdec Anon.Student.Id 0.9     
    ## logitdec KC..Default. 0.8     
    ## lineafm KC..Default.      
    ## lineafmKC..Default.:Level..Unitname.+logitdecKC..Default.+logitdecAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.187931 
    ## LogLike logistic: -30845.19646772

Crossvalidation
===============

    # make folds
    unq = sample(unique(val$Anon.Student.Id))
    sfold = rep(1:5,length.out=length(unq))
    val$fold = rep(0,length(val[,1]))
    for(i in 1:5){val$fold[which(val$Anon.Student.Id %in% unq[which(sfold==i)])]=i}

    #simple AFM minus student intercept
    modelob <- LKT(
      data = val, interc=TRUE,
      components = c("KC..Default.","KC..Default."),
      features = c("intercept", "lineafm"),
      cv = TRUE)

    ## intercept KC..Default.      
    ## lineafm KC..Default.      
    ## lineafmKC..Default.+interceptKC..Default.+1 
    ## McFadden's R2 logistic: 0.182037 
    ## LogLike logistic: -31069.09249305

    mean(modelob$cv_res$mcfad)

    ## [1] 0.1739434

    #complex AFM minus student intercept
    modelob <- LKT(
      data = val, interc=TRUE,
      components = c("KC..Default.","KC..Default."),
      features = c("intercept$", "lineafm$"),
      cv = TRUE)

    ## intercept$ KC..Default.      
    ## lineafm$ KC..Default.      
    ## lineafmKC..Default.:e$data$KC..Default.+interceptKC..Default.:e$data$KC..Default.+1 
    ## McFadden's R2 logistic: 0.193518 
    ## LogLike logistic: -30633.00356118

    mean(modelob$cv_res$mcfad)

    ## [1] 0.1738372
