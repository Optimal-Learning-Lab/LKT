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

    # get the times of each trial in seconds from 1901?
    val$CF..Time.<-as.numeric(as.POSIXct(as.character(val$Time),format="%Y-%m-%d %H:%M:%S"))

    # create durations
    val$Duration..sec.<-(val$CF..End.Latency.+val$CF..Review.Latency.+500)/1000

    # this function needs times and durations but you don't need it if you don't want to model time effects
    val <- computeSpacingPredictors(val, "KC..Default.") #allows recency, spacing, forgeting features to run

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

    # have to have prior predicitons in data to do the next model in and adaptive system
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
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.5     
    ## recency KC..Default. 0.501     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.349478 
    ## LogLike logistic: -24709.09318892 
    ## step par values =0.5,0.501
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.5     
    ## recency KC..Default. 0.499     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.349535 
    ## LogLike logistic: -24706.9194436 
    ## step par values =0.5,0.499
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.99999     
    ## recency KC..Default. 1e-05     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.323546 
    ## LogLike logistic: -25694.07293643 
    ## step par values =0.99999,1e-05
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.99999     
    ## recency KC..Default. 1e-05     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.323546 
    ## LogLike logistic: -25694.07293643 
    ## step par values =0.99999,1e-05
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.99899     
    ## recency KC..Default. 1e-05     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.323554 
    ## LogLike logistic: -25693.76443106 
    ## step par values =0.99899,1e-05
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.99999     
    ## recency KC..Default. 0.00101     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.3238 
    ## LogLike logistic: -25684.44115204 
    ## step par values =0.99999,0.00101
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.99999     
    ## recency KC..Default. 1e-05     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.323546 
    ## LogLike logistic: -25694.07293643 
    ## step par values =0.99999,1e-05
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.651655296438547     
    ## recency KC..Default. 0.348344703561453     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.351976 
    ## LogLike logistic: -24614.1903755 
    ## step par values =0.6516553,0.3483447
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.652655296438547     
    ## recency KC..Default. 0.348344703561453     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.351973 
    ## LogLike logistic: -24614.33437565 
    ## step par values =0.6526553,0.3483447
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.650655296438547     
    ## recency KC..Default. 0.348344703561453     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.35198 
    ## LogLike logistic: -24614.06301874 
    ## step par values =0.6506553,0.3483447
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.651655296438547     
    ## recency KC..Default. 0.349344703561453     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.351971 
    ## LogLike logistic: -24614.39159125 
    ## step par values =0.6516553,0.3493447
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.651655296438547     
    ## recency KC..Default. 0.347344703561453     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.351981 
    ## LogLike logistic: -24614.01228249 
    ## step par values =0.6516553,0.3473447
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.612230306991811     
    ## recency KC..Default. 0.331181103011575     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352154 
    ## LogLike logistic: -24607.43061376 
    ## step par values =0.6122303,0.3311811
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.613230306991811     
    ## recency KC..Default. 0.331181103011575     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352152 
    ## LogLike logistic: -24607.53046725 
    ## step par values =0.6132303,0.3311811
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.611230306991811     
    ## recency KC..Default. 0.331181103011575     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352157 
    ## LogLike logistic: -24607.33153834 
    ## step par values =0.6112303,0.3311811
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.612230306991811     
    ## recency KC..Default. 0.332181103011575     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352153 
    ## LogLike logistic: -24607.47995625 
    ## step par values =0.6122303,0.3321811
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.612230306991811     
    ## recency KC..Default. 0.330181103011575     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352156 
    ## LogLike logistic: -24607.37292413 
    ## step par values =0.6122303,0.3301811
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.508996035601948     
    ## recency KC..Default. 0.323090894871563     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352254 
    ## LogLike logistic: -24603.6616443 
    ## step par values =0.508996,0.3230909
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.509996035601948     
    ## recency KC..Default. 0.323090894871563     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352254 
    ## LogLike logistic: -24603.63051538 
    ## step par values =0.509996,0.3230909
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.507996035601948     
    ## recency KC..Default. 0.323090894871563     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352253 
    ## LogLike logistic: -24603.69395486 
    ## step par values =0.507996,0.3230909
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.508996035601948     
    ## recency KC..Default. 0.324090894871563     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352254 
    ## LogLike logistic: -24603.64518846 
    ## step par values =0.508996,0.3240909
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.508996035601948     
    ## recency KC..Default. 0.322090894871563     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352253 
    ## LogLike logistic: -24603.69989366 
    ## step par values =0.508996,0.3220909
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.533834547270276     
    ## recency KC..Default. 0.326026919705472     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.2565602 
    ## step par values =0.5338345,0.3260269
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.534834547270276     
    ## recency KC..Default. 0.326026919705472     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.27470323 
    ## step par values =0.5348345,0.3260269
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.532834547270276     
    ## recency KC..Default. 0.326026919705472     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.25602823 
    ## step par values =0.5328345,0.3260269
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.533834547270276     
    ## recency KC..Default. 0.327026919705472     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.26381339 
    ## step par values =0.5338345,0.3270269
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.533834547270276     
    ## recency KC..Default. 0.325026919705472     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.25749089 
    ## step par values =0.5338345,0.3250269
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.528108294253262     
    ## recency KC..Default. 0.325826782045827     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.27093837 
    ## step par values =0.5281083,0.3258268
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.529108294253262     
    ## recency KC..Default. 0.325826782045827     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.28105697 
    ## step par values =0.5291083,0.3258268
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.527108294253262     
    ## recency KC..Default. 0.325826782045827     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.27792567 
    ## step par values =0.5271083,0.3258268
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.528108294253262     
    ## recency KC..Default. 0.326826782045827     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.27663085 
    ## step par values =0.5281083,0.3268268
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.528108294253262     
    ## recency KC..Default. 0.324826782045827     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.27351951 
    ## step par values =0.5281083,0.3248268
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.532664364722354     
    ## recency KC..Default. 0.325986020779804     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.25594487 
    ## step par values =0.5326644,0.325986
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.533664364722354     
    ## recency KC..Default. 0.325986020779804     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.27204747 
    ## step par values =0.5336644,0.325986
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.531664364722354     
    ## recency KC..Default. 0.325986020779804     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.25695979 
    ## step par values =0.5316644,0.325986
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.532664364722354     
    ## recency KC..Default. 0.326986020779804     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.2792447 
    ## step par values =0.5326644,0.326986
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.532664364722354     
    ## recency KC..Default. 0.324986020779804     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.25716721 
    ## step par values =0.5326644,0.324986
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.494240485869806     
    ## recency KC..Default. 0.318415048687929     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352232 
    ## LogLike logistic: -24604.4755526 
    ## step par values =0.4942405,0.318415
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.495240485869806     
    ## recency KC..Default. 0.318415048687929     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352234 
    ## LogLike logistic: -24604.42489733 
    ## step par values =0.4952405,0.318415
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.493240485869806     
    ## recency KC..Default. 0.318415048687929     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352231 
    ## LogLike logistic: -24604.52764449 
    ## step par values =0.4932405,0.318415
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.494240485869806     
    ## recency KC..Default. 0.319415048687929     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352234 
    ## LogLike logistic: -24604.42043457 
    ## step par values =0.4942405,0.319415
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.494240485869806     
    ## recency KC..Default. 0.317415048687929     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.35223 
    ## LogLike logistic: -24604.53921635 
    ## step par values =0.4942405,0.317415
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.52893832330565     
    ## recency KC..Default. 0.325251848269858     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.26655134 
    ## step par values =0.5289383,0.3252518
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.52993832330565     
    ## recency KC..Default. 0.325251848269858     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.26194331 
    ## step par values =0.5299383,0.3252518
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.52793832330565     
    ## recency KC..Default. 0.325251848269858     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.27248545 
    ## step par values =0.5279383,0.3252518
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.52893832330565     
    ## recency KC..Default. 0.326251848269858     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.26750135 
    ## step par values =0.5289383,0.3262518
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.52893832330565     
    ## recency KC..Default. 0.324251848269858     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.27394032 
    ## step par values =0.5289383,0.3242518
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.531673945805239     
    ## recency KC..Default. 0.325790870414857     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.27275532 
    ## step par values =0.5316739,0.3257909
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.532673945805239     
    ## recency KC..Default. 0.325790870414857     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.25554548 
    ## step par values =0.5326739,0.3257909
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.530673945805239     
    ## recency KC..Default. 0.325790870414857     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.25886472 
    ## step par values =0.5306739,0.3257909
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.531673945805239     
    ## recency KC..Default. 0.326790870414857     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.26182246 
    ## step par values =0.5316739,0.3267909
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.531673945805239     
    ## recency KC..Default. 0.324790870414857     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.25947803 
    ## step par values =0.5316739,0.3247909
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.5325893128318     
    ## recency KC..Default. 0.325971232690059     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.25593653 
    ## step par values =0.5325893,0.3259712
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.5335893128318     
    ## recency KC..Default. 0.325971232690059     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.27262738 
    ## step par values =0.5335893,0.3259712
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.5315893128318     
    ## recency KC..Default. 0.325971232690059     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.25698464 
    ## step par values =0.5315893,0.3259712
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

    ## [1] 0.172508

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

    ## [1] 0.1725072
