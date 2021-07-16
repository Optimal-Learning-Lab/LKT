LKT [![](https://cranlogs.r-pkg.org/badges/LKT)](https://cran.r-project.org/package=LKT)
========================================================================================

Please see the manual and vignette.
===================================

Will be creating examples here.
===============================

    library(LKT)

    ## Warning: package 'LKT' was built under R version 4.0.5

    # data.table is the base data type
    library(data.table)
    datafile<-"C:\\Users\\ppavl\\OneDrive - The University of Memphis\\IES Data\\ds1465_tx_All_Data_64_2016_0720_222352.txt" # CHANGE THIS VALUE TO THE DataShop export file IN YOUR R WORKING DIRECTORY
    val<-read.table(colClasses = c("Anon.Student.Id"="character"),datafile,sep="\t", header=TRUE,quote="\"")
    val<-val[1:2000,]
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
    ## McFadden's R2 logistic: 0.38021 
    ## LogLike logistic: -794.26409056

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
    ## McFadden's R2 logistic: 0.380357 
    ## LogLike logistic: -794.07578027

    # have to have prior predicitons in data to do the next model in and adaptive system
    #   this needs to be added to the data as it is collected
    val$pred<-modelob$prediction

### PFA random effect version too slow for seminar and mostly just to allow comparison models (impractical)

### modelob &lt;- LKT(

### data = val, interc=TRUE,

### components = c(“Anon.Student.Id”, “KC..Default.”, “KC..Default.”, “KC..Default.”),

### features = c(“intercept@”, “intercept”, “linesuc”,“linefail”))

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
    ## McFadden's R2 logistic: 0.383228 
    ## LogLike logistic: -790.39658068

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
    ## McFadden's R2 logistic: 0.394814 
    ## LogLike logistic: -775.54966768

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
    ## McFadden's R2 logistic: 0.420591 
    ## LogLike logistic: -742.51607213

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
    ## McFadden's R2 logistic: 0.434616 
    ## LogLike logistic: -724.5431518 
    ## step par values =0.5,0.5
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.501     
    ## recency KC..Default. 0.5     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.434619 
    ## LogLike logistic: -724.53946098 
    ## step par values =0.501,0.5
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.499     
    ## recency KC..Default. 0.5     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.434613 
    ## LogLike logistic: -724.54734604 
    ## step par values =0.499,0.5
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.5     
    ## recency KC..Default. 0.501     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.434556 
    ## LogLike logistic: -724.62028819 
    ## step par values =0.5,0.501
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.5     
    ## recency KC..Default. 0.499     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.434675 
    ## LogLike logistic: -724.46709223 
    ## step par values =0.5,0.499
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.99999     
    ## recency KC..Default. 1e-05     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.418784 
    ## LogLike logistic: -744.83157344 
    ## step par values =0.99999,1e-05
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.99999     
    ## recency KC..Default. 1e-05     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.418784 
    ## LogLike logistic: -744.83157344 
    ## step par values =0.99999,1e-05
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.99899     
    ## recency KC..Default. 1e-05     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.418789 
    ## LogLike logistic: -744.82491608 
    ## step par values =0.99899,1e-05
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.99999     
    ## recency KC..Default. 0.00101     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.419105 
    ## LogLike logistic: -744.4206984 
    ## step par values =0.99999,0.00101
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.99999     
    ## recency KC..Default. 1e-05     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.418784 
    ## LogLike logistic: -744.83157344 
    ## step par values =0.99999,1e-05
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.719108778367792     
    ## recency KC..Default. 0.280891221632208     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444198 
    ## LogLike logistic: -712.26418432 
    ## step par values =0.7191088,0.2808912
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.720108778367792     
    ## recency KC..Default. 0.280891221632208     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444197 
    ## LogLike logistic: -712.26543558 
    ## step par values =0.7201088,0.2808912
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.718108778367792     
    ## recency KC..Default. 0.280891221632208     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444199 
    ## LogLike logistic: -712.26295001 
    ## step par values =0.7181088,0.2808912
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.719108778367792     
    ## recency KC..Default. 0.281891221632208     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444177 
    ## LogLike logistic: -712.2903128 
    ## step par values =0.7191088,0.2818912
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.719108778367792     
    ## recency KC..Default. 0.279891221632208     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444218 
    ## LogLike logistic: -712.23841715 
    ## step par values =0.7191088,0.2798912
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.785771004792704     
    ## recency KC..Default. 0.180843616778972     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444319 
    ## LogLike logistic: -712.10845258 
    ## step par values =0.785771,0.1808436
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.786771004792704     
    ## recency KC..Default. 0.180843616778972     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444317 
    ## LogLike logistic: -712.11087155 
    ## step par values =0.786771,0.1808436
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.784771004792704     
    ## recency KC..Default. 0.180843616778972     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444321 
    ## LogLike logistic: -712.1060804 
    ## step par values =0.784771,0.1808436
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.785771004792704     
    ## recency KC..Default. 0.181843616778972     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.44434 
    ## LogLike logistic: -712.08140016 
    ## step par values =0.785771,0.1818436
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.785771004792704     
    ## recency KC..Default. 0.179843616778972     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444297 
    ## LogLike logistic: -712.13644769 
    ## step par values =0.785771,0.1798436
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.749981708178378     
    ## recency KC..Default. 0.234556695349466     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444762 
    ## LogLike logistic: -711.54138394 
    ## step par values =0.7499817,0.2345567
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.750981708178378     
    ## recency KC..Default. 0.234556695349466     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444762 
    ## LogLike logistic: -711.54089494 
    ## step par values =0.7509817,0.2345567
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.748981708178378     
    ## recency KC..Default. 0.234556695349466     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444763 
    ## LogLike logistic: -711.53955646 
    ## step par values =0.7489817,0.2345567
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.749981708178378     
    ## recency KC..Default. 0.235556695349466     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444758 
    ## LogLike logistic: -711.54594858 
    ## step par values =0.7499817,0.2355567
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.749981708178378     
    ## recency KC..Default. 0.233556695349466     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444767 
    ## LogLike logistic: -711.53493194 
    ## step par values =0.7499817,0.2335567
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.755309337425204     
    ## recency KC..Default. 0.222931193247045     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444784 
    ## LogLike logistic: -711.51314085 
    ## step par values =0.7553093,0.2229312
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.756309337425204     
    ## recency KC..Default. 0.222931193247045     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444783 
    ## LogLike logistic: -711.51429064 
    ## step par values =0.7563093,0.2229312
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.754309337425204     
    ## recency KC..Default. 0.222931193247045     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444787 
    ## LogLike logistic: -711.50870616 
    ## step par values =0.7543093,0.2229312
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.755309337425204     
    ## recency KC..Default. 0.223931193247045     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444783 
    ## LogLike logistic: -711.51345562 
    ## step par values =0.7553093,0.2239312
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.755309337425204     
    ## recency KC..Default. 0.221931193247045     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444784 
    ## LogLike logistic: -711.5130799 
    ## step par values =0.7553093,0.2219312
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.744591335382703     
    ## recency KC..Default. 0.221039945094289     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.4448 
    ## LogLike logistic: -711.49170079 
    ## step par values =0.7445913,0.2210399
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.745591335382703     
    ## recency KC..Default. 0.221039945094289     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444797 
    ## LogLike logistic: -711.49562037 
    ## step par values =0.7455913,0.2210399
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.743591335382703     
    ## recency KC..Default. 0.221039945094289     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.4448 
    ## LogLike logistic: -711.49214656 
    ## step par values =0.7435913,0.2210399
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.744591335382703     
    ## recency KC..Default. 0.222039945094289     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444799 
    ## LogLike logistic: -711.49352446 
    ## step par values =0.7445913,0.2220399
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.744591335382703     
    ## recency KC..Default. 0.220039945094289     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.4448 
    ## LogLike logistic: -711.49281476 
    ## step par values =0.7445913,0.2200399
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.72549250395032     
    ## recency KC..Default. 0.215839711313006     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444813 
    ## LogLike logistic: -711.47590591 
    ## step par values =0.7254925,0.2158397
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.72649250395032     
    ## recency KC..Default. 0.215839711313006     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444812 
    ## LogLike logistic: -711.47732097 
    ## step par values =0.7264925,0.2158397
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.72449250395032     
    ## recency KC..Default. 0.215839711313006     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444814 
    ## LogLike logistic: -711.47438725 
    ## step par values =0.7244925,0.2158397
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.72549250395032     
    ## recency KC..Default. 0.216839711313006     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444816 
    ## LogLike logistic: -711.47207113 
    ## step par values =0.7254925,0.2168397
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.72549250395032     
    ## recency KC..Default. 0.214839711313006     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.44481 
    ## LogLike logistic: -711.47972499 
    ## step par values =0.7254925,0.2148397
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.684904371637684     
    ## recency KC..Default. 0.216520345255937     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444849 
    ## LogLike logistic: -711.42993049 
    ## step par values =0.6849044,0.2165203
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.685904371637684     
    ## recency KC..Default. 0.216520345255937     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444848 
    ## LogLike logistic: -711.43079708 
    ## step par values =0.6859044,0.2165203
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.683904371637684     
    ## recency KC..Default. 0.216520345255937     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444849 
    ## LogLike logistic: -711.42950824 
    ## step par values =0.6839044,0.2165203
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.684904371637684     
    ## recency KC..Default. 0.217520345255937     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444851 
    ## LogLike logistic: -711.42691901 
    ## step par values =0.6849044,0.2175203
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.684904371637684     
    ## recency KC..Default. 0.215520345255937     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444851 
    ## LogLike logistic: -711.42730896 
    ## step par values =0.6849044,0.2155203
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.643175243307196     
    ## recency KC..Default. 0.214309023717862     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444855 
    ## LogLike logistic: -711.42163441 
    ## step par values =0.6431752,0.214309
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.644175243307196     
    ## recency KC..Default. 0.214309023717862     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444855 
    ## LogLike logistic: -711.42167567 
    ## step par values =0.6441752,0.214309
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.642175243307196     
    ## recency KC..Default. 0.214309023717862     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444855 
    ## LogLike logistic: -711.42158667 
    ## step par values =0.6421752,0.214309
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.643175243307196     
    ## recency KC..Default. 0.215309023717862     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444858 
    ## LogLike logistic: -711.41753835 
    ## step par values =0.6431752,0.215309
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.643175243307196     
    ## recency KC..Default. 0.213309023717862     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444851 
    ## LogLike logistic: -711.42630294 
    ## step par values =0.6431752,0.213309
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638342385027005     
    ## recency KC..Default. 0.216872260852994     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444862 
    ## LogLike logistic: -711.41221335 
    ## step par values =0.6383424,0.2168723
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639342385027005     
    ## recency KC..Default. 0.216872260852994     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444862 
    ## LogLike logistic: -711.41219881 
    ## step par values =0.6393424,0.2168723
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.637342385027005     
    ## recency KC..Default. 0.216872260852994     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444862 
    ## LogLike logistic: -711.41224417 
    ## step par values =0.6373424,0.2168723
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638342385027005     
    ## recency KC..Default. 0.217872260852994     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444864 
    ## LogLike logistic: -711.40959092 
    ## step par values =0.6383424,0.2178723
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638342385027005     
    ## recency KC..Default. 0.215872260852994     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.44486 
    ## LogLike logistic: -711.41539771 
    ## step par values =0.6383424,0.2158723
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40430632 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429477 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40433918 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.222872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40399869 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.220872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40483677 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640928496164801     
    ## recency KC..Default. 0.222724131908131     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40435667 
    ## step par values =0.6409285,0.2227241
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.641928496164801     
    ## recency KC..Default. 0.222724131908131     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40437147 
    ## step par values =0.6419285,0.2227241
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639928496164801     
    ## recency KC..Default. 0.222724131908131     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40435654 
    ## step par values =0.6399285,0.2227241
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640928496164801     
    ## recency KC..Default. 0.223724131908131     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40505825 
    ## step par values =0.6409285,0.2237241
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640928496164801     
    ## recency KC..Default. 0.221724131908131     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40433377 
    ## step par values =0.6409285,0.2217241
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639902124522451     
    ## recency KC..Default. 0.222195722015061     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40430948 
    ## step par values =0.6399021,0.2221957
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640902124522451     
    ## recency KC..Default. 0.222195722015061     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.4042451 
    ## step par values =0.6409021,0.2221957
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638902124522451     
    ## recency KC..Default. 0.222195722015061     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40426891 
    ## step par values =0.6389021,0.2221957
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639902124522451     
    ## recency KC..Default. 0.223195722015061     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40459793 
    ## step par values =0.6399021,0.2231957
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639902124522451     
    ## recency KC..Default. 0.221195722015061     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40465949 
    ## step par values =0.6399021,0.2211957
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639464257608843     
    ## recency KC..Default. 0.221970293720424     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40465345 
    ## step par values =0.6394643,0.2219703
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640464257608843     
    ## recency KC..Default. 0.221970293720424     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40444718 
    ## step par values =0.6404643,0.2219703
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638464257608843     
    ## recency KC..Default. 0.221970293720424     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40448354 
    ## step par values =0.6384643,0.2219703
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639464257608843     
    ## recency KC..Default. 0.222970293720424     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40440672 
    ## step par values =0.6394643,0.2229703
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639464257608843     
    ## recency KC..Default. 0.220970293720424     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40482119 
    ## step par values =0.6394643,0.2209703
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639278454313169     
    ## recency KC..Default. 0.221874636070166     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.4046966 
    ## step par values =0.6392785,0.2218746
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640278454313169     
    ## recency KC..Default. 0.221874636070166     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40435955 
    ## step par values =0.6402785,0.2218746
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638278454313169     
    ## recency KC..Default. 0.221874636070166     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40471276 
    ## step par values =0.6382785,0.2218746
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639278454313169     
    ## recency KC..Default. 0.222874636070166     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40434517 
    ## step par values =0.6392785,0.2228746
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639278454313169     
    ## recency KC..Default. 0.220874636070166     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.4048994 
    ## step par values =0.6392785,0.2208746
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274680245199     
    ## recency KC..Default. 0.221872693055793     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40448888 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274680245199     
    ## recency KC..Default. 0.221872693055793     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429352 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274680245199     
    ## recency KC..Default. 0.221872693055793     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40451913 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274680245199     
    ## recency KC..Default. 0.222872693055793     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40399971 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274680245199     
    ## recency KC..Default. 0.220872693055793     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40480874 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.221872692311738     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40437479 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274678799962     
    ## recency KC..Default. 0.221872692311738     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429475 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274678799962     
    ## recency KC..Default. 0.221872692311738     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40433709 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.222872692311738     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40399857 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.220872692311738     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40524356 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40430632 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429477 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40433918 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.222872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40399869 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.220872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40483677 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40430632 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429477 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40433918 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.222872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40399869 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.220872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40483677 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.221872692311738     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40437494 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274678799962     
    ## recency KC..Default. 0.221872692311738     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429411 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274678799962     
    ## recency KC..Default. 0.221872692311738     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40451069 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.222872692311738     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40434532 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.220872692311738     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40490092 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40430632 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429477 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40433918 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.222872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40399869 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.220872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40483677 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40430632 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429477 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40433918 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.222872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40399869 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.220872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40483677 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.221872692311738     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40430808 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274678799962     
    ## recency KC..Default. 0.221872692311738     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429088 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274678799962     
    ## recency KC..Default. 0.221872692311738     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40434011 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.222872692311738     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40399831 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.220872692311738     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40491237 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40430632 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429477 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40433918 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.222872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40399869 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.220872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40483677 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40430632 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429477 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40433918 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.222872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40399869 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.220872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40483677 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40438739 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429201 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40433731 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.222872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40399853 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.220872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40480954 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40430632 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429477 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40433918 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.222872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40399869 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.220872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40483677 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40430632 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429477 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40433918 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.222872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40399869 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.220872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40483677 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40448236 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40466917 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.404406 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.222872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.4039983 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.220872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40490337 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40430632 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429477 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40433918 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.222872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40399869 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.220872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40483677 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40430632 
    ## step par values =0.6392747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429477 
    ## step par values =0.6402747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638274678799962     
    ## recency KC..Default. 0.221872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40433918 
    ## step par values =0.6382747,0.2218727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.222872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40399869 
    ## step par values =0.6392747,0.2228727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639274678799962     
    ## recency KC..Default. 0.220872692311737     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40483677 
    ## step par values =0.6392747,0.2208727
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.661479748710759     
    ## recency KC..Default. 0.640909574902     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.425547 
    ## LogLike logistic: -736.16544876 
    ## step par values =0.6614797,0.6409096
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.662479748710759     
    ## recency KC..Default. 0.640909574902     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.425547 
    ## LogLike logistic: -736.16549244 
    ## step par values =0.6624797,0.6409096
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.660479748710759     
    ## recency KC..Default. 0.640909574902     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.425547 
    ## LogLike logistic: -736.16542345 
    ## step par values =0.6604797,0.6409096
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.661479748710759     
    ## recency KC..Default. 0.641909574902     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.42547 
    ## LogLike logistic: -736.26378205 
    ## step par values =0.6614797,0.6419096
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.661479748710759     
    ## recency KC..Default. 0.639909574902     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.425618 
    ## LogLike logistic: -736.07425209 
    ## step par values =0.6614797,0.6399096
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330716564843     
    ## recency KC..Default. 0.222930193611993     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40401436 
    ## step par values =0.6393307,0.2229302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640330716564843     
    ## recency KC..Default. 0.222930193611993     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40412377 
    ## step par values =0.6403307,0.2229302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638330716564843     
    ## recency KC..Default. 0.222930193611993     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40404379 
    ## step par values =0.6383307,0.2229302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330716564843     
    ## recency KC..Default. 0.223930193611993     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40504618 
    ## step par values =0.6393307,0.2239302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330716564843     
    ## recency KC..Default. 0.221930193611993     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40446718 
    ## step par values =0.6393307,0.2219302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639286207062151     
    ## recency KC..Default. 0.222498857514154     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40427537 
    ## step par values =0.6392862,0.2224989
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640286207062151     
    ## recency KC..Default. 0.222498857514154     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40463636 
    ## step par values =0.6402862,0.2224989
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638286207062151     
    ## recency KC..Default. 0.222498857514154     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40430197 
    ## step par values =0.6382862,0.2224989
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639286207062151     
    ## recency KC..Default. 0.223498857514154     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40472229 
    ## step par values =0.6392862,0.2234989
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639286207062151     
    ## recency KC..Default. 0.221498857514154     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40442993 
    ## step par values =0.6392862,0.2214989
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639328009577292     
    ## recency KC..Default. 0.222903960529702     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.404359 
    ## step par values =0.639328,0.222904
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640328009577292     
    ## recency KC..Default. 0.222903960529702     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.4039898 
    ## step par values =0.640328,0.222904
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638328009577292     
    ## recency KC..Default. 0.222903960529702     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40438583 
    ## step par values =0.638328,0.222904
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639328009577292     
    ## recency KC..Default. 0.223903960529702     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40546377 
    ## step par values =0.639328,0.223904
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639328009577292     
    ## recency KC..Default. 0.221903960529702     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40436605 
    ## step par values =0.639328,0.221904
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330706723091     
    ## recency KC..Default. 0.22293009823679     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40401439 
    ## step par values =0.6393307,0.2229301
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640330706723091     
    ## recency KC..Default. 0.22293009823679     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.4041249 
    ## step par values =0.6403307,0.2229301
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638330706723091     
    ## recency KC..Default. 0.22293009823679     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40404531 
    ## step par values =0.6383307,0.2229301
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330706723091     
    ## recency KC..Default. 0.22393009823679     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40506299 
    ## step par values =0.6393307,0.2239301
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330706723091     
    ## recency KC..Default. 0.22193009823679     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429248 
    ## step par values =0.6393307,0.2219301
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330715835399     
    ## recency KC..Default. 0.222930186543044     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40401393 
    ## step par values =0.6393307,0.2229302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640330715835399     
    ## recency KC..Default. 0.222930186543044     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.4041239 
    ## step par values =0.6403307,0.2229302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638330715835399     
    ## recency KC..Default. 0.222930186543044     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40404424 
    ## step par values =0.6383307,0.2229302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330715835399     
    ## recency KC..Default. 0.223930186543044     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.4053036 
    ## step par values =0.6393307,0.2239302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330715835399     
    ## recency KC..Default. 0.221930186543044     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40435967 
    ## step par values =0.6393307,0.2219302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330715707672     
    ## recency KC..Default. 0.22293018530526     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40401408 
    ## step par values =0.6393307,0.2229302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640330715707672     
    ## recency KC..Default. 0.22293018530526     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40412388 
    ## step par values =0.6403307,0.2229302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638330715707672     
    ## recency KC..Default. 0.22293018530526     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40404425 
    ## step par values =0.6383307,0.2229302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330715707672     
    ## recency KC..Default. 0.22393018530526     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40530359 
    ## step par values =0.6393307,0.2239302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330715707672     
    ## recency KC..Default. 0.22193018530526     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40429407 
    ## step par values =0.6393307,0.2219302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330715835318     
    ## recency KC..Default. 0.222930186542266     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40401464 
    ## step par values =0.6393307,0.2229302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640330715835318     
    ## recency KC..Default. 0.222930186542266     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40412481 
    ## step par values =0.6403307,0.2229302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638330715835318     
    ## recency KC..Default. 0.222930186542266     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40439219 
    ## step par values =0.6383307,0.2229302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330715835318     
    ## recency KC..Default. 0.223930186542266     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.40506177 
    ## step par values =0.6393307,0.2239302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330715835318     
    ## recency KC..Default. 0.221930186542266     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40437071 
    ## step par values =0.6393307,0.2219302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330715835399     
    ## recency KC..Default. 0.222930186543044     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40401393 
    ## step par values =0.6393307,0.2229302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.640330715835399     
    ## recency KC..Default. 0.222930186543044     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.4041239 
    ## step par values =0.6403307,0.2229302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.638330715835399     
    ## recency KC..Default. 0.222930186543044     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40404424 
    ## step par values =0.6383307,0.2229302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330715835399     
    ## recency KC..Default. 0.223930186543044     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444868 
    ## LogLike logistic: -711.4053036 
    ## step par values =0.6393307,0.2239302
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.639330715835399     
    ## recency KC..Default. 0.221930186543044     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.444869 
    ## LogLike logistic: -711.40435967 
    ## step par values =0.6393307,0.2219302

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
    ## McFadden's R2 logistic: 0.447458 
    ## LogLike logistic: -708.08582913

base4
=====

    modelob <- LKT(
      data = val, interc=TRUE,
      components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
      features = c("intercept", "intercept", "base4","logitdec"),
      fixedpars=c(0.1890747,0.6309054,0.05471752,0.99999,0.2160748))

    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## base4 KC..Default. 0.1890747 0.6309054 0.05471752 0.99999  
    ## logitdec KC..Default. 0.2160748     
    ## logitdecKC..Default.+base4KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.439751 
    ## LogLike logistic: -717.96307079

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
    ## McFadden's R2 logistic: 0.209249 
    ## LogLike logistic: -1013.35259968

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
    ## McFadden's R2 logistic: 0.307606 
    ## LogLike logistic: -887.30713311

    mean(modelob$cv_res$mcfad)

    ## [1] -0.0647124

    #complex AFM minus student intercept
    modelob <- LKT(
      data = val, interc=TRUE,
      components = c("KC..Default.","KC..Default."),
      features = c("intercept$", "lineafm$"),
      cv = TRUE)

    ## intercept$ KC..Default.      
    ## lineafm$ KC..Default.      
    ## lineafmKC..Default.:e$data$KC..Default.+interceptKC..Default.:e$data$KC..Default.+1 
    ## McFadden's R2 logistic: 0.41987 
    ## LogLike logistic: -743.4396316

    mean(modelob$cv_res$mcfad)

    ## [1] -Inf
