LKT [![](https://cranlogs.r-pkg.org/badges/LKT)](https://cran.r-project.org/package=LKT)
========================================================================================

Examples below
==============

Please see the manual and vignette for more information.

Load data
=========

Get the data free:
<a href="https://datashop.memphis.edu/DatasetInfo?datasetId=1465" class="uri">https://datashop.memphis.edu/DatasetInfo?datasetId=1465</a>

    library(LKT)

    ## Warning: package 'LKT' was built under R version 4.0.5

    # data.table is the base data type
    library(data.table)
    datafile<-"ds1465_tx_All_Data_64_2016_0720_222352.txt" 
    val<-read.table(colClasses = c("Anon.Student.Id"="character"),datafile,sep="\t", header=TRUE,quote="\"")

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

Additive Factors Model (AFM) fixed effect version
=================================================

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

Performance Factors Analysis (PFA) fixed effect version
=======================================================

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

PFA using difficulty sensitive predictors (composite model requiring pred from prior model)
===========================================================================================

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

Recent Performance Factors Analysis (RPFA)
==========================================

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

Recency tracing with logitdec and transfer from cluster
=======================================================

    modelob <- LKT(
      data = val, interc=TRUE,
      components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default.","KC..Cluster."),
      features = c("intercept", "intercept", "logitdec","recency","logitdec"),
      fixedpars=c(.9,.5,.5))

    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## logitdec KC..Default. 0.9     
    ## recency KC..Default. 0.5     
    ## logitdec KC..Cluster. 0.5     
    ## logitdecKC..Cluster.+recencyKC..Default.+logitdecKC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.330848 
    ## LogLike logistic: -25416.73506278

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
    ## Many search iterations
    ## 
    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## propdec2 KC..Default. 0.5325893128318     
    ## recency KC..Default. 0.324971232690059     
    ## recencyKC..Default.+propdec2KC..Default.+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.352264 
    ## LogLike logistic: -24603.2739021 
    ## step par values =0.5325893,0.3249712

Performance Prediction Equation (PPE)
=====================================

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

individualized Additive Factors Model (iAFM) fixed effect version
=================================================================

    modelob <- LKT(
      data = val, interc=TRUE,
      components = c("Anon.Student.Id","KC..Default.","KC..Default."),
      features = c("intercept", "intercept", "lineafm"),
      covariates = c(NA,NA,"Anon.Student.Id"))

    ## intercept Anon.Student.Id      
    ## intercept KC..Default.      
    ## lineafm KC..Default.      
    ## lineafmKC..Default.:Anon.Student.Id+interceptKC..Default.+interceptAnon.Student.Id+1 
    ## McFadden's R2 logistic: 0.301758 
    ## LogLike logistic: -26521.6774637

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

    ## [1] 0.1748922

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

    ## [1] 0.1746848

References
==========

Anderson, J. R., Fincham, J. M., & Douglass, S. (1999). Practice and
retention: A unifying analysis. *Journal of Experimental Psychology:
Learning, Memory, & Cognition*, *25*(5), 1120–1136.

Atkinson, R. C. (1972). Ingredients for a theory of instruction.
*American Psychologist*, *27*(10), 921–931.

Barnes, T. (2005). The Q-matrix method: Mining student response data for
knowledge. In *20th Nat. Conf. American Association for Artificial
Intelligence, Educational Data Mining Workshop* (pp. 39–46). AAAI Press.

Benjamini, Y., & Yekutieli, D. (2001). The control of the false
discovery rate in multiple testing under dependency. *Ann. Statist.*,
*29*(4), 1165–1188.
<https://projecteuclid.org:443/euclid.aos/1013699998>

Cao, M., Pavlik Jr, P. I., & Bidelman, G. M. (2019). Incorporating Prior
Practice Difficulty into Performance Factor Analysis to Model Mandarin
Tone Learning. In *11th Int. Conf. Educational Data Mining* (pp.
516–519).

Carpenter, S. K., Pashler, H., Wixted, J. T., & Vul, E. (2008). The
effects of tests on learning and forgetting. *Memory & Cognition*,
*36*(2), 438–448.

Carvalho, P. F., & Goldstone, R. L. (2019). A computational model of
context-dependent encodings during category learning. *PsyArXiv*, 1–42.

Cen, H., Koedinger, K. R., & Junker, B. (2007). Is over practice
necessary? – Improving learning efficiency with the Cognitive Tutor
through educational data mining. In *13th Int. Conf. Artificial
Intelligence in Education* (pp. 511–518).

Chi, M., Koedinger, K. R., Gordon, G., Jordan, P., & VanLehn, K. (2011).
Instructional factors analysis: A cognitive model for multiple
instructional interventions. In *4th Int. Conf. Educational Data Mining*
(pp. 61–70).

Collins, M. G., Gluck, K. A., Walsh, M., Krusmark, M., & Gunzelmann, G.
(2016). Using prior data to inform model parameters in the predictive
performance equation. In *CogSci 2016* (pp. 75–80). Cognitive Science
Society.

Corbett, A. T., & Anderson, J. R. (1992). Student modeling and mastery
learning in a computer-based programming tutor. In *2nd Int. Conf.
Intelligent Tutoring Systems* (pp. 413–420). Springer-Verlag.

Corbett, A. T., & Anderson, J. R. (1994). Knowledge tracing: Modeling
the acquisition of procedural knowledge. *User Modeling and User-Adapted
Interaction*, *4*(4), 253–278.

De Boeck, P., Bakker, M., Zwitser, R., Nivard, M., Hofman, A.,
Tuerlinckx, F., & Partchev, I. (2011). The estimation of item response
models with the lmer function from the lme4 package in R. *Journal of
Statistical Software*, *39*(12), 1–28.

Eglington, L. G., & Pavlik Jr, P. I. (2020). Optimizing practice
scheduling requires quantitative tracking of individual item
performance. *Npj Science of Learning*, *5*(1), 15.
<https://doi.org/10.1038/s41539-020-00074-4>

Fischer, G. H. (1973). The linear logistic test model as an instrument
in educational research. *Acta Psychologica*, *37*(6), 359–374.
<http://www.sciencedirect.com/science/article/pii/0001691873900036>

Galyardt, A., & Goldin, I. (2014). Recent-performance factors analysis.
In *7th Int. Conf. Educational Data Mining* (pp. 411–412).

Galyardt, A., & Goldin, I. (2015). Move your lamp post: Recent data
reflects learner knowledge better than older data. *Journal of
Educational Data Mining*, *7*(2), 83–108.
<https://doi.org/10.5281/zenodo.3554671>

Gelman, A., & Hill, J. (2006). *Data analysis using regression and
multilevel/hierarchical models*. Cambridge Univ. Press, Cambridge, U.K.

Gervet, T., Koedinger, K., Schneider, J., & Mitchell, T. (2020). When is
Deep Learning the Best Approach to Knowledge Tracing? *JEDM| Journal of
Educational Data Mining*, *12*(3), 31–54.

Goldin, I. M., & Galyardt, A. (2015). Convergent validity of a student
model: Recent-performance factors analysis. In *8th Int. Conf.
Educational Data Mining* (Vols. 548-5551, pp. 548–551).

Gong, Y., Beck, J. E., & Heffernan, N. T. (2011). How to construct more
accurate student models: Comparing and optimizing knowledge tracing and
performance factor analysis. *International Journal of Artificial
Intelligence in Education*, *21*(1), 27–46.
<https://doi.org/10.3233/JAI-2011-016>

Gong, Y., Beck, J., & Heffernan, N. T. (2010). Comparing knowledge
tracing and performance factor analysis by using multiple model fitting
procedures. In *10th Int. Conf. Intelligent Tutoring Systems* (Vol.
6094, pp. 35–44). Springer Berlin / Heidelberg.
<https://doi.org/10.1007/978-3-642-13388-6_8>

Katz, S., Connelly, J., & Wilson, C. (2007). Out of the lab and into the
classroom: An evaluation of reflective dialogue in Andes. *Frontiers in
Artificial Intelligence and Applications*, *158*, 425–432.

Kelley, C. R. (1969). What is adaptive training? *Human Factors*,
*11*(6), 547–556. <https://doi.org/10.1177/001872086901100602>

Khajah, M. M., Lindsey, R. V., & Mozer, M. C. (2014). Maximizing
students’ retention via spaced review: Practical guidance from
computational models of memory. *Topics in Cognitive Science*, *6*(1),
157–169. <https://doi.org/10.1111/tops.12077>

Koedinger, K. R., Baker, R. S., Cunningham, K., Skogsholm, A., Leber,
B., & Stamper, J. (2010). A data repository for the EDM community: The
PSLC DataShop. In *Handb. Educ. Data Min.* (Vol. 43, pp. 43–56). CRC
Press.

Koedinger, K. R., Corbett, A. T., & Perfetti, C. (2012). The
knowledge-learning-instruction framework: Bridging the science-practice
chasm to enhance robust student learning. *Cognitive Science*, *36*(5),
757–798. <https://doi.org/10.1111/j.1551-6709.2012.01245.x>

Koedinger, K. R., Yudelson, M. V., & Pavlik, P. I. (2016). Testing
theories of transfer using error rate learning curves. *Topics in
Cognitive Science*, published online.
<https://doi.org/10.1111/tops.12208>

Learning Center, P. S. of. (n.d.-a). *DataShop@CMU*.
<https://pslcdatashop.web.cmu.edu/>

Learning Center, P. S. of. (n.d.-b). *DataShop@Memphis*.
<https://datashop.memphis.edu>

Lindsey, R. V., Shroyer, J. D., Pashler, H., & Mozer, M. C. (2014).
Improving students’ long-term knowledge retention through personalized
review. *Psychological Science*, 639–647.
<http://pss.sagepub.com/content/early/2014/01/17/0956797613504302.abstract>

Lord, F. M. (1953). The relation of test score to the trait underlying
the test. *Educational and Psychological Measurement*, *13*, 517–549.

Mitrovic, A., & Martin, B. (2007). Evaluating the effect of open student
models on self-assessment. *International Journal of Artificial
Intelligence in Education*, *17*(2), 121–144.
<http://iospress.metapress.com/content/XL475M4828420465>

Pardos, Z., & Dadu, A. (2018). dAFM: Fusing psychometric and
connectionist modeling for Q-matrix refinement. *Journal of Educational
Data Mining*, *10*(2), 1–27.

Pavlik Jr., P. I., & Anderson, J. R. (2005). Practice and forgetting
effects on vocabulary memory: An activation-based model of the spacing
effect. *Cognitive Science*, *29*(4), 559–586.

Pavlik Jr., P. I., & Anderson, J. R. (2008). Using a model to compute
the optimal schedule of practice. *Journal of Experimental Psychology:
Applied*, *14*(2), 101–117.

Pavlik Jr., P. I., Bolster, T., Wu, S., Koedinger, K. R., & MacWhinney,
B. (2008). Using optimally selected drill practice to train basic facts.
In *9th Int. Conf. Intelligent Tutoring Systems* (pp. 593–602).

Pavlik Jr., P. I., Cen, H., & Koedinger, K. R. (2009). Performance
factors analysis – A new alternative to knowledge tracing. In *14th Int.
Conf. Artificial Intelligence in Education* (pp. 531–538).

Pavlik Jr, P. I., & Eglington, L. G. (n.d.). *LKT*.
<https://github.com/Optimal-Learning-Lab/LKT>

Pavlik Jr., P. I., Olney, A. M., Banker, A., Eglington, L., & Yarbro, J.
(2020). The Mobile Fact and Concept Textbook System (MoFaCTS). In *21st
Int. Conf. on Artificial Intelligence in Education, 2nd Workshop on
Intelligent Textbooks* (pp. 35–49). In CEUR workshop proceedings (Vol.
2674).

Pavlik Jr., P. I., Yudelson, M., & Koedinger, K. R. (2011). Using
contextual factors analysis to explain transfer of least common multiple
skills. In *15th Int. Conf. Artificial Intelligence in Education* (Vol.
6738, pp. 256–263). Springer.
<https://doi.org/10.1007/978-3-642-21869-9_34>

Pavlik Jr., P. I., Yudelson, M., & Koedinger, K. R. (2015). A
measurement model of microgenetic transfer for improving instructional
outcomes. *International Journal of Artificial Intelligence in
Education*, *25*, 346–379. <https://doi.org/10.1007/s40593-015-0039-y>

Pelánek, R. (2017). Bayesian knowledge tracing, logistic models, and
beyond: an overview of learner modeling techniques. *User Modeling and
User-Adapted Interaction*, *27*(3), 313–350.
<https://doi.org/10.1007/s11257-017-9193-2>

Peterson, L. R. (1965). Paired-associate latencies after the last error.
*Psychonomic Science*, *2*(6), 167–168.

Rasch, G. (1966). An item analysis which takes individual differences
into account. *British Journal of Mathematical and Statistical
Psychology*, *19*(1), 49–57.
<https://doi.org/10.1111/j.2044-8317.1966.tb00354.x>

Razzaq, L., Feng, M., Nuzzo-Jones, G., Heffernan, N. T., Koedinger, K.
R., Junker, B., Ritter, S., Knight, A., Aniszczyk, C., Chokesy, S.,
Livak, T., Mercado, E., Turner, T. E., Upalekar, R., Walanoski, J. A.,
Macasek, M. A., & Rasmussen, K. P. (2005). The Assistment Project:
Blending assessment and assisting. In *12th Int. Conf. Artificial
Intelligence in Education* (pp. 555–562). ISO Press.

Rickard, T. C. (1997). Bending the power law: A CMPL theory of strategy
shifts and the automatization of cognitive skills. *Journal of
Experimental Psychology: General*, *126*(3), 288–311.

Rickard, T. C. (1999). A CMPL alternative account of practice effects in
numerosity judgment tasks. *Journal of Experimental Psychology:
Learning, Memory, & Cognition*, *25*(2), 532–542.

Rosé, C. P., McLaughlin, E. A., Liu, R., & Koedinger, K. R. (2019).
Explanatory learner models: Why machine learning (alone) is not the
answer. *British Journal of Educational Technology*, *50*(6), 2943–2958.
<https://doi.org/10.1111/bjet.12858>

Scheiblechner, H. (1972). Das lernen und losen komplexer denkaufgaben.
*Zeitschrift Fur Experimentelle Und Angewandte Psychologie*, *19*,
476–506.
[&lt;Go to WoS&gt;://A1993MJ80100006](%3CGo%20to%20WoS%3E://A1993MJ80100006)

Segalowitz, N. S., & Segalowitz, S. J. (1993). Skilled performance,
practice, and the differentiation of speed-up from automatization
effects - evidence from 2nd-language word recognition. *Applied
Psycholinguistics*, *14*(3), 369–385.
[&lt;Go to ISI&gt;://A1993MJ80100006](%3CGo%20to%20ISI%3E://A1993MJ80100006)

Settles, B., & Meeder, B. (2016). A trainable spaced repetition model
for language learning. In *Proc. 54th Association for Computational
Linguistics* (pp. 1848–1858).

Spada, H., & McGaw, B. (1985). The assessment of learning effects with
linear logistic test models. In *Test Des. Dev. Psychol. Psychometrics*
(pp. 169–194). Academic Press.

Squire, L. R. (1992). Declarative and nondeclarative memory: multiple
brain systems supporting learning and memory. *Journal of Cognitive
Neuroscience*, *4*(3), 232–243.

Stamper, J., & Koedinger, K. (2011). Human-machine student model
discovery and improvement using DataShop. In *15th Int. Conf. Artificial
Intelligence in Education* (Vol. 6738, pp. 353–360). Springer.
<https://doi.org/10.1007/978-3-642-21869-9_46>

Stamper, J., Koedinger, K., Pavlik Jr., P. I., Rose, C., Liu, R., Eagle,
M., Yudelson, M., & Veeramachaneni, K. (2016). Educational data analysis
using LearnSphere workshop. In *9th Int. Conf. on Educational Data
Mining*. <http://ceur-ws.org/Vol-1633/>

Stamper, J., & Pardos, Z. (2016). The 2010 KDD Cup competition dataset:
Engaging the machine learning community in predictive learning
analytics. *Journal of Learning Analytics*, *3*(2), 312–316.

Taatgen, N. A., & Lee, F. J. (2003). Production compilation: Simple
mechanism to model complex skill acquisition. *Human Factors*, *45*(1),
61–76.

Tatsuoka, K. K. (1983). Rule space: An approach for dealing with
misconceptions based on item response theory. *Journal of Educational
Measurement*, *20*(4), 345–354.

Thompson, C. P., Wenger, S. K., & Bartling, C. A. (1978). How recall
facilitates subsequent recall: A reappraisal. *Journal of Experimental
Psychology: Human Learning & Memory*, *4*(3), 210–221.

Walsh, M. M., Gluck, K. A., Gunzelmann, G., Jastrzembski, T., Krusmark,
M., Myung, J. I., Pitt, M. A., & Zhou, R. (2018). Mechanisms underlying
the spacing effect in learning: A comparison of three computational
models. *Journal of Experimental Psychology: General*, *147*(9),
1325–1348.

Wickelgren, W. A. (1974). Single-trace fragility theory of memory
dynamics. *Memory & Cognition*, *2*(4), 775–780.

Wickens, T. D., & Izawa, C. (1999). Measuring the time course of
retention. In *On human memory: Evolution, progress, and reflections on
the 30th anniversary of the Atkinson-Shiffrin model* (pp. 245–266).
Lawrence Erlbaum, Mahwah, NJ.

Yan, V. X., Schuetze, B. A., & Eglington, L. G. (2020). A review of the
interleaving effect: Theories and lessons for future research.
*PsyArXiv*, 1–39.
