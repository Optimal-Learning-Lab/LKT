# Model search and LASSO helpers for LKT.
# These functions build on LKT() and the feature/model modules.


#' Search for an LKT model specification
#'
#' @description Runs forward and/or backward stepwise search over candidate component-feature pairs, tracking nonlinear parameters and common fit metrics. This is a legacy public model-selection helper used by the vignette examples.
#'
#' @param data Data frame or data table containing `Anon.Student.Id`, `CF..ansbin.`, component columns, and optional `fold`.
#' @param usefolds Numeric vector of folds passed to `LKT()` for fitting.
#' @param allcomponents Character vector of candidate component columns.
#' @param allfeatures Character vector of candidate feature names.
#' @param currentcomponents Character vector of component columns in the starting model.
#' @param specialcomponents Character vector of components paired one-to-one with `specialfeatures` and included outside the crossed search grid.
#' @param specialfeatures Character vector of special feature names paired with `specialcomponents`.
#' @param forv Minimum improvement required to add a term.
#' @param bacv Maximum loss tolerated when removing a term.
#' @param preset Optional preset model family: `"static"`, `"AFM"`, `"PFA"`, `"advanced"`, `"AFMLLTM"`, `"PFALLTM"`, or `"advancedLLTM"`.
#' @param presetint Logical; include preset intercept terms when `TRUE`.
#' @param currentfeatures Character vector of feature names in the starting model.
#' @param verbose Logical passed to `LKT()`.
#' @param currentfixedpars Numeric vector of fixed parameter values for the starting model.
#' @param maxitv Maximum nonlinear optimizer iterations passed to `LKT()`.
#' @param interc Logical passed to `LKT()` to include a global intercept.
#' @param forward Logical; allow forward additions.
#' @param backward Logical; allow backward removals.
#' @param metric Selection metric. One of `"BIC"`, `"AUC"`, `"AIC"`, `"RMSE"`, or `"R2"`.
#' @param removefeat Character vector of feature names to exclude from the crossed candidate list.
#' @param removecomp Character vector of component names paired with `removefeat` to exclude from the crossed candidate list.
#' @return A list containing `tracetable` search history and the final `currentfit` LKT model.
#' @examples
#' \donttest{
#' data(samplelkt)
#' samplelkt$CF..ansbin. <- as.integer(samplelkt$Outcome == "CORRECT")
#' searched <- buildLKTModel(
#'   data = samplelkt,
#'   allcomponents = "KC..Default.",
#'   allfeatures = "lineafm",
#'   forv = 0,
#'   bacv = 0,
#'   interc = TRUE,
#'   verbose = FALSE
#' )
#' names(searched)
#' }
#' @export
buildLKTModel <- function(data,usefolds = NA,
                          allcomponents,allfeatures,
                          currentcomponents=c(),specialcomponents=c(),specialfeatures=c()
                          ,forv,bacv,preset=NA,presetint=T,
                          currentfeatures=c(),verbose=FALSE,
                          currentfixedpars =c(),maxitv=10,interc = FALSE,
                          forward= TRUE, backward=TRUE, metric="BIC",removefeat=c(), removecomp=c()){


  if(is.na(usefolds)[1])
  {data$fold<-1
  usefolds=1}

  if(is.null(data$fold))
  {data$fold<-1
    usefolds=1}


  if(!is.na(preset)){if(preset=="static"){allfeatures<-c("intercept")}


    if(preset=="AFM"){allfeatures<-c("intercept","lineafm","logafm","lineafm")}

    if(preset=="AFMLLTM"){allfeatures<-c("intercept","lineafm","logafm","lineafm","lineafm$","logafm$","lineafm$")}

    if(preset=="PFA"){allfeatures<-c("intercept","lineafm","logafm","lineafm", "logsuc","logfail",  "linesuc","linefail")}

    if(preset=="PFALLTM"){allfeatures<-c("intercept","lineafm","logafm","lineafm", "logsuc","logfail",  "linesuc","linefail"
                                         ,"lineafm$","logafm$","lineafm$", "logsuc$","logfail$",  "linesuc$","linefail$")}

    if(preset=="advanced"){allfeatures<-c("intercept","lineafm","logafm","lineafm", "logsuc","logfail",
                                          "linesuc","linefail", "logitdec","propdec","recency","base")}

    if(preset=="advancedLLTM"){allfeatures<-c("intercept","lineafm","logafm","lineafm", "logsuc","logfail",
                                              "linesuc","linefail", "logitdec","propdec","recency","base","lineafm$","logafm$","lineafm$", "logsuc$","logfail$",
                                              "linesuc$","linefail$")}
    if(presetint==F){allfeatures<-allfeatures[allfeatures!="intercept"]}
  }
  lkt_feature_param_counts(c(allfeatures, currentfeatures, specialfeatures))
  if (length(currentcomponents) != length(currentfeatures)) {
    stop("currentcomponents and currentfeatures must have the same length.",
         call. = FALSE)
  }
  if (length(specialcomponents) != length(specialfeatures)) {
    stop("specialcomponents and specialfeatures must have the same length.",
         call. = FALSE)
  }
  currentfit<-list()
  startfitscor <- Inf
  currentfitscore<- Inf
  k<-0
  paramvec<-currentfixedpars
  compstat<- c()

  feature_param_count <- function(feature) {
    lkt_feature_param_counts(feature)
  }

  materialize_fixedpars <- function(fixedpars, optimizedpars) {
    fixedpars <- as.numeric(fixedpars)
    if (length(fixedpars) == 0) {
      return(fixedpars)
    }
    if (!is.atomic(optimizedpars) && !is.null(optimizedpars$par)) {
      missing_idx <- which(is.na(fixedpars))
      fixedpars[missing_idx] <- optimizedpars$par[seq_along(missing_idx)]
    }
    fixedpars
  }

  tracetable<- as.data.frame(matrix(data=NA,nrow=0,ncol=10))
  x<-c("comp","feat","r2","ind","params","BIC","AUC","AIC","RMSE","action")
  colnames(tracetable)<-x

  if(length(currentcomponents)>0){
    fixedparct<-0
    for(ct in currentfeatures){
      fixedparct<-fixedparct+feature_param_count(ct)}
    currentfit<-LKT(data = data, usefolds=usefolds,interc=interc,maxitv=maxitv,verbose=verbose,
                    components = currentcomponents,
                    features = currentfeatures,fixedpars = ifelse(is.na(currentfixedpars),rep(NA,fixedparct),currentfixedpars)
                    )

    BICis<- (length(currentfit$coefs)+fixedparct)*log(length(currentfit$prediction))-2*currentfit$loglik
    AUCis<- suppressMessages(pROC::auc(data$CF..ansbin., currentfit$prediction)[1])
    AICis<- (length(currentfit$coefs)+fixedparct)*2-2*currentfit$loglik
    RMSEis<- sqrt(mean((data$CF..ansbin.[data$fold %in% usefolds]-currentfit$prediction[data$fold %in% usefolds])^2))
  cat("\nStep",k,"results - pars ",length(currentfit$coefs)+fixedparct," current BIC",BICis,"current AIC",AICis,"current AUC",AUCis,
                         "current RMSE",RMSEis," McFadden's R2",currentfit$r2,"\n")
    cat(currentfeatures,"\n",currentcomponents,"\n")
    if(!is.atomic(currentfit$optimizedpars)){
      cat("pars",currentfit$optimizedpars$par,"\n")
      paramvec<-currentfit$optimizedpars$par}
    tracetable[nrow(tracetable) + 1,] =
      list(comp=paste(currentcomponents,collapse=" "),feat=paste(currentfeatures,collapse=" "),r2=currentfit$r2,ind=0,params=length(currentfit$coefs)+fixedparct,
           BIC=BICis,AUC=AUCis,AIC=AICis,RMSE=RMSEis,action=paste("starting model"))

    switch(metric,
           "AUC" = {
             currentfitscore<- -AUCis},
           "AIC" = {
             currentfitscore<-AICis},
           "BIC" = {
             currentfitscore<-BICis},
           "RMSE" = {
             currentfitscore<-RMSEis},
           "R2" = {
             currentfitscore<- -currentfit$r2})
  } else {
    meancor<-mean(data$CF..ansbin.)
    ll<- sum(log(ifelse(data$CF..ansbin.[data$fold %in% usefolds]==1,meancor,1-meancor)))
    BICis<- log(length(data$CF..ansbin.[data$fold %in% usefolds]))-2*ll
    AUCis<- .5
    AICis<- 2-2*ll
    RMSEis<- sqrt(mean((data$CF..ansbin.[data$fold %in% usefolds]-mean(data$CF..ansbin.[data$fold %in% usefolds]))^2))
    tracetable[nrow(tracetable) + 1,] =
      list(comp="none",feat="none",r2=0,ind=0,params=1,
           BIC=BICis,AUC=AUCis,AIC=AICis,RMSE=RMSEis,action=paste("null model"))

    switch(metric,
           "AUC" = {
             currentfitscore<- -AUCis},
           "AIC" = {
             currentfitscore<-AICis},
           "BIC" = {
             currentfitscore<-BICis},
           "RMSE" = {
             currentfitscore<-RMSEis},
           "R2" = {
             currentfitscore<- 0})
  }

  # create null model also and put that on graph

  while (is.infinite(startfitscor) | currentfitscore!=startfitscor){
    startfitscor<-currentfitscore

    k<-k+1
    cat(white$bgBlack$bold("\nStep ",k,"start\n"))
    bestmod<-NULL
    bestfixedpars<-NULL
    if(forward){
      cat("\ntrying to add\n")
      testtable<- as.data.frame(matrix(data=NA,nrow=0,ncol=10))
      x<-c("comp","feat","r2","ind","params","BIC","AUC","AIC","RMSE","action")
      colnames(testtable)<-x
      ij<-0
      complist<-c()
      featlist<-c()
      for(i in allcomponents){
        for(j in allfeatures){
          complist<-c(complist,i)
          featlist<-c(featlist,j)
        }}

      complist<-c(specialcomponents, complist)
      featlist<-c(specialfeatures, featlist)
      combined_list <- paste(complist, featlist, sep = "_")
      remove_list <- paste(removecomp, removefeat, sep = "_")
      indices_to_keep <- !combined_list %in% remove_list
      complist <- complist[indices_to_keep]
      featlist <- featlist[indices_to_keep]
      for(w in 1:length(complist)){
        i<-complist[w]
        j<-featlist[w]


        if(sum(paste(i,j) == data.frame(paste(currentcomponents,currentfeatures)))==1) next
        ij<-ij+1
        testfeatures <- c(currentfeatures,j)
        testcomponents <- c(currentcomponents,i)
        fixedparct<-0
        for(ct in testfeatures){
          fixedparct<-fixedparct+feature_param_count(ct)}
        testfixedpars <- c(paramvec, rep(NA, feature_param_count(j)))
        fittest<-LKT(data = data,usefolds=usefolds, interc=interc,maxitv=maxitv,verbose=verbose,
                      components = testcomponents,
                     features = testfeatures,fixedpars = testfixedpars)

        BICis<- (length(fittest$coefs)+fixedparct)*log(length(fittest$prediction))-2*fittest$loglik
        AUCis<- suppressMessages(pROC::auc(data$CF..ansbin., fittest$prediction)[1])
        AICis<- (length(fittest$coefs)+fixedparct)*2-2*fittest$loglik
        RMSEis<- sqrt(mean((data$CF..ansbin.[data$fold %in% usefolds]-fittest$prediction[data$fold %in% usefolds])^2))
        testtable[nrow(testtable) + 1,] =
          list(comp=i,feat=j,r2=fittest$r2,ind=ij,params=length(fittest$coefs)+fixedparct,
               BIC=BICis,AUC=AUCis,AIC=AICis,RMSE=RMSEis,action=paste("add\n" ,paste(j,i,sep="-")))

        switch(metric,
               "AUC" = {compstat<- -testtable$AUC
               currentcompstat<- -AUCis},
               "AIC" = {compstat<- testtable$AIC
               currentcompstat<-AICis},
               "BIC" = {compstat<- testtable$BIC
               currentcompstat<-BICis},
               "RMSE" = {compstat<- testtable$RMSE
               currentcompstat<-RMSEis},
               "R2" = {compstat<- -testtable$r2
               currentcompstat<- -fittest$r2})
        cat(paste(j,i,sep="-"),length(fittest$coefs)+fixedparct,currentcompstat,"\n")

        if(min(compstat)==currentcompstat){
          bestmod<-fittest
          bestfixedpars<-testfixedpars
        }
      }

      if(min(compstat)+forv<currentfitscore){cat("added","\n")
        tracetable<-rbind(tracetable,testtable[which.min(compstat),])
        currentfitscore<-min(compstat)
        currentfeatures<-c(currentfeatures,testtable[which.min(compstat),]$feat)
        currentcomponents<-c(currentcomponents,testtable[which.min(compstat),]$comp)
        paramvec<-materialize_fixedpars(bestfixedpars, bestmod$optimizedpars)
        cat(testtable[which.min(compstat),]$feat,testtable[which.min(compstat),]$comp,"\n")
      }}

    # retain the best model from forward
    # then assume that model was selected and use its parameters as a basis

    #make sure backwards tests until there is no change in feature length for one iteration

    if(length(currentfeatures)>1 & backward)
    {
      cat("\ntrying to remove\n")
      testtable<- as.data.frame(matrix(data=NA,nrow=0,ncol=10))
      x<-c("comp","feat","r2","ind","params","BIC","AUC","AIC","RMSE","action")
      colnames(testtable)<-x

      for(i in 1:length(currentcomponents)){
        testfeatures <- currentfeatures[-i]
        testcomponents <- currentcomponents[-i]
        testpars<-c()


        fixedparct<-0
        pc<-1
        featct<-1
        for(ct in currentfeatures){
          ctparct <- feature_param_count(ct)
          if(ct==currentfeatures[i] & currentcomponents[featct]==currentcomponents[i]){
          }else{
            if(ctparct>0 ){
              fixedparct<-fixedparct+ctparct
              testpars<-c(testpars,paramvec[pc:(pc+ctparct-1)])
            }}
          pc<-pc+ctparct
          featct<-featct+1
        }
        fittest<-LKT(data = data, usefolds=usefolds,interc=interc,maxitv=maxitv,verbose=verbose,
                     components = testcomponents,
                     features = testfeatures,fixedpars = testpars)
        BICis<- (length(fittest$coefs)+fixedparct)*log(length(fittest$prediction))-2*fittest$loglik
        AUCis<- suppressMessages(pROC::auc(data$CF..ansbin., fittest$prediction)[1])
        AICis<- (length(fittest$coefs)+fixedparct)*2-2*fittest$loglik
        RMSEis<- sqrt(mean((data$CF..ansbin.[data$fold %in% usefolds]-fittest$prediction[data$fold %in% usefolds])^2))


        testtable[nrow(testtable) + 1,] =
          list(comp=i,feat=i,r2=fittest$r2,ind=i,params=length(fittest$coefs)+fixedparct,
               BIC=BICis,AUC=AUCis,AIC=AICis,RMSE=RMSEis,action=paste("drop\n" ,paste(currentfeatures[i],currentcomponents[i],sep="-")))

        switch(metric,
               "AUC" = {compstat<- -testtable$AUC
               currentcompstat<- -AUCis},
               "AIC" = {compstat<- testtable$AIC
               currentcompstat<-AICis},
               "BIC" = {compstat<- testtable$BIC
               currentcompstat<-BICis},
               "RMSE" = {compstat<- testtable$RMSE
               currentcompstat<-RMSEis},
               "R2" = {compstat<- -testtable$r2
               currentcompstat<- -fittest$r2})

        cat(paste(currentfeatures[i],currentcomponents[i],sep="-"),length(fittest$coefs)+fixedparct,currentcompstat,"\n")
        if(min(compstat)==currentcompstat){
          bestmod<-fittest
          bestfixedpars<-testpars
        }

      }
      if(min(compstat)-bacv<currentfitscore){cat("removed","\n")
        tracetable<-rbind(tracetable,testtable[which.min(compstat),])
        currentfitscore<-min(compstat)
        cat(currentfeatures[testtable[which.min(compstat),]$feat],currentcomponents[testtable[which.min(compstat),]$comp],"\n")
        currentfeatures<-currentfeatures[-testtable[which.min(compstat),]$feat]
        currentcomponents<-currentcomponents[-testtable[which.min(compstat),]$comp]
        paramvec<-materialize_fixedpars(bestfixedpars, bestmod$optimizedpars)
      }}

    if(length(currentcomponents)>0){
      fixedparct<-0
      for(ct in currentfeatures){
        fixedparct<-fixedparct+feature_param_count(ct)}
      if(length(paramvec) != fixedparct){
        stop("buildLKTModel parameter vector length mismatch: expected ", fixedparct,
             " but found ", length(paramvec), call. = FALSE)
      }
      currentfit<-LKT(data = data, usefolds=usefolds, interc=interc,maxitv=maxitv,verbose=verbose,
                      components = currentcomponents,
                      features = currentfeatures,fixedpars = paramvec)

      BICis<- (length(currentfit$coefs)+fixedparct)*log(length(currentfit$prediction))-2*currentfit$loglik
      AUCis<- suppressMessages(pROC::auc(data$CF..ansbin., currentfit$prediction)[1])
      AICis<- (length(currentfit$coefs)+fixedparct)*2-2*currentfit$loglik
      RMSEis<- sqrt(mean((data$CF..ansbin.[data$fold %in% usefolds]-currentfit$prediction[data$fold %in% usefolds])^2))
      cat("\nStep",k,"results - pars ",length(currentfit$coefs)+fixedparct," current BIC",BICis,"current AIC",AICis,"current AUC",AUCis,
                           "current RMSE",RMSEis," McFadden's R2",currentfit$r2,"\n")

      cat(currentfeatures,"\n",currentcomponents,"\n")
      if(!is.atomic(currentfit$optimizedpars)){
        cat("pars",currentfit$optimizedpars$par,"\n")
        paramvec<-currentfit$optimizedpars$par}
    }}

  # repeat until no more above threshold
  # report final
  tryCatch({
    par(mar=c(16, 5, 1, 1))
    matplot(cbind(-scale(tracetable$r2),scale(tracetable$BIC),-scale(tracetable$AUC),
                  scale(tracetable$AIC),scale(tracetable$RMSE)), type="l", xaxt = "n",
            ylab = "Scaled Score", lwd=2, cex=1.5)

    axis(1, at = 1:nrow(tracetable), labels = paste(tracetable$action), cex.axis = 1, las=2)
    legend("topright", c("R2", "BIC", "AUC", "AIC", "RMSE"), col=1:5, cex=1, lty=1:5, lwd=2)
    mtext(side=1, text="Step action", line=14)
  }, error = function(e) {
    print("Problem creating figure.")
  })


  return(list(tracetable,currentfit))
}




#' Build LASSO input data for LKT feature search
#'
#' @description Constructs the sparse design matrices used by `LASSOLKTModel()` by expanding nonlinear features across a grid of fixed parameter values. This legacy public helper is vignette-facing and returns the `nosolve = TRUE` form of `LKT()`.
#'
#' @param data Data frame or data table containing `Anon.Student.Id`, `CF..ansbin.`, component columns, and optional `fold`.
#' @param gridpars Numeric vector of parameter values used to materialize nonlinear feature candidates.
#' @param allcomponents Character vector of candidate component columns.
#' @param allfeatures Character vector of candidate feature names.
#' @param preset Optional preset model family: `"static"`, `"AFM"`, `"PFA"`, `"advanced"`, `"AFMLLTM"`, `"PFALLTM"`, or `"advancedLLTM"`.
#' @param presetint Logical; include preset intercept terms when `TRUE`.
#' @param specialcomponents Character vector of components paired one-to-one with `specialfeatures`.
#' @param specialfeatures Character vector of special feature names paired with `specialcomponents`.
#' @param specialpars Numeric vector of parameter values for `specialfeatures`.
#' @param removefeat Character vector of feature names to exclude from the crossed candidate list.
#' @param removecomp Character vector of component names paired with `removefeat` to exclude from the crossed candidate list.
#' @return A list from `LKT(nosolve = TRUE)` containing LASSO feature names and sparse design data.
#' @examples
#' \donttest{
#' data(samplelkt)
#' samplelkt$CF..ansbin. <- as.integer(samplelkt$Outcome == "CORRECT")
#' lasso_data <- LASSOLKTData(
#'   data = samplelkt,
#'   gridpars = c(0.25, 0.5),
#'   allcomponents = "KC..Default.",
#'   allfeatures = "lineafm"
#' )
#' names(lasso_data)
#' }
#' @export
LASSOLKTData <- function(data,gridpars,
                          allcomponents,allfeatures,preset=NA,presetint=T,
                          specialcomponents=c(),specialfeatures=c(),specialpars=c(),removefeat=c(), removecomp=c()){
  if(!is.na(preset)){if(preset=="static"){allfeatures<-c("intercept")}


    if(preset=="AFM"){allfeatures<-c("intercept","lineafm","logafm","lineafm")}

    if(preset=="AFMLLTM"){allfeatures<-c("intercept","lineafm","logafm","lineafm","lineafm$","logafm$","lineafm$")}

    if(preset=="PFA"){allfeatures<-c("intercept","lineafm","logafm","lineafm", "logsuc","logfail",  "linesuc","linefail")}

    if(preset=="PFALLTM"){allfeatures<-c("intercept","lineafm","logafm","lineafm", "logsuc","logfail",  "linesuc","linefail"
                                         ,"lineafm$","logafm$","lineafm$", "logsuc$","logfail$",  "linesuc$","linefail$")}

    if(preset=="advanced"){allfeatures<-c("intercept","lineafm","logafm","lineafm", "logsuc","logfail",
                                          "linesuc","linefail", "logitdec","propdec","recency","base")}

    if(preset=="advancedLLTM"){allfeatures<-c("intercept","lineafm","logafm","lineafm", "logsuc","logfail",
                                              "linesuc","linefail", "logitdec","propdec","recency","base","lineafm$","logafm$","lineafm$", "logsuc$","logfail$",
                                              "linesuc$","linefail$")}
    if(presetint==F){allfeatures<-allfeatures[allfeatures!="intercept"]}
  }
  lkt_feature_param_counts(c(allfeatures, specialfeatures))
  if (length(specialcomponents) != length(specialfeatures)) {
    stop("specialcomponents and specialfeatures must have the same length.",
         call. = FALSE)
  }

  cat(white$bgBlack$bold("\nStart making data\n"))

  complist<-c()
  featlist<-c()
  allpars<-c()
  for(i in allcomponents){
    for(j in allfeatures){
      if(lkt_feature_param_counts(j)==0){
        complist<-c(complist,i)
        featlist<-c(featlist,j)} else {
          #if it has parameters, add for each value in grid
          complist<-c(complist,rep(i,length(gridpars)))
          featlist<-c(featlist,rep(j,length(gridpars)))
          allpars<-c(allpars,gridpars)}
    }}

  complist<-c(specialcomponents, complist)
  featlist<-c(specialfeatures, featlist)

  combined_list <- paste(complist, featlist, sep = "_")
  remove_list <- paste(removecomp, removefeat, sep = "_")
  indices_to_keep <- !combined_list %in% remove_list
  complist <- complist[indices_to_keep]
  featlist <- featlist[indices_to_keep]


  allpars<-c(specialpars, allpars)
  # retain the best model data
  return(
    LKT(data = data,   components = complist,
        features = featlist,fixedpars = allpars, nosolve=TRUE)
  )
}



#' Fit a LASSO-selected LKT model
#'
#' @description Builds LKT feature matrices with `LASSOLKTData()` and fits a binomial `glmnet` path. This is a legacy public model-selection helper retained for vignette compatibility.
#'
#' @param data Data frame or data table containing `Anon.Student.Id`, `CF..ansbin.`, component columns, and a `fold` column.
#' @param gridpars Numeric vector of parameter values used to materialize nonlinear feature candidates.
#' @param allcomponents Character vector of candidate component columns.
#' @param preset Optional preset model family: `"static"`, `"AFM"`, `"PFA"`, `"advanced"`, `"AFMLLTM"`, `"PFALLTM"`, or `"advancedLLTM"`.
#' @param presetint Logical; include preset intercept terms when `TRUE`.
#' @param allfeatures Character vector of candidate feature names.
#' @param specialcomponents Character vector of components paired one-to-one with `specialfeatures`.
#' @param specialfeatures Character vector of special feature names paired with `specialcomponents`.
#' @param specialpars Numeric vector of parameter values for `specialfeatures`.
#' @param target_n Desired number of nonzero model features used to choose a lambda from the fitted path.
#' @param removefeat Character vector of feature names to exclude from the crossed candidate list.
#' @param removecomp Character vector of component names paired with `removefeat` to exclude from the crossed candidate list.
#' @param test_fold Fold value used as the held-out test fold.
#' @return A list containing training and test matrices, response vectors, the fitted `glmnet` object, AUC/RMSE/BIC by lambda, selected lambda index, predictions, selected coefficients, and selected feature summary.
#' @examples
#' \dontrun{
#' data(samplelkt)
#' samplelkt$CF..ansbin. <- as.integer(samplelkt$Outcome == "CORRECT")
#' samplelkt$fold <- rep(1:2, length.out = nrow(samplelkt))
#' lasso_model <- LASSOLKTModel(
#'   data = samplelkt,
#'   gridpars = c(0.25, 0.5),
#'   allcomponents = "KC..Default.",
#'   allfeatures = "lineafm",
#'   target_n = 2,
#'   test_fold = 1
#' )
#' names(lasso_model)
#' }
#' @export
LASSOLKTModel <- function(data,gridpars,allcomponents,preset=NA,presetint=T,allfeatures,specialcomponents=c(),
                          specialfeatures=c(),specialpars=c(), target_n,removefeat=c(), removecomp=c(),test_fold = 1){

  datmat = LASSOLKTData(setDT(data),gridpars,
                        allcomponents,allfeatures,preset=preset,presetint=presetint,
                        specialcomponents=specialcomponents,specialfeatures=specialfeatures,
                        specialpars=specialpars,removefeat=removefeat, removecomp=removecomp)

  m1 = as.matrix(datmat$lassodata[[2]])
  colnames(m1) = datmat$lassodata[[1]]

  train_x <- m1
  train_y <- data$CF..ansbin.


  allfold = unique(data$fold)
  all_x = m1
  all_y = data$CF..ansbin.

  train_fold = allfold[which(allfold!=test_fold)]
  train_x = all_x[which(data$fold %in% train_fold),]
  train_y = all_y[which(data$fold %in% train_fold)]
  test_x = all_x[which(data$fold %in% test_fold),]
  test_y = all_y[which(data$fold %in% test_fold)]
  #Test on remaining fold

  start=Sys.time()
  fit=glmnet(x = train_x, y = train_y, family = "binomial")
  end=Sys.time()
  end-start
  print(end-start)

  preds = predict(fit, newx = test_x, s = fit$lambda,type="response")#runs fast


  n_features=rep(NA,length(fit$lambda))
  for(j in 1:length(fit$lambda)){
    coefs=coef(fit, s = fit$lambda[j])
    n_features[j] = length(which(!(coefs==0)))
  }

  auc_lambda <- apply(preds, 2, function(col) {
   roc(test_y, col)$auc
  })

  rmse_lambda <- apply(preds, 2, function(col) {
    sqrt(mean((test_y-col)^2))
  })

  target_idx = which.min(abs(n_features - target_n))
  target_auc = auc_lambda[which.min(abs(n_features - target_n))]
  target_rmse = rmse_lambda[which.min(abs(n_features - target_n))]

  #save(preds,target25,target100,cloze_test_results,file=paste0("cloze_testFold_",testf,".RData"))
  BIC_lambda = rep(NA,length(fit$lambda))

  for(i in 1:length(fit$lambda)){
    tLL <- -deviance(fit)[i]
    k <- fit$df[i]
    n <- nobs(fit)
    BIC_lambda[i] = log(n)*k - tLL
    print(i)
  }

  #Returning features retained in lasso model with target lambda along with coefficients
  target_coefs = coef(fit, s = fit$lambda[target_idx])
  kept_features = rownames(target_coefs)[which(!(target_coefs==0))]
  kept_coefs = target_coefs[which(!(target_coefs==0))]
  model_features = data.frame(kept_features = kept_features,kept_coefs = kept_coefs)

  return_list = list(train_x,train_y,test_x,test_y,fit,target_auc,target_rmse,n_features,auc_lambda,rmse_lambda,BIC_lambda,target_idx,preds,target_coefs,model_features)#,fit)
  names(return_list) = c("train_x","train_y","test_x","test_y","fit","target_auc","target_rmse","n_features","auc_lambda","rmse_lambda","BIC_lambda","target_idx","preds","target_coefs","model_features")


  return(return_list)
}
