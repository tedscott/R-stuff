#
# Simplified recursive feature elimination for GLM (caret one does not work)
# author: toreli
#
#
rfe.tmp <- function(X.df, method="glm", plot=T) {
  X.sub <- data.matrix(X.df[,-ncol(X.df)]) # remove last column
  y.sub <- X.df[,ncol(X.df)] # response is last column
  y.sub <- factor(as.numeric(y.sub), levels=c("0","1"), labels=c("X0","X1"))
  
  rtn <- as.data.frame(matrix(NA, ncol=9, nrow=ncol(X.sub)))
  while(length(X.sub)>0) {
    print(paste0("NCOL: ", ncol(X.sub)))
    print(class(X.sub))
    if(class(X.sub)!="matrix")
      X.sub <- t(t(X.sub))
    print(dim(X.sub))
    if (method=="glm") {
      train.ctrl = trainControl(method="cv", number=5, 
                                classProbs=T, summaryFunction=twoClassSummary, 
                                savePredictions=T, verboseIter=T, allowParallel=T)
      fit <- train(X.sub, 
                   y.sub, method="glm", family=binomial(), preProc = c("center","scale"),
                   metric="ROC", trControl=train.ctrl)
    } else if (method=="rf") {
      
      train.ctrl = trainControl(method="cv", number=10, 
                                classProbs=T, summaryFunction=twoClassSummary, 
                                savePredictions=T, verboseIter=T, allowParallel=T)
      rf.grid <-  expand.grid(mtry=c(2))
      if (ncol(X.sub)>1) {
        fit <- train(X.sub, y.sub, 
                     method="rf", metric="ROC", 
                     importance=T, verbose=T, 
                     trControl=train.ctrl, tuneGrid=rf.grid)
      } else {
        fit <- train(IsRetained~TenureDays, X.df,
                     method="rf", metric="ROC", 
                     importance=T, verbose=T, 
                     trControl=train.ctrl, tuneGrid=rf.grid)
        
      }
    } else {
      stop("Invalid method")
    }
    print(fit)
    rtn[ncol(X.sub),1:7] <- fit$results
    names(rtn) <- c(names(fit$results), "feature", "nparam")
    rtn[ncol(X.sub),"nparam"] <- ncol(X.sub)
    
    vimp <- varImp(fit, scale=F)
    vimp.df <- vimp[["importance"]]
    vimp <- as.vector(vimp.df[,1]); 
    names(vimp) <- row.names(vimp.df)
    vimp <- sort(vimp)
    print(vimp)
    print(paste0("length(vimp) = ", length(vimp)))
    
    if (ncol(X.sub)>2) {
      rtn[length(vimp),"feature"] <- names(vimp)[1]
    } else if (ncol(X.sub)==2) {
      rtn[1:2,"feature"] <-  names(vimp)[1:2]
    }
    X.sub <- X.sub[,colnames(X.sub) %in% names(vimp)[-1]]
    
  }
  if (plot) {
    pdf(paste0("fit-rfe-", method, ".pdf"), width=5, height=5)
    p <- ggplot(rtn, aes(x=nparam, y=ROC,ymin=ROC-ROCSD, ymax=ROC+ROCSD, label=feature)) 
    p <- p + geom_point() + geom_errorbar(width=0.5)
    p <- p + geom_text(angle=90, hjust = 0, aes(y=0.55)) 
    p <- p + xlab("Features (#)") + ylab("ROC") #+ scale_y_continuous(limits=c(0.55,0.72))# ylim(0.55, 0.71)
    print(p)
    dev.off()
  }
  
  #rtn$params <- as.integer(row.names(rtn))
  rtn
}