# ======================== Script  Information =================================
# PURPOSE: Exploratory plots and ratio analysis
#
#
#
# NOTES:
#   1)
#
# PROJECT INFORMATION:
#   Name  : 901240_TellMeTrigger
#
# HISTORY:----
#   Date		        Remarks
#	-----------	   ---------------------------------------------------------------
#	2017-01-17      Created.   <mialdea>
#
# ==============================  Environment Setup  ===========================
palette(c("black", "#FDB100", "#0078FD",  "#800080","yellowgreen", "#CC5500", "#400080", "#008000", "#663000", "#FF51FF", "#386CB0", "#BDF9FF", "#FFCA99"))
OfficeColors <- c("#00B0F6", "#00BF7D", "#F8766D", "#a6cee3", "#E76BF3") #lighter
#OfficeColors <- c("#2B579A", "#217346", "#B7472A", "#0173C7", "#7D3778") #official

names(OfficeColors) <- c("Word", "Excel", "PowerPoint", "Outlook", "OWA")

options(stringsAsFactors=FALSE, cores=3)

require(plyr)
require(clickstream)

require(CosmosToR)
#===============================  Cosmos VC Connection  ========================
vc <- vc_connection('https://cosmos14.osdinfra.net/cosmos/ACE.proc/', TRUE)
# ==============================  Bar Plot  ====================================
d <- ss_all(vc, '/users/DIG-shared/TellMeTriggers/TellMeCohortCmdSequences10000.ss')
d2 <- ss_all(vc, '/users/DIG-shared/TellMeTriggers/NonTellMeTCIDNoQueryCmdSequences10000.ss')
d <- rbind(d, d2)
rm(d2); gc()


# quick & dirty function to convert data frame to clickstream S3 data object
# Also filters out command sequences that start with "TellMe" & cuts off sequence at first instance of TellMe
# Note: it's slow!
# order specifies the minimum clickstream length to keep; default=2
DataFrameToClickstreams <- function(data, order=2, DoTrim=TRUE, RemoveRepeats=FALSE, control.cutoff=17, prb=0.05) {
  vec_len <<- c()
  cs <- apply(data, 1, function(x, o=order, ...) {
    cmdStreamRaw <- strsplit(x[[2]], split=",", fixed=T)[[1]]
    if (RemoveRepeats) {
      #cmdStreamRaw <- unique(cmdStreamRaw)
      while(any((cmdStreamRaw==data.table::shift(cmdStreamRaw, 1))[-1])){
        cmdStreamRaw <- cmdStreamRaw[c(TRUE, (cmdStreamRaw!=data.table::shift(cmdStreamRaw, 1))[-1])]
      }
    }
    ixOz <-  grep("TellMe", cmdStreamRaw)
    len <- length(cmdStreamRaw)
    if (length(ixOz)==0) {
      cmdStreamRaw <- c(cmdStreamRaw, "SomethingElse")
      len <- len+1
      if (len >= control.cutoff) trim <- len else trim <- rbinom(1, 1, prb) * len
    } else {
      trim <- ifelse(DoTrim, min(c(ixOz, len)), len)
    }
    if (trim <= o) return(NA) 
    vec_len <<- append(vec_len, trim)
    return(paste(c(x[[1]], cmdStreamRaw[1:trim]), sep=",", collapse=","))})
  cs <- cs[!is.na(cs)]
  csf <- tempfile()
  on.exit(unlink(csf), add=T)
  writeLines(cs, csf)
  cls <- readClickstreams(csf, header = TRUE)
  return(cls)
}
#convert data to clickstream object
dcls <- DataFrameToClickstreams(d, DoTrim = T, RemoveRepeats=T, prb=0.5)
quantile(vec_len)
summary(dcls)

###
#Optimization
maxOrder <- 11
result <- data.frame()
for (k in 2:maxOrder) {
  mc <- fitMarkovChain(clickstreamList = dcls, order = k, control=list(optimizer="quadratic", use.lpSolve="linprog"))
  result <- rbind(result, c(k, summary(mc)$aic, summary(mc)$bic))
}
names(result) <- c("Order", "AIC", "BIC")
result

###
#Final model

mc <- fitMarkovChain(clickstreamList = dcls, order = 1, control=list(optimizer="quadratic", use.lpSolve="linprog"))
summary(mc)
#plot(mc)

absorbingStates(mc)
predict(mc, new("Pattern", sequence = c("Copy"), absorbingProbabilities=data.frame(SomethingElse=0.9, TellMe=0.1)), dist=1)


##
#Clustering
clusters <- clusterClickstreams(clickstreamList = dcls, order = 1, centers = 5)
summary(clusters$clusters[[1]])
plot(clusters)

mc1 <- fitMarkovChain(clickstreamList = clusters$clusters[[1]], order = 2, control=list(optimizer="quadratic", use.lpSolve="linprog"))
predict(mc1, new("Pattern", sequence = c("TabInsert", "TabHome", "TellMe")))
