####
# tedscott
# 8.29.2016
#
# script to remove features from a sparse dataset to only those that occur for at
# least 0.1% (default) of the users (
#
#
###

# prune function
col.prune <- function(inDF, thresh=0.001) {
  # ratio of count of non-zero entries to total number of rows
  # must be >= the threshold (default = 0.1%)
  tempDF <- inDF[colSums(inDF!=0)/nrow(inDF) >= thresh]
  return(tempDF)
}







