DataFrameToClickstreams <- function(data, order=2, DoTrim=TRUE, RemoveRepeats=FALSE, control.cutoff=17, prb=0.05) {
  vec_len <<- c()
  myfun2 <- function(x, o=order, ...) {
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
      len <- len +1
      if (len >= control.cutoff) trim <- len else trim <- rbinom(1,1,prb) * len
    } else {
      trim <- ifelse(DoTrim, min(c(ixOz, len)), len)
    }
    if (trim <= o) return(NA) 
    vec_len <<- append(vec_len, trim)
    return(paste(c(x[[1]], cmdStreamRaw[1:trim]), sep=",", collapse=","))}
  cs <- apply(data, 1, myfun2)
  cs <- cs[!is.na(cs)]
  csf <- tempfile()
  on.exit(unlink(csf), add=T)
  writeLines(cs, csf)
  cls <- readClickstreams(csf, header = TRUE)
  return(cls)
}