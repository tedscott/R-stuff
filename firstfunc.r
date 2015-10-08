myfunc <- function() {
  x<-rnorm(100)
  mean(x)
}

secondfunc <- function(x) {
  x+rnorm(length(x))
}