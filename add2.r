add2<-function(x,y){
  x+y
}

above_n<-function(x,n=10)
{
  use<-x>n # returns logical vector
  x[use] # uses logical vector and displays those that are TRUE
}