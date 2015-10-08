
# Vectors -----------------------------------------------------------------

#Create a data set
#Vectors
X = c(1,2,3,4,5,6) 
y<-c(1,2,3,4,5,6,7)
# numeric vector
a <- c(1,2,5.3,6,-2,4) 
# character vector
b <- c("one","two","three") 
#logical vector
c <- c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE) 
#A data frame is a list of equal-length vectors of possibly different types

# Combining Data ----------------------------------------------------------

firstnames =c("Raj","Joe","Marc")
fullname=paste(firstnames,"Krishnan")
fullname
age=c("56","45","45")
nameage =cbind(fullname,age)
nameage
newentry=c("Fran Krishnan","32")
nameage1=rbind(nameage,newentry)
nameage1
