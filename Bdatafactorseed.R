
# Data Frame --------------------------------------------------------------

#Data Frame
d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata <- data.frame(d,e,f)
# variable names 
names(mydata) <- c("ID","Color","Passed")     
mydata$Color
#Categorical variable / Factor variables
#Factor variables are categorical variables that can be either numeric / string variables; 
#Levels - sorted list of all the distinct values of the data vector
#Categorical variables to factor variables


# Data factor seed --------------------------------------------------------


#Whenever you use random number generation, using the same seed will yield 
set.seed(124) 
#the same results (e.g. Separating training and Testing data set; generating sample data)
schtyp <- sample(0:1, 20, replace = TRUE) 
schtyp
schtyp.f <- factor(schtyp, labels = c("private", "public")) 
schtyp.f 
is.factor(schtyp.f)

