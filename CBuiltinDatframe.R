

# CBuiltInDataset ---------------------------------------------------------


#Built in data frame
mtcars
#How many rows are there in the dataset?
mtcars$mpg
#What is the difference between the following commands?
mtcars$mpg[8]
mtcars[8,]
mtcars["Merc 240D",]

#What does the following command do? Explain the output
mtcars[which.max(mtcars$mpg),]

#Plotting data
attach(mtcars)
plot(wt, mpg, main="Scatterplot Example", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
