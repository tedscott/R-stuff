# excel leading indicators using glmnet with L1 regularization
#
# author: tedscott
# date: 6.20.2016
#
# set filenames as needed and parallel parameters to run logisitic regression
# on Excel feature tables that are already filtered to tenure cohorts on ACE.proc

# packages
library (dplyr)
library (ggplot2)
library (caret)
library(caTools)
library(ROCR)
library(pROC)
library(corrplot)
library(e1071)

#install.packages("doParallel")
# http://michael.hahsler.net/SMU/LearnROnYourOwn/code/doMC.html
library(doParallel)
registerDoParallel(cores=8)
detectCores() # get 24 on hs-spades-01
getDoParWorkers()
#install.packages("glmnet")
library(glmnet)

source("./rfemod.R")

# use CosmosToR package to download from cosmos streams directly to DF
library(CosmosToR)
vc <- vc_connection('https://cosmos14.osdinfra.net/cosmos/ACE.proc/')

# edit filePath for each tenure group feature table
#filePath <- "/users/DIG-shared/ExcelSessionTable/ModelFeatures/ExcelFeatureTable_2016_01_LessThanOneWeek.ss"
filePath <- "/users/DIG-shared/ExcelSessionTable/ModelFeatures/ExcelFeatureTable_2016_01_OneWeekToOneMonth_sample_big.ss"


rawDF <- ss_all(vc, filePath)

# remove all the columns that sum to zero (none of the parent commands or glumps were used)
rawDF <- rawDF[, colSums(rawDF != 0) > 0]

# drop SqmUserId column
rawDF$SqmUserId <- NULL

# if reading results from a Scope sampled stream, remove the Weight column
rawDF$Weight <- NULL

summary(rawDF)
str(rawDF)

# write out csv in case we need it later
write.csv(rawDF, file="OneWeekToOneMonth_500000sample.csv")

# if we read it in we need to remove some columns
rawDF <- read.csv("./LessThanOneWeek.csv")
rawDF$X <- NULL
rawDF$Weight <- NULL # just in case
rawDF$SqmUserId <- NULL

#y = rawDF$Retained90D

#train/test split
set.seed(732)
#trainIndex <- createDataPartition(rawDF$Retained90D, p = .8, list = FALSE, times = 1)
#head(trainIndex)

#rawDFTrain <- rawDF[ trainIndex,]
#rawDFTest  <- rawDF[-trainIndex,]

# X must be a matrix for glmnet
X.sub <- data.matrix(rawDF[,-ncol(rawDF)]) # remove last column
y.sub <- rawDF[,ncol(rawDF)] # response is last column
y.sub <- factor(as.numeric(y.sub), levels=c("0","1"), labels=c("X0","X1"))

# gotta time this part!
start.time <- Sys.time()

# do cv log regression fit, use parallel, AUC as measure of best fit
# use parallel processing, 10-fold CV, and mix lasso vs ridge regression via alpha, 1=pure lasso
cv.fit <-  cv.glmnet(X.sub, y.sub, family="binomial", parallel = T, type.measure = "auc")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# 17 min on hs-spades-01, using 8 cores
# for LessThanOneWeek full data set (902455 rows, 293 vars)

# plot it
plot(cv.fit, xvar="dev", label=TRUE)

# what was min lambda?
cv.fit$lambda.min
cv.fit$lambda.lse
coef(cv.fit, s="lambda.min")

# filter to only those with values
coeffs <- coef(cv.fit, s="lambda.min")
coeffsValues <- coeffs[which(coeffs[,1] != 0),]
coeffsDF <- data.frame(coeffsValues)

predictions <- predict(cv.fit, newx = X.sub, type="class", s="lambda.min")
confusionMatrix(predictions, y.sub)

# for lambda = 0.005 typically get 20 vars + intercept
coeffs0.005 <- coef(cv.fit, s=0.005)
coeffs0.005.values <- coeffs0.005[which(coeffs0.005[,1] != 0),]
coeffs0.005.DF <- data.frame(coeffs0.005.values)
View(coeffs0.005.DF)
rownames(coeffs0.005.DF)
coeffs0.005.vars <- rownames(coeffs0.005.DF)[2:nrow(coeffs0.005.DF)]

# what's the accuracy with s=0.005? which gives fewer vars
predictions <- predict(cv.fit, newx = X.sub, type="class", s=0.005)
confusionMatrix(predictions, y.sub)

# make a heatmap of those vars
dataDF <- rawDF
#col.keep <- c("Retained90D","TotalDistinctUserCmds","TotalDistinctUserGlumps")
x.cor <- cor(dataDF[,(colnames(dataDF) %in% coeffs0.005.vars)])

# corrplot is a nicer output
corrplot(x.cor, method="circle", type="lower", order="hclust", tl.col="black", tl.srt=45)


