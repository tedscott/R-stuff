# TedScott
# 6.24.2016
# script to perform univariate tests of features + base model for each cohort group
# the model cofficients are output in CSVs


# load libraries
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

setwd("//officefile/public/tedscott/data")

# load datasets
LessThanOneWeekDF <- read.csv("./LessThanOneWeek.csv")
LessThanOneWeekDF$X <- NULL
LessThanOneWeekDF$Weight <- NULL # just in case
LessThanOneWeekDF$SqmUserId <- NULL

OneWeekToOneMonthDF <- read.csv("./OneWeekToOneMonth_500000sample.csv")
OneWeekToOneMonthDF$X <- NULL
OneWeekToOneMonthDF$Weight <- NULL # just in case
OneWeekToOneMonthDF$SqmUserId <- NULL

OneMonthToSixMonthsDF <- read.csv("./OneMonthToSixMonths.csv")
OneMonthToSixMonthsDF$X <- NULL
OneMonthToSixMonthsDF$Weight <- NULL # just in case
OneMonthToSixMonthsDF$SqmUserId <- NULL

MoreThanSixMonthsDF <- read.csv("./MoreThanSixMonths.csv")
MoreThanSixMonthsDF$X <- NULL
MoreThanSixMonthsDF$Weight <- NULL # just in case
MoreThanSixMonthsDF$SqmUserId <- NULL

##### RFE ########
# Time for modeling via glm
# 0-7days RFE basevars

# set DF to investigate
rawDF <- LessThanOneWeekDF

baseVars <- c("GlumpUsageRatio","NumActiveDays","TotalDistinctUserCmds","g_Tables",
              "g_Save","p_Ribbon","g_OutSpace","g_Insert","g_Open","p_OutSpace","p_CtxUIRow",
              "g_BordersFormatting")

# create formula
baseFormula <- as.formula(paste("Retained90D ~",paste(baseVars, collapse= "+")))

# run log reg model
baseModel <- glm(baseFormula , data=rawDF, family=binomial(link="logit"))

# check model parameters
summary(baseModel)

# RFE vars to consider for univariate tests
testVars <- c("g_ColorPicker","g_DocRecovery","g_Help","g_Identity","g_Options","g_Shapes","g_PivotTables",
              "g_Share","p_StatusBar","g_Exit","g_Hyperlink","p_PointerModeOptions")

uniFormula <- as.formula(paste("Retained90D ~",paste(c(baseVars,testVars[1]), collapse= "+")))
uniModel <- glm(uniFormula, data=rawDF, family=binomial(link="logit"))
#summary(uniModel)
# run all the models
# initialize output with results from first test
# first column is feature, second is log odds, last column is % change in retained for unit change in feature
foo <- c(testVars[1], 
        coef(uniModel)[testVars[1]][[1]], 
        100*(exp(coef(uniModel)[testVars[1]][[1]])-1))

# gotta time this part!
start.time <- Sys.time()

# loop through all the features to test and build up result list
for (feature in testVars[2:length(testVars)]) {
  uniFormula <- as.formula(paste("Retained90D ~",paste(c(baseVars,feature), collapse= "+")))
  uniModel <- glm(uniFormula, data=rawDF, family=binomial(link="logit"))
  results <- c(feature, 
               coef(uniModel)[feature][[1]], 
               100*(exp(coef(uniModel)[feature][[1]])-1))
  foo <- c(foo, results)
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken # about 2 min on hs-spades-01 for 12 variables, 900000 rows!

# make it a matrix for readability
results <- matrix(data=foo, nrow=length(testVars), ncol=3, byrow=TRUE)
resultsDF <- as.data.frame(results)
names(resultsDF) <- c("feature","coeff","pct_change")

# output it to a file
write.csv(resultsDF, "./LessThanOneWeek-RFE-UnivariateResults.csv")

###### glmnet results ######
# Time for modeling via glm
# 0-7days glmnet basevars
baseVars <- c("GlumpUsageRatio","NumActiveDays","TotalDistinctUserGlumps","g_Tables",
              "p_TabInsert","g_Insert","p_Ribbon","g_Windows","p_TabHome","g_Formulas","g_Filter","g_Commenting",
              "g_CellFormatting","p_Nil")

# create formula
baseFormula <- as.formula(paste("Retained90D ~",paste(baseVars, collapse= "+")))

# run log reg model
baseModel <- glm(baseFormula , data=rawDF, family=binomial(link="logit"))

# check model parameters
summary(baseModel)

# RFE vars to consider for univariate tests
testVars <- c("p_CtxUIFormulaBar","g_ColorPicker","g_ConditionalFormatting","g_DataValidation","g_DocRecovery",
              "g_Help","g_Identity","g_Options","g_SmartArt")

uniFormula <- as.formula(paste("Retained90D ~",paste(c(baseVars,testVars[1]), collapse= "+")))
uniModel <- glm(uniFormula, data=rawDF, family=binomial(link="logit"))
#summary(uniModel)
# run all the models
# initialize output with results from first test
# first column is feature, second is log odds, last column is % change in retained for unit change in feature
foo <- c(testVars[1], 
         coef(uniModel)[testVars[1]][[1]], 
         100*(exp(coef(uniModel)[testVars[1]][[1]])-1))

# gotta time this part!
start.time <- Sys.time()

# loop through all the features to test and build up result list
for (feature in testVars[2:length(testVars)]) {
  uniFormula <- as.formula(paste("Retained90D ~",paste(c(baseVars,feature), collapse= "+")))
  uniModel <- glm(uniFormula, data=rawDF, family=binomial(link="logit"))
  results <- c(feature, 
               coef(uniModel)[feature][[1]], 
               100*(exp(coef(uniModel)[feature][[1]])-1))
  foo <- c(foo, results)
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken # about 2 min on hs-spades-01 for 12 variables, 900000 rows!

# make it a matrix for readability
results <- matrix(data=foo, nrow=length(testVars), ncol=3, byrow=TRUE)
resultsDF <- as.data.frame(results)
names(resultsDF) <- c("feature","coeff","pct_change")

# output it to a file
write.csv(resultsDF, "./LessThanOneWeek-glmnet-UnivariateResults.csv")

######
# OneWeekToOneMonth
#####

# set DF to investigate
rawDF <- OneWeekToOneMonthDF

##### RFE ########
# Time for modeling via glm
# 8-30days RFE basevars
baseVars <- c("TotalDistinctUserGlumps","NumActiveDays","TotalDistinctUserCmds","g_Insert",
              "p_Ribbon","g_OutSpace","g_Tables","g_Formulas","g_CopyPaste")

# create formula
baseFormula <- as.formula(paste("Retained90D ~",paste(baseVars, collapse= "+")))

# run log reg model
baseModel <- glm(baseFormula , data=rawDF, family=binomial(link="logit"))

# check model parameters
summary(baseModel)

# RFE vars to consider for univariate tests
testVars <- c("g_DocRecovery","g_Options","g_Identity","g_Help","p_CtxUIFormulaBar","g_Print",
              "g_SmartArt","g_PivotFieldList","g_ColorPicker","p_FloatieXLEditText","g_Windows",
              "p_CtxUIChartDataSeries","g_Clear","g_DataTab","g_Filter","g_ConditionalFormatting")

uniFormula <- as.formula(paste("Retained90D ~",paste(c(baseVars,testVars[1]), collapse= "+")))
uniModel <- glm(uniFormula, data=rawDF, family=binomial(link="logit"))
#summary(uniModel)
# run all the models
# initialize output with results from first test
# first column is feature, second is log odds, last column is % change in retained for unit change in feature
foo <- c(testVars[1], 
         coef(uniModel)[testVars[1]][[1]], 
         100*(exp(coef(uniModel)[testVars[1]][[1]])-1))

# gotta time this part!
start.time <- Sys.time()

# loop through all the features to test and build up result list
for (feature in testVars[2:length(testVars)]) {
  uniFormula <- as.formula(paste("Retained90D ~",paste(c(baseVars,feature), collapse= "+")))
  uniModel <- glm(uniFormula, data=rawDF, family=binomial(link="logit"))
  results <- c(feature, 
               coef(uniModel)[feature][[1]], 
               100*(exp(coef(uniModel)[feature][[1]])-1))
  foo <- c(foo, results)
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken # about 2 min on hs-spades-01 for 12 variables, 900000 rows!

# make it a matrix for readability
results <- matrix(data=foo, nrow=length(testVars), ncol=3, byrow=TRUE)
resultsDF <- as.data.frame(results)
names(resultsDF) <- c("Feature","Coeff","Pct_change")

# output it to a file
write.csv(resultsDF, "./OneWeekToOneMonth-RFE-UnivariateResults.csv")


######
# OneMonthToSixMonths
#####

# set DF to investigate
rawDF <- OneMonthToSixMonthsDF

##### RFE ########
# Time for modeling via glm
# 30-180days RFE basevars
baseVars <- c("g_Save","TotalDistinctUserGlumps","NumActiveDays","TotalDistinctUserCmds",#"p_OutSpace",
              "p_CtxUIRow","g_Filter","p_Nil","g_ColorPicker", "g_OutSpace",
              "g_Tables","p_CtxUIColumn","g_CopyPaste","p_GalPasteLite")

# create formula
baseFormula <- as.formula(paste("Retained90D ~",paste(baseVars, collapse= "+")))

# run log reg model
baseModel <- glm(baseFormula , data=rawDF, family=binomial(link="logit"))

# check model parameters
summary(baseModel)

# RFE vars to consider for univariate tests
testVars <- c("g_Open","g_Help","g_Close","g_ReadOnly",
              "p_GalPaste","g_Zoom","p_StatusBar","p_Custom","g_PivotTables",
              "g_PrintPreview","g_NumberFormatting", "g_Proofing")

uniFormula <- as.formula(paste("Retained90D ~",paste(c(baseVars,testVars[1]), collapse= "+")))
uniModel <- glm(uniFormula, data=rawDF, family=binomial(link="logit"))
#summary(uniModel)
# run all the models
# initialize output with results from first test
# first column is feature, second is log odds, last column is % change in retained for unit change in feature
foo <- c(testVars[1], 
         coef(uniModel)[testVars[1]][[1]], 
         100*(exp(coef(uniModel)[testVars[1]][[1]])-1))

# gotta time this part!
start.time <- Sys.time()

# loop through all the features to test and build up result list
for (feature in testVars[2:length(testVars)]) {
  uniFormula <- as.formula(paste("Retained90D ~",paste(c(baseVars,feature), collapse= "+")))
  uniModel <- glm(uniFormula, data=rawDF, family=binomial(link="logit"))
  results <- c(feature, 
               coef(uniModel)[feature][[1]], 
               100*(exp(coef(uniModel)[feature][[1]])-1))
  foo <- c(foo, results)
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken # about 2 min on hs-spades-01 for 12 variables, 900000 rows!

# make it a matrix for readability
results <- matrix(data=foo, nrow=length(testVars), ncol=3, byrow=TRUE)
resultsDF <- as.data.frame(results)
names(resultsDF) <- c("Feature","Coeff","Pct_change")

# output it to a file
write.csv(resultsDF, "./OneMonthToSixMonths-RFE-UnivariateResults.csv")



##### RFE ########
# Time for modeling via glm
# 180+days RFE basevars

# set DF to investigate
rawDF <- MoreThanSixMonthsDF

baseVars <- c(#"g_Save","p_OutSpace",
              "NumActiveDays"#,"TotalDistinctUserCmds"#,
              #"g_Font",
              #"p_CtxUIRow","g_Filter","p_Nil","g_ColorPicker", "g_OutSpace","g_CellFormatting",
              #"g_Tables","p_Ribbon","g_CopyPaste","g_Tables", "g_Home","g_GridFormatting"
              )

# create formula
baseFormula <- as.formula(paste("Retained90D ~",paste(baseVars, collapse= "+")))

# run log reg model
baseModel <- glm(baseFormula , data=rawDF, family=binomial(link="logit"))

# check model parameters
summary(baseModel)

# RFE vars to consider for univariate tests
testVars <- c(#"g_FormulaActions","p_TabTableTools",
              "g_Ink",
              #"g_Selection",
              #"g_InsertObject","g_Delete",
              "g_Clear","g_FindReplace","p_CtxUIListRange",
              "g_QuickCode","p_CtxUIConnector", "g_Inspector","p_CustomizeMenu","p_CtxUIChartChartTitle",
              "p_TabIgxCreateGraphic")

uniFormula <- as.formula(paste("Retained90D ~",paste(c(baseVars,testVars[1]), collapse= "+")))
uniModel <- glm(uniFormula, data=rawDF, family=binomial(link="logit"))
#summary(uniModel)
# run all the models
# initialize output with results from first test
# first column is feature, second is log odds, last column is % change in retained for unit change in feature
foo <- c(testVars[1], 
         coef(uniModel)[testVars[1]][[1]], 
         100*(exp(coef(uniModel)[testVars[1]][[1]])-1))

# gotta time this part!
start.time <- Sys.time()

# loop through all the features to test and build up result list
for (feature in testVars[2:length(testVars)]) {
  uniFormula <- as.formula(paste("Retained90D ~",paste(c(baseVars,feature), collapse= "+")))
  uniModel <- glm(uniFormula, data=rawDF, family=binomial(link="logit"))
  results <- c(feature, 
               coef(uniModel)[feature][[1]], 
               100*(exp(coef(uniModel)[feature][[1]])-1))
  foo <- c(foo, results)
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken # about 2 min on hs-spades-01 for 12 variables, 900000 rows!

# make it a matrix for readability
results <- matrix(data=foo, nrow=length(testVars), ncol=3, byrow=TRUE)
resultsDF <- as.data.frame(results)
names(resultsDF) <- c("Feature","Coeff","Pct_change")

# output it to a file
write.csv(resultsDF, "./MoreThanSixMonths-RFE-UnivariateResults.csv")

