
# Read and Prepare Data ---------------------------------------------------

# Read in the data
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE); 
str(tweets)
# Create dependent variable
tweets$Negative = as.factor(tweets$Avg <= -1); 
table(tweets$Negative)


# Corpus Prep -------------------------------------------------------------

# Install new packages
install.packages("tm"); 
library(tm);
install.packages("SnowballC"); 
library(SnowballC)

# Create corpus
corpus = Corpus(VectorSource(tweets$Tweet)); corpus; corpus[[1]]

# Text Operations
corpus = tm_map(corpus, tolower); corpus = tm_map(corpus, PlainTextDocument);
corpus[[1]] # use this at each stage to see what happens
corpus = tm_map(corpus, removePunctuation);stopwords("english")[1:10]
corpus[[1]] 
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]] 
corpus = tm_map(corpus, stemDocument)
corpus[[1]]  


# Create Document Term Matrix ---------------------------------------------

# Create matrix
frequencies = DocumentTermMatrix(corpus); frequencies
# Look at matrix 
inspect(frequencies[1000:1005,505:515])

# Check for sparsity
findFreqTerms(frequencies, lowfreq=20);
sparse = removeSparseTerms(frequencies, 0.995);
sparse


# Create data frame -------------------------------------------------------


# Convert to a data frame
tweetsSparse = as.data.frame(as.matrix(sparse))
# Make all variable names R-friendly
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Add dependent variable
tweetsSparse$Negative = tweets$Negative


# Train Test Split --------------------------------------------------------


# Split the data
library(caTools)
set.seed(123); 
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split==TRUE); 
testSparse = subset(tweetsSparse, split==FALSE)


# CART Model --------------------------------------------------------------

# Build a CART model
library(rpart);
library(rpart.plot)
tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)


# Evaluate Performance ----------------------------------------------------

# Evaluate the performance of the model
predictCART = predict(tweetCART, newdata=testSparse, type="class")
table(testSparse$Negative, predictCART)
# Compute accuracy   
(294+18)/(294+6+37+18)
# Baseline accuracy 
table(testSparse$Negative)   
300/(300+55)


# Random Forest -----------------------------------------------------------


# Random forest model
library(randomForest);set.seed(123); tweetRF = randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)
table(testSparse$Negative, predictRF)
# Accuracy:
(293+21)/(293+7+34+21)




                