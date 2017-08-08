#
# sample processing of a CSV file of student data
# with some plotting and basic stats analysis
#
# ted scott (scott.ted.j@gmail.com)
# 
# 12.12.2016
#

# run these if they have never been installed
# dplyr is a package for cleaning and wrangling the data
# ggplot2 is a package for plotting
install.packages("dplyr")
install.packages("ggplot2")

# load the packages
library(dplyr)
library(ggplot2)

# recommended cheat sheets for syntax and functions in these packages
# https://www.rstudio.com/resources/cheatsheets/

# in addition to stackoverflow, I find http://www.cookbook-r.com/ to be handy for using R, especially for plotting


# read in a csv with data in it
# adjust path to correct location
records <- read.csv("E:/R-stuff/fake-students.csv")

# convert the dataframe to a tbl_df which gives nicer outputs
recordsTbl <- tbl_df(records)

# summarize the data
summary(recordsTbl)

# another summary that gives info about data types
str(recordsTbl)

# see the full contents
recordsTbl

# view the data in a dataframe tab (table of data) in spreadsheet format
View(recordsTbl)

# update the values in a column
# want to replace F, S, W in trimester column to be Fall, Winter, and Spring
# uses the dplyr package's 'pipe' syntax which sends the DF to the function (mutate)
# here I am not rewriting the old dataframe but making a new one
# Note: could also use replace() function
newRecords <- recordsTbl %>% 
  mutate(trimester=ifelse(trimester=="F", "Fall", ifelse(trimester=="W", "Winter", ifelse(trimester=="S", "Spring", "Unknown"))))

# view the data in a dataframe tab (table of data)
View(newRecords)

# let's get the simple average grade per teacher
# yes, the function name is "summarise"
newRecords %>% group_by(teacher) %>% 
  summarise(avgGrade = mean(score))

# should probably get the means per class, per teacher
# since it might differ by class
newRecords %>% group_by(teacher, class) %>% 
  summarise(avgGrade = mean(score))



################
#
# plot some data
#
################


# histograms of scores by class
histScores <- ggplot(newRecords, aes(x=score, fill=class)) +
  geom_histogram(binwidth=1, alpha=0.5, position="identity") 
histScores

# histograms of scores by teacher
# add density plots to see if distribution is unimodal, bimodal, etc
histTeacher <- ggplot(newRecords, aes(x=score, fill=teacher)) +
  geom_histogram(binwidth=1, alpha=0.5, position="identity") +
  geom_density(alpha=0.2)
histTeacher

# not really enough data, but both teachers appear to have bimodal distributions in scores
# across classes


# create some groupings so we can plot means later
# this groups by teacher and class and gets the mean score for each group
cdat <- newRecords %>% group_by(teacher, class) %>% summarise(meanScore=mean(score))

# facet the plots to see each separately
# the ".~teacher" means plot them by teacher
histTeacher + facet_grid(.~teacher)

# facet by class too so we can see each class and teacher distribution of scores in isolation
# also show the mean of scores, which uses the cdat groupings we made above
histTeacher + 
  geom_vline(data=cdat, aes(xintercept=meanScore, color=teacher),linetype="dashed", size=1) +
  facet_grid(class~teacher) 
  

# boxplots are nice too for seeing the distribution and median
# which is often a better measure than the mean for non-normal distibutions
# facet it by teacher 
ggplot(newRecords, aes(x=class, y=score, color=class)) +
  geom_boxplot(alpha=0.5) +
  facet_grid(.~teacher)

# boxplots of grades by teacher, class and trimester
ggplot(newRecords, aes(x=class, y=score, color=class)) +
  geom_boxplot(alpha=0.5) +
  facet_grid(trimester~teacher)


########################
#
# some quick stats stuff
#
########################

# based on the plots, it appears that there is a difference in the scores given out by each
# teacher for each class

# let's test whether or not the apparent difference in scores between the teachers is statistically
# significant such that we might want to go discuss with the teachers/students to find out if
# there might be something going on to explain the difference


# check for equal variances in the scores by teacher to see if we can trust the p-value of a t-test
var.test(newRecords$score[newRecords$teacher=="mein"], newRecords$score[newRecords$teacher=="scott"])

# p-value is greater than 0.05 (5% significance is common)
# which means the variances are not statisticaly different and hence are homogeneous enough for a t-test


# we could assign the scores to variables to save typing like so
# mein <- newRecords$score[newRecords$teacher=="mein"]
# scott <- newRecords$score[newRecords$teacher=="scott"]
# but I won't to be explicit and make each line clearer

# since variance is similar between the two sets of scores, let's check...
# is there a statistically significant difference in student scores between the teachers?
t.test(newRecords$score[newRecords$teacher=="mein"], newRecords$score[newRecords$teacher=="scott"], var.equal=TRUE)

# p-value is 0.066, which is not significant at the p = 0.05 level but pretty close
# so there is a nearly significant difference in the scores of the students between the
# two teachers

# you could argue that the two sets of students are NOT independent given that they all attend EPS
# and have other classes in common, so you could add "paired=TRUE" to the t-test
t.test(newRecords$score[newRecords$teacher=="mein"], newRecords$score[newRecords$teacher=="scott"], var.equal=TRUE, paired=TRUE)

# now you get a p-value of 0.054, so there may be something to say here about the scores being different between the two teachers
# you could do a one sided t-test where the null hypothesis is that the mean score is nearly the same 
# and the alternative hypothesis is that the mean scores for teacher "mein" are statistically lower
# but in this toy data set the answer will be that we reject the null in favor of the alternate hypothesis :)

# there's so much more that could be done with this simple dataset, but hopefully this gives you an idea
# of some of the ease and power of R for analysis
