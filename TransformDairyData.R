# set a flag to define the environment
Azure = FALSE

if(Azure) {
  frame1<-maml.mapInputPort(1)
} else {
  # if in R studio read in the csv
  dirName <- "C:/GIT/EdX-DataSci/DAT203_Lab02B/"
  fileName <- "cadairydata.csv"
  infile<-file.path(dirName,fileName)
  frame1<-read.csv(infile,header=TRUE,stringsAsFactors = FALSE)
}

## select a subset of columns
library(dplyr)
frame1<-select(frame1, Year, Month, Cottagecheese.Prod, Icecream.Prod, Milk.Prod)

# chain verbs to show totals for August
frame1<-frame1 %>%
  filter(Month=='Aug') %>%
  mutate(Total.Prod = Cottagecheese.Prod + Icecream.Prod + Milk.Prod)

# plot something
plot(frame1$Year,frame1$Cottagecheese.Prod)

# if in Azure output the data frame
if(Azure) maml.mapoutputPort('frame1')