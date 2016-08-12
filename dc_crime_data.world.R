# read in the data for 2016 Washington DC crime from data.world
fileurl <- "https://data.world/opportunity/washington-dc-crime-incidents/file/Crime_Incidents__2016.csv"

# sadly, can't pull it directly due to some end of file error
# but there must be a way to pull directly from data.world, right?
#DCCrime <- read.csv(fileurl)

DCCrime <- read.csv("C:/Users/tedscott/Downloads/Crime_Incidents__2016.csv")

# look at the data in a table by district
table(DCCrime$OFFENSE, DCCrime$DISTRICT)

# looks like the sixth and seventh district are the most violent
# plot to see
library(ggplot2)
library(dplyr)

grouped <- DCCrime %>% select(OFFENSE, DISTRICT) %>% group_by(DISTRICT)
grouped %>% count(OFFENSE) %>% ggplot(aes(x=DISTRICT, y=n), fill=OFFENSE) + 
  geom_point() + 
  facet_wrap(~OFFENSE) + 
  theme(axis.text.x=element_text(angle=90))

# should probably play with the y-axis range so homicides, etc show up better
# or just filter to violent crimes?
violent <- c("HOMICIDE","ASSAULT W/DANGEROUS WEAPON","SEX ABUSE")
grouped_filtered <- grouped %>% filter(OFFENSE %in% violent)

# now plot it
grouped_filtered %>% count(OFFENSE) %>% ggplot(aes(x=DISTRICT, y=n), fill=OFFENSE) + 
  geom_point() + 
  facet_wrap(~OFFENSE) + 
  theme(axis.text.x=element_text(angle=90))

# now, can I map it out somehow?
# TODO


