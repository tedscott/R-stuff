who =read.csv('WHO.csv')
str(who)
summary(who)

#Which country has the smallest percentage of the population over 60?
which.min(who$Over60)
who$Country[183]
who[183,'Country']
who[183,1]

#Which region has the lowest average child mortality rate across all countries in that region?
sort(tapply(who$ChildMortality,who$Region,mean))

plot(who$GNI,who$FertilityRate)
     