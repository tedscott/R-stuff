#Load Crime Data mvtWeek1.csv to mvt
mvt=read.csv("mvtWeek1.csv")
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$Date = DateConvert
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)

#In which month did the fewest motor vehicle thefts occur?
sort(table(mvt$Month))

#hist(mvt$Date, breaks=100)
boxplot(mvt$Date ~ mvt$Arrest)


#What is the proportion of arrests made in year 2001
table(mvt$Arrest, mvt$Year=="2001")
