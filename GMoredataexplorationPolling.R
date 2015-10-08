poll= read.csv('AnonymityPoll.csv')
summary(poll$Smartphone) 

MidwestInterviewees = subset(poll, Region=="Midwest")
table(MidwestInterviewees$State)

sort(table(SouthInterviewees$State))

table(poll$Internet.Use, poll$Smartphone) 
