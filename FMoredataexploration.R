data.frame(v=1:4,ch=c("a","b","c","d"),n=10)             
#What is the Output?

BMI<-rnorm(n=1000, m=24.2, sd=2.2)  		
#Run str and summary commands

#Run the following and explain what each command does
rm(list=ls()) # Clears everything from the workspace
gender = rep(c("female","male"),c(1835,2691)) 
admitted = rep(c("yes","no","yes","no"),c(557,1278,1198,1493)) 
dept = rep(c("A","B","C","D","E","F","A","B","C","D","E","F"),
           c(89,17,202,131,94,24,19,8,391,244,299,317))
dept2 = rep(c("A","B","C","D","E","F","A","B","C","D","E","F"),
            c(512,353,120,138,53,22,313,207,205,279,138,351))
department = c(dept,dept2)# Create two sets and combine them

ucb = data.frame(gender,admitted,department) 
rm(gender,admitted,dept,dept2,department) 
ls()
