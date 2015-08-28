n = 10
dice = 1:6

#This is wrong
#wrong <- rep(mean(sample(dice,n,TRUE)), 100)

#this is correct
right <- replicate(100, mean(sample(dice,n,TRUE)))

par(mfrow=c(1,2))

#hist(wrong)
hist(right)
boxplot(right)