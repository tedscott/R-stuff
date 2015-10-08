# Read in data
baseball = read.csv("baseball.csv"); 
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002); str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA; str(moneyball)

# Scatterplot to check for linear relationship
plot(baseball$W,baseball$Team,col=ifelse(baseball$Playoffs==1,"red","blue")); 
plot(moneyball$RD, moneyball$W); Wins > 95 to make it to playoffs

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball); summary(WinsReg); Wins= 80.881375 + .105766 *RD >= 95;RD >133.4

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball); summary(RunsReg);

RunsReg = lm(RS ~ OBP + SLG, data=moneyball); summary(RunsReg) (BA is not very useful or makes no difference)
