

# Regression to Predict winds and Runs ------------------------------------

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball); 
summary(WinsReg); 
#Wins= 80.881375 + .105766 *RD >= 95;RD >133.4
# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball); 
summary(RunsReg);
RunsReg = lm(RS ~ OBP + SLG, data=moneyball); 
summary(RunsReg) 
#(BA is not very useful or makes no difference)