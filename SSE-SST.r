prediction=predict(model,newdata=foo)
SSE=sum((prediction-dataTest$score)^2)
SST=sum((mean(dataTrain$score)-dataTest$score)^2)
1-SSE/SST