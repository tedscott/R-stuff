# generate the rfe-plots

fitDF <- read.csv("./ExcelLeadingIndicators/rfe_glm_p0.001_OneWeekToOneMonth_500000sample.csv")

fitDF$X <- NULL


View(fitDF)

# filter to top 20 in terms of variable importance to the glm model
filteredDF <- fitDF %>% filter(nparam <= 20)

p <- ggplot(filteredDF, aes(x=nparam, y=ROC,ymin=ROC-ROCSD, ymax=ROC+ROCSD, label=feature)) 
p <- p + geom_point() + geom_errorbar(width=0.5)
p <- p + geom_text(angle=90, hjust = 0, aes(y=0.55)) 
p <- p + xlab("Features (#)") + ylab("ROC")
print(p)
