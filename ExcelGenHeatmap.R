# generate heatmap of feature correlation
install.packages("corrplot")
library(corrplot)

# read in RFE fit results
fitDF <- read.csv("./ExcelLeadingIndicators/rfe_glm_p0.05_morethan6months.csv")
fitDF$X <- NULL

# read in dataset
dataDF <- read.csv("./ExcelLeadingIndicators/MoreThanSixMonths.csv")

# filter to top 25 in terms of variable importance to the glm model
col.keep <- fitDF %>% filter(nparam <= 25) %>% select(feature)
col.keep.v <- as.vector(col.keep$feature)

#col.keep <- c("Retained90D","TotalDistinctUserCmds","TotalDistinctUserGlumps")
x.cor <- cor(dataDF[,(colnames(dataDF) %in% col.keep$feature)])

# corrplot is much nicer and easier to read
corrplot(x.cor, method="circle", type="lower", 
         order="hclust", 
         tl.col="black", tl.srt=45)
