# look at tenant clusters based on health metrics
# start with per month to look at cluster stability over time
setwd("e:/R-stuff/")

library(ggplot2)
library(dplyr)
library(corrplot) 
#devtools::install_github("jaredlander/coefplot")
# if an error with ssl cert, do this first: install.packages(c("curl", "httr"))
library(coefplot) #coefficient plots
library(ggpubr) # plotting library
#library("PerformanceAnalytics")
library(useful) # for clustering
#install.packages("cluster")
#install.packages("factoextra")
library(cluster)
library(factoextra)

# maybe kmeans can use parallel?
# library(doParallel)
# registerDoParallel(cores=2)
# detectCores() 
# getDoParWorkers()


options(tibble.print_max = Inf, tibble.width = Inf) # to always show all rows and all columns.


# get the file
metrics <- read.csv(file.choose(), stringsAsFactors = FALSE)
summary(metrics)

metricsApr <- metrics %>% filter(grepl("2017-04-30", Date)) %>% select(-Date)


# create a tenure column
metricsApr <- metricsApr %>% 
  mutate(TenureDays = as.numeric(Sys.Date() - as.Date(Tenant_First_SubscriptionStartDate)))

# exploratory by segment group
# metricsAprSMB <- metricsApr %>% filter(SegmentGroup=="SMB")
# metricsAprEPG <- metricsApr %>% filter(SegmentGroup=="EPG")
# metricsAprCA <- metricsApr %>% filter(SegmentGroup=="CA")
# summary(metricsAprSMB)
# summary(metricsAprEPG)
# summary(metricsAprCA)

# remove big DF to save space
remove(metrics)

# exploratory plots

# all by SegmentGroup
ggplot(metricsApr, aes(x=NumWXPOUsers, y=NumQualityMembers)) + geom_point() + geom_smooth(method="glm") + facet_grid(.~SegmentGroup)

# log of each
# WXPO
ggplot(metricsApr, aes(x=log10(NumWXPOUsers), y=log10(NumQualityMembers))) + 
         geom_point() + 
         geom_smooth(method="glm") + 
         facet_grid(.~SegmentGroup)

#WXP
ggplot(metricsApr, aes(x=log10(NumWXPUsers), y=log10(NumQualityMembers))) + 
  geom_point() + 
  geom_smooth(method="glm") + 
  facet_grid(.~SegmentGroup)


# NumWXPODocsHaveMultipleUsers
ggplot(metricsApr, aes(x=NumWXPODocsHaveMultipleUsers, y=NumQualityMembers)) + 
  geom_point() + 
  geom_smooth(method="glm") + 
  facet_grid(.~SegmentGroup)

#WXPO
ggplot(metricsApr, aes(x=log10(NumWXPODocsHaveMultipleUsers), y=log10(NumQualityMembers))) + 
  geom_point() + 
  geom_smooth(method="glm") + 
  facet_grid(.~SegmentGroup)
#WXP
ggplot(metricsApr, aes(x=log10(NumWXPDocsHaveMultipleUsers), y=log10(NumQualityMembers))) + 
  geom_point() + 
  geom_smooth(method="glm") + 
  facet_grid(.~SegmentGroup)

# NumWXPOEdges
ggplot(metricsApr, aes(x=NumWXPOEdges, y=NumQualityMembers)) + 
  geom_point() + 
  geom_smooth(method="glm") + 
  facet_grid(.~SegmentGroup)
#WXPO
ggplot(metricsApr, aes(x=log10(NumWXPOEdges), y=log10(NumQualityMembers))) + 
  geom_point() + 
  geom_smooth(method=glm) + 
  facet_grid(.~SegmentGroup)
#WXP
ggplot(metricsApr, aes(x=log10(NumWXPEdges), y=log10(NumQualityMembers))) + 
  geom_point() + 
  geom_smooth(method=glm) + 
  facet_grid(.~SegmentGroup)

# NumWXPOBiDiRelationships
ggplot(metricsApr, aes(x=NumWXPOBiDiRelationships, y=NumQualityMembers)) + 
  geom_point() + 
  geom_smooth(method="glm") + 
  facet_grid(.~SegmentGroup)
#WXPO
ggplot(metricsApr, aes(x=log(NumWXPOBiDiRelationships), y=log(NumQualityMembers))) + 
  geom_point() + 
  geom_smooth(method=glm) + 
  facet_grid(.~SegmentGroup)

#WXP
ggplot(metricsApr, aes(x=log10(NumWXPBiDiRelationships), y=log10(NumQualityMembers))) + 
  geom_point() + 
  geom_smooth(method=glm) + 
  facet_grid(.~SegmentGroup)

# NumWXPONotBiDiEdges
ggplot(metricsApr, aes(x=NumWXPONotBiDiEdges, y=NumQualityMembers)) + 
  geom_point() + 
  geom_smooth(method="glm") + 
  facet_grid(.~SegmentGroup)

#WXPO
ggplot(metricsApr, aes(x=log10(NumWXPONotBiDiEdges), y=log10(NumQualityMembers))) + 
  geom_point() + 
  geom_smooth(method=glm) + 
  facet_grid(.~SegmentGroup)

#WXP
ggplot(metricsApr, aes(x=log10(NumWXPNotBiDiEdges), y=log10(NumQualityMembers))) + 
  geom_point() + 
  geom_smooth(method=glm) + 
  facet_grid(.~SegmentGroup)

# NumWXPONewViralUsers
ggplot(metricsApr, aes(x=NumWXPONewViralUsers, y=NumQualityMembers)) + 
  geom_point() + 
  geom_smooth(method="glm") + 
  facet_grid(.~SegmentGroup)
#WXPO
ggplot(metricsApr, aes(x=log10(NumWXPONewViralUsers), y=log10(NumQualityMembers))) + 
  geom_point() + 
  geom_smooth(method=glm) + 
  facet_grid(.~SegmentGroup)

# NumWXPNewViralUsers
ggplot(metricsApr, aes(x=NumWXPNewViralUsers, y=NumQualityMembers)) + 
  geom_point() + 
  geom_smooth(method="glm") + 
  facet_grid(.~SegmentGroup)

ggplot(metricsApr, aes(x=log10(NumWXPNewViralUsers), y=log10(NumQualityMembers))) + 
  geom_point() + 
  geom_smooth(method=glm) + 
  facet_grid(.~SegmentGroup)

# new viral users is not a great fit - check quantiles
quantile(metricsApr$NumWXPNewViralUsers, seq(0.99,1,0.001), na.rm=TRUE)


# WXPDegreeMax
ggplot(metricsApr, aes(x=WXPDegreeMax, y=NumQualityMembers)) + 
  geom_point() + 
  geom_smooth(method="glm") + 
  facet_grid(.~SegmentGroup)

ggplot(metricsApr, aes(x=log10(WXPDegreeMax), y=log10(NumQualityMembers))) + 
  geom_point() + 
  geom_smooth(method=glm) + 
  facet_grid(.~SegmentGroup)

# check tenure days vs QM
ggplot(metricsAprWXP, aes(x=log(TenureDays), y=log(NumQualityMembers))) + 
  geom_point() + 
  geom_smooth(method="glm") + 
  facet_grid(.~SegmentGroup)


# make a DF of just the WXP variables since individual app vars will correlate with them
metricsAprWXP <- metricsApr %>% 
  select(-contains("NumWXPO"), 
         -contains("Word"),
         -contains("Excel"),
         -contains("PowerPoint"),
         -contains("OneNote"),
         -WXPOInboundDegreeMax, -WXPOOutboundDegreeMax) 

# how does tenure look?
cor(metricsAprWXP$NumQualityMembers, metricsAprWXP$TenureDays)
# nearly 0, not a great fit

# quick GLM to get some coeffs
glmfit <- glm(NumQualityMembers ~ NumWXPUsers + 
                NumWXPDocsHaveMultipleUsers +
                NumWXPEdges +
                NumWXPBiDiRelationships + 
                NumWXPNotBiDiEdges + 
                NumWXPNewViralUsers +
                WXPOutboundDegreeMax + 
                WXPInboundDegreeMax +
                WXPDegreeMax +
                TenureDays, data=metricsAprWXP)

summary(glmfit)




# add more columns
# density 2E/(N*(N-1))
# avg degree: 2E/N
metricsAprWXP <- metricsAprWXP %>% mutate(
  WXPDensity = 2*NumWXPEdges/(NumWXPUsers*(NumWXPUsers - 1)),
  WXPMeanDegree = 2*NumWXPEdges/NumWXPUsers)

# replace Density and MeanDegree NaN with 0
metricsAprWXP$WXPDensity[is.nan(metricsAprWXP$WXPDensity)] <- 0
metricsAprWXP$WXPMeanDegree[is.nan(metricsAprWXP$WXPMeanDegree)] <- 0

# one last correlation check
metricsAprWXPBest <- metricsAprWXP %>% select(NumWXPUsers, 
                                              NumWXPEdges,
                                              NumWXPDocsHaveMultipleUsers,
                                              NumWXPBiDiRelationships, 
                                              NumWXPNewViralUsers,
                                              WXPDegreeMax,
                                              WXPMeanDegree,
                                              WXPDensity,
                                              TenureDays,
                                              NumQualityMembers)

corrs <- cor(metricsAprWXPBest, use="complete.obs")
corrplot(corrs, method="circle", type="lower", order="hclust", tl.col="black", tl.srt=45)

# this takes too long on this one month of data
#chart.Correlation(metricsAprWXPSmallNumeric, histogram=TRUE, pch=19)

# quick GLM of those vars
glmfitWXPbest<- glm(NumQualityMembers ~ NumWXPUsers + 
                    NumWXPEdges +
                    NumWXPDocsHaveMultipleUsers +
                    NumWXPBiDiRelationships + 
                    NumWXPNewViralUsers +
                    WXPDegreeMax +
                      WXPMeanDegree +
                      WXPDensity +
                    TenureDays, data=metricsAprWXP)

summary(glmfitWXPbest)

coefplot(glmfitWXPbest)

# plot those vs QM
ggplot(metricsAprWXP, aes(x=(WXPDensity), y=log10(NumQualityMembers))) + 
  geom_point() + 
  geom_smooth(method="glm") + 
  facet_grid(.~SegmentGroup)

ggplot(metricsAprWXP, aes(x=log10(WXPMeanDegree), y=log10(NumQualityMembers))) + 
  geom_point() + 
  geom_smooth(method="glm") + 
  facet_grid(.~SegmentGroup)



#better histogram
ggplot(metricsAprWXP, aes(x=log10(NumWXPUsers))) + 
  geom_histogram(bins=20, alpha=0.7) +
  scale_x_continuous(limits=c(0, 6)) +
  scale_y_continuous(limits=c(0, 40000), breaks=seq(0,40000,10000)) +
  geom_vline(aes(xintercept=log10(2000)), color="red", size=3) +
  geom_vline(aes(xintercept=log10(300)), color="blue", size=3) +
  geom_vline(aes(xintercept=log10(10000)), color="green", size=3) +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        axis.text.y  = element_text(size=16))

# figure out what sizes to use to bucket the tenants before clustering
# histogram
# probably should divide analysis up by size - check log10
ggplot(metricsAprWXP, aes(x=(SegmentGroup), y=log10(NumWXPUsers))) + 
  geom_boxplot() +
  scale_y_continuous(limits=c(0, 6), breaks=seq(0,6,1)) +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        axis.text.y  = element_text(size=16))


# quantiles on sizes to help decide on buckets
quantile(metricsAprWXP$NumWXPUsers, seq(0.5,1,0.05))

#50%    55%    60%    65%    70%    75%    80%    85%    90%    95%   100% 
#1      2      2      2      3      4      6      9     16     43 219019  

quantile(metricsAprWXP$NumWXPUsers, seq(0.9,1,0.005))
#90%     90.5%       91%     91.5%       92%     92.5%       93%     93.5%       94% 
#131.00    141.00    151.00    162.00    176.00    192.00    210.00    230.00    254.00 
#94.5%       95%     95.5%       96%     96.5%       97%     97.5%       98%     98.5% 
#283.00    322.00    364.00    421.00    493.00    589.00    718.00    913.00   1217.00 
#99%     99.5%      100% 
#1775.51   3279.51 219019.00

quantile(metricsAprWXP$NumWXPUsers, seq(0.99,1,0.0005))

# how many tenants have > 2000 cloud users
sum(metricsAprWXP$NumWXPUsers > 2000) # 1417 in Apr 2017

sum(metricsAprWXP$NumWXPUsers >= 2000 & metricsAprWXP$NumWXPUsers < 10000) # 1251

# how many have > 99.9%-tile (10000)
sum(metricsAprWXP$NumWXPUsers >= 10000) # 166

# how many in small bucket (< 300)
sum(metricsAprWXP$NumWXPUsers < 300) # 758712

sum(metricsAprWXP$NumWXPUsers >= 300 & metricsAprWXP$NumWXPUsers < 2000) #7185

# if we reduce the data to those small tenants, how do the feature coeffs look?
metricsAprWXPSmall <- metricsAprWXP %>% filter(NumWXPUsers <= 300)

glmfitWXPSmall<- glm(NumQualityMembers ~ NumWXPUsers + 
                       NumWXPEdges +
                       NumWXPDocsHaveMultipleUsers +
                       NumWXPBiDiRelationships + 
                       NumWXPNewViralUsers +
                       WXPDegreeMax +
                       WXPMeanDegree +
                       WXPDensity +
                       TenureDays, data=metricsAprWXPSmall)

summary(glmfitWXPSmall) # all are still significant
coefplot(glmfitWXPSmall) # but some flip sign

# reduce columns by removing inbound and outbound degrees
metricsAprWXP <- metricsAprWXP %>% select(-WXPOutboundDegreeMax, -WXPInboundDegreeMax)

# add t-shirt sizes as a column
metricsAprWXP$Size=case_when(metricsAprWXP$NumWXPUsers < 300 ~ "S",
                             metricsAprWXP$NumWXPUsers < 2000 ~ "M",
                             metricsAprWXP$NumWXPUsers < 10000 ~ "L",
                             metricsAprWXP$NumWXPUsers >= 10000 ~ "XL")
    

# get mean for each numeric col in t-shirt size group
metricsAprWXP %>% group_by(Size) %>% summarise_if(is.numeric, mean, na.rm = TRUE)

metricsAprWXP <- metricsAprWXP %>% add_count(Size)
meansBySize <- metricsAprWXP %>% 
  group_by(Size) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  arrange(NumWXPUsers) %>%
  rename(Count=n)

# tenure is negative for some due to missing subscription start date
# get the means for the tenure for those with valid date
meansTenureBySize <- metricsAprWXP %>% 
  group_by(Size) %>% 
  filter(TenureDays >= 0) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  arrange(NumWXPUsers) %>%
  rename(Count=n)
View(meansTenureBySize)

# round things to 4 places
meansBySize[, -1] <- meansBySize[,2:ncol(meansBySize)] %>% round(digits=3)
View(meansBySize)

write.csv(meansBySize,"meansbysizeAor2017.csv")

# time to bring in a few more months and look at variables over time
# and over tenure months


##################################################
# get the file for March
metrics <- read.csv("E:/random data/TenantHealthBestCols_May2016_Apr2017.csv", stringsAsFactors = FALSE)
summary(metrics)

metricsMar <- metrics %>% filter(grepl("2017-03-31", Date)) %>% select(-Date)


# create a tenure column
metricsMar <- metricsMar %>% 
  mutate(TenureDays = as.numeric(Sys.Date() - as.Date(Tenant_First_SubscriptionStartDate)))

# make a DF of just the WXP variables since individual app vars will correlate with them
metricsMarWXP <- metricsMar %>% 
  select(-contains("NumWXPO"), 
         -contains("Word"),
         -contains("Excel"),
         -contains("PowerPoint"),
         -contains("OneNote"),
         -WXPOInboundDegreeMax, -WXPOOutboundDegreeMax) 

# add more columns
# density 2E/(N*(N-1))
# avg degree: 2E/N
metricsMarWXP <- metricsMarWXP %>% mutate(
  WXPDensity = 2*NumWXPEdges/(NumWXPUsers*(NumWXPUsers - 1)),
  WXPMeanDegree = 2*NumWXPEdges/NumWXPUsers)

# replace Density and MeanDegree NaN with 0
metricsMarWXP$WXPDensity[is.nan(metricsMarWXP$WXPDensity)] <- 0
metricsMarWXP$WXPMeanDegree[is.nan(metricsMarWXP$WXPMeanDegree)] <- 0

# correlation check
metricsMarWXPFit <- metricsMarWXP %>% select(NumWXPUsers, 
                                              NumWXPEdges,
                                              NumWXPDocsHaveMultipleUsers,
                                              NumWXPBiDiRelationships, 
                                              NumWXPNewViralUsers,
                                              WXPDegreeMax,
                                              WXPMeanDegree,
                                              WXPDensity,
                                              TenureDays,
                                              NumQualityMembers)

corrsMar <- cor(metricsMarWXPFit, use="complete.obs")
corrplot(corrsMar, method="circle", type="lower", order="hclust", tl.col="black", tl.srt=45)

# this takes too long on this one month of data
#chart.Correlation(metricsAprWXPSmallNumeric, histogram=TRUE, pch=19)

# quick GLM of those vars
glmfitWXPMar<- glm(NumQualityMembers ~ NumWXPUsers + 
                      NumWXPEdges +
                      NumWXPDocsHaveMultipleUsers +
                      NumWXPBiDiRelationships + 
                      NumWXPNewViralUsers +
                      WXPDegreeMax +
                      WXPMeanDegree +
                      WXPDensity +
                      TenureDays, data=metricsMarWXP)

summary(glmfitWXPMar)
coefplot(glmfitWXPMar)

# remove what we don't need
remove(metricsMar,metricsMarWXPFit)


##################################################
# get the file for October 2016 (skip the seasonal months of Nov - Jan)
#metrics <- read.csv("E:/random data/TenantHealthBestCols_May2016_Apr2017.csv", stringsAsFactors = FALSE)
#summary(metrics)

metricsOct <- metrics %>% filter(grepl("2016-10-31", Date)) %>% select(-Date)


# create a tenure column
metricsOct <- metricsOct %>% 
  mutate(TenureDays = as.numeric(Sys.Date() - as.Date(Tenant_First_SubscriptionStartDate)))

# make a DF of just the WXP variables since individual app vars will correlate with them
metricsOctWXP <- metricsOct %>% 
  select(-contains("NumWXPO"), 
         -contains("Word"),
         -contains("Excel"),
         -contains("PowerPoint"),
         -contains("OneNote"),
         -WXPOInboundDegreeMax, -WXPOOutboundDegreeMax) 

# add more columns
# density 2E/(N*(N-1))
# avg degree: 2E/N
metricsOctWXP <- metricsOctWXP %>% mutate(
  WXPDensity = 2*NumWXPEdges/(NumWXPUsers*(NumWXPUsers - 1)),
  WXPMeanDegree = 2*NumWXPEdges/NumWXPUsers)

# replace Density and MeanDegree NaN with 0
metricsOctWXP$WXPDensity[is.nan(metricsOctWXP$WXPDensity)] <- 0
metricsOctWXP$WXPMeanDegree[is.nan(metricsOctWXP$WXPMeanDegree)] <- 0

# correlation check
metricsOctWXPFit <- metricsOctWXP %>% select(NumWXPUsers, 
                                             NumWXPEdges,
                                             NumWXPDocsHaveMultipleUsers,
                                             NumWXPBiDiRelationships, 
                                             NumWXPNewViralUsers,
                                             WXPDegreeMax,
                                             WXPMeanDegree,
                                             WXPDensity,
                                             TenureDays,
                                             NumQualityMembers)

corrsOct <- cor(metricsOctWXPFit, use="complete.obs")
corrplot(corrsOct, method="circle", type="lower", order="hclust", tl.col="black", tl.srt=45)

# this takes too long on this one month of data
#chart.Correlation(metricsAprWXPSmallNumeric, histogram=TRUE, pch=19)

# quick GLM of those vars
glmfitWXPOct<- glm(NumQualityMembers ~ NumWXPUsers + 
                     NumWXPEdges +
                     NumWXPDocsHaveMultipleUsers +
                     NumWXPBiDiRelationships + 
                     NumWXPNewViralUsers +
                     WXPDegreeMax +
                     WXPMeanDegree +
                     WXPDensity +
                     TenureDays, data=metricsOctWXP)

summary(glmfitWXPOct)
coefplot(glmfitWXPOct)

remove(metricsOct,metricsOctWXPFit)


##################################################
# get the file for September 2016 
#metrics <- read.csv("E:/random data/TenantHealthBestCols_May2016_Apr2017.csv", stringsAsFactors = FALSE)
#summary(metrics)

metricsSep <- metrics %>% filter(grepl("2016-09-30", Date)) %>% select(-Date)


# create a tenure column
metricsSep <- metricsSep %>% 
  mutate(TenureDays = as.numeric(Sys.Date() - as.Date(Tenant_First_SubscriptionStartDate)))

# make a DF of just the WXP variables since individual app vars will correlate with them
metricsSepWXP <- metricsSep %>% 
  select(-contains("NumWXPO"), 
         -contains("Word"),
         -contains("Excel"),
         -contains("PowerPoint"),
         -contains("OneNote"),
         -WXPOInboundDegreeMax, -WXPOOutboundDegreeMax) 

# add more columns
# density 2E/(N*(N-1))
# avg degree: 2E/N
metricsSepWXP <- metricsSepWXP %>% mutate(
  WXPDensity = 2*NumWXPEdges/(NumWXPUsers*(NumWXPUsers - 1)),
  WXPMeanDegree = 2*NumWXPEdges/NumWXPUsers)

# replace Density and MeanDegree NaN with 0
metricsSepWXP$WXPDensity[is.nan(metricsSepWXP$WXPDensity)] <- 0
metricsSepWXP$WXPMeanDegree[is.nan(metricsSepWXP$WXPMeanDegree)] <- 0

# correlation check
metricsSepWXPFit <- metricsSepWXP %>% select(NumWXPUsers, 
                                             NumWXPEdges,
                                             NumWXPDocsHaveMultipleUsers,
                                             NumWXPBiDiRelationships, 
                                             NumWXPNewViralUsers,
                                             WXPDegreeMax,
                                             WXPMeanDegree,
                                             WXPDensity,
                                             TenureDays,
                                             NumQualityMembers)

corrsSep <- cor(metricsSepWXPFit, use="complete.obs")
corrplot(corrsSep, method="circle", type="lower", order="hclust", tl.col="black", tl.srt=45)

# this takes too long on this one month of data
#chart.Correlation(metricsAprWXPSmallNumeric, histogram=TRUE, pch=19)

# quick GLM of those vars
glmfitWXPSep<- glm(NumQualityMembers ~ NumWXPUsers + 
                     NumWXPEdges +
                     NumWXPDocsHaveMultipleUsers +
                     NumWXPBiDiRelationships + 
                     NumWXPNewViralUsers +
                     WXPDegreeMax +
                     WXPMeanDegree +
                     WXPDensity +
                     TenureDays, data=metricsSepWXP)

summary(glmfitWXPSep)
coefplot(glmfitWXPSep)

remove(metricsSep,metricsSepWXPFit)

##################################################
# get the file for May 2016 
#metrics <- read.csv("E:/random data/TenantHealthBestCols_May2016_Apr2017.csv", stringsAsFactors = FALSE)
#summary(metrics)

metricsMay <- metrics %>% filter(grepl("2016-05-31", Date)) %>% select(-Date)


# create a tenure column
metricsMay <- metricsMay %>% 
  mutate(TenureDays = as.numeric(Sys.Date() - as.Date(Tenant_First_SubscriptionStartDate)))

# make a DF of just the WXP variables since individual app vars will correlate with them
metricsMayWXP <- metricsMay %>% 
  select(-contains("NumWXPO"), 
         -contains("Word"),
         -contains("Excel"),
         -contains("PowerPoint"),
         -contains("OneNote"),
         -WXPOInboundDegreeMax, -WXPOOutboundDegreeMax) 

# add more columns
# density 2E/(N*(N-1))
# avg degree: 2E/N
metricsMayWXP <- metricsMayWXP %>% mutate(
  WXPDensity = 2*NumWXPEdges/(NumWXPUsers*(NumWXPUsers - 1)),
  WXPMeanDegree = 2*NumWXPEdges/NumWXPUsers)

# replace Density and MeanDegree NaN with 0
metricsMayWXP$WXPDensity[is.nan(metricsMayWXP$WXPDensity)] <- 0
metricsMayWXP$WXPMeanDegree[is.nan(metricsMayWXP$WXPMeanDegree)] <- 0

# correlation check
metricsMayWXPFit <- metricsMayWXP %>% select(NumWXPUsers, 
                                             NumWXPEdges,
                                             NumWXPDocsHaveMultipleUsers,
                                             NumWXPBiDiRelationships, 
                                             NumWXPNewViralUsers,
                                             WXPDegreeMax,
                                             WXPMeanDegree,
                                             WXPDensity,
                                             TenureDays,
                                             NumQualityMembers)

corrsMay <- cor(metricsMayWXPFit, use="complete.obs")
corrplot(corrsMay, method="circle", type="lower", order="hclust", tl.col="black", tl.srt=45)

# this takes too long on this one month of data
#chart.Correlation(metricsAprWXPSmallNumeric, histogram=TRUE, pch=19)

# quick GLM of those vars
glmfitWXPMay<- glm(NumQualityMembers ~ NumWXPUsers + 
                     NumWXPEdges +
                     NumWXPDocsHaveMultipleUsers +
                     NumWXPBiDiRelationships + 
                     NumWXPNewViralUsers +
                     WXPDegreeMax +
                     WXPMeanDegree +
                     WXPDensity +
                     TenureDays, data=metricsMayWXP)

summary(glmfitWXPMay)
coefplot(glmfitWXPMay)

remove(metricsMay,metricsMayWXPFit)

# look at the means as we did for April 2017 by t-shirt size for comparison
# add t-shirt sizes as a column
metricsMayWXP$Size=case_when(metricsMayWXP$NumWXPUsers < 300 ~ "S",
                             metricsMayWXP$NumWXPUsers < 2000 ~ "M",
                             metricsMayWXP$NumWXPUsers < 10000 ~ "L",
                             metricsMayWXP$NumWXPUsers >= 10000 ~ "XL")


# get mean for each numeric col in t-shirt size group
metricsMayWXP %>% group_by(Size) %>% summarise_if(is.numeric, mean, na.rm = TRUE)

metricsMayWXP <- metricsMayWXP %>% add_count(Size) %>% select(-WXPOutboundDegreeMax, -WXPInboundDegreeMax)
meansBySize <- metricsMayWXP %>% 
  group_by(Size) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  arrange(NumWXPUsers) %>%
  rename(Count=n)

# tenure is negative for some due to missing subscription start date
# get the means for the tenure for those with valid date
meansTenureBySize <- metricsMayWXP %>% 
  group_by(Size) %>% 
  filter(TenureDays >= 0) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  arrange(NumWXPUsers) %>%
  rename(Count=n)
View(meansTenureBySize)

# round things to 4 places
meansBySize[, -1] <- meansBySize[,2:ncol(meansBySize)] %>% round(digits=3)
View(meansBySize)

write.csv(meansBySize,"meansbysizeMay2016.csv")


##########################################################
# still feel like there are too many features for clustering - try Boruta to get feature importance and reduce the number
install.packages("Boruta")
library(Boruta)

set.seed(121773)

# get a 50000 row sample since Boruta is slow!
# Boruta does not run parallel on windows (uses ranger package)
#rawDF.sample <- rawDF[sample(nrow(rawDF), 50000), ]

# remove non-numeric columns and TenureDays
# filter to those not Small tenants (reduce N to ~ 8k)
# April 2017
metricsAprWXPNoSmall <- metricsAprWXP %>% filter(Size != "S")
metricsAprWXPNumeric <- metricsAprWXPNoSmall %>% select_if(is.numeric) %>% select(-n)

# N = 8602, run Boruta
boruta.train <- Boruta(NumQualityMembers~., data=metricsAprWXPNumeric, doTrace=2)
print(boruta.train)
plot(boruta.train, las=2)

# remove TenureDays, NewViralUsers, WXPDegreeMax
boruta.train2 <- Boruta(NumQualityMembers~.-TenureDays-NumWXPNewViralUsers-WXPDegreeMax, data=metricsAprWXPNumeric, doTrace=2)
print(boruta.train2)
plot(boruta.train2, las=2)

# compare with Oct 2016
metricsOctWXPNoSmall <- metricsOctWXP %>% filter(NumWXPUsers >= 300)
metricsOctWXPNumeric <- metricsOctWXPNoSmall %>% 
  select_if(is.numeric) %>% 
  select(-contains("WXPO"), -WXPInboundDegreeMax)

# remove TenureDays, NewViralUsers, WXPDegreeMax
boruta.train <- Boruta(NumQualityMembers~.-TenureDays-NumWXPNewViralUsers-WXPDegreeMax, data=metricsOctWXPNumeric, doTrace=2)
print(boruta.train)
plot(boruta.train, las=2)

# some cleanup
remove(glmfit, glmfitWXPbest, glmfitWXPMar, glmfitWXPMay, glmfitWXPOct, glmfitWXPSep, glmfitWXPSmall)
remove(metricsApr, metricsMay, metricsSep, metricsMar, metricsOct, metricsOctWXPNoSmall, metricsOctWXPNumeric)


#################
# alright, variables for clustering will be
colNames <- c("NumWXPUsers", "NumWXPDocsHaveMultipleUsers", "NumWXPEdges",
              "NumWXPBiDiRelationships", "NumWXPNotBiDiEdges", "WXPDensity", 
              "WXPMeanDegree", "NumQualityMembers")

# subset metricsAprWXPNumeric (no small tenants) and run a simple kmeans
metricsAprWXPNumericBest <- metricsAprWXPNumeric %>% select(colNames)
set.seed(732)

#  time this part!
start.time <- Sys.time()

AprClust <- kmeans(x=metricsAprWXPNumericBest, centers=5)
AprClust$size
plot(AprClust, data=metricsAprWXPNumericBest)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# the five clusters do look pretty distinct, but cluster 1 points are pretty sparse in the plot
# of PCA1 vs PCA 2

# run it again adding in some random starts
set.seed(121773)
start.time <- Sys.time()
AprClust2 <- kmeans(x=metricsAprWXPNumericBest, centers=5, nstart=25)
AprClust2$size
plot(AprClust2, data=metricsAprWXPNumericBest)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# 12 min, same results as above, so don't need that many random starts. Will do 5.

# plot with tenant t-shirt size (M, L, XL) as class to see if it is a mix or if they 
# separate into the clusters with good correlation
# metricsAprWXPNoSmall has the Size column
start.time <- Sys.time()

AprClust3 <- kmeans(x=metricsAprWXPNumericBest, centers=5, nstart=5)
AprClust3$size
plot(AprClust3, data=metricsAprWXPNoSmall, class="Size")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#use fviz plot
# Visualize k-means clusters
#fviz_cluster(AprClust3, data = metricsAprWXPNoSmall, geom = "point",
#             stand = FALSE)


# use Hartigan's rule to find optimal number of clusters
# FitKMeans from "useful" package
start.time <- Sys.time()
findClusterNum <- FitKMeans(metricsAprWXPNumericBest, max.clusters=30, nstart=10, seed=121773)
findClusterNum

PlotHartigan(findClusterNum)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# 28 sec, local min at 2 clusters, then continued drop out to 30
# so 2 might be optimal?


# also good to check the Gap Statistic from "cluster" package


start.time <- Sys.time()
theGapStat <- clusGap(metricsAprWXPNumericBest, FUN = kmeans, K.max=20, nstart=10)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# 36 min for K.max=20
# should do 40?

gapDF <- as.data.frame(theGapStat$Tab)
View(gapDF)

# plot the gap stat vs # of clusters
plot(theGapStat, frame = FALSE, xlab = "Number of clusters k")
print(theGapStat, method = "firstmax")
# suggests 1 cluster is best

# Use factoextra
fviz_gap_stat(theGapStat)

# ggplot it for nicer visual?
ggplot(gapDF, aes(x=1:nrow(gapDF))) +
  geom_line(aes(y=gap), color="red") +
  geom_point(aes(y=gap), color="red") +
  geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim), color="red") +
  labs(x="Number of Clusters", y="Gap")

######################################
#
# one cluster isn't particularly useful - see what happens if I remove the XL tenants
# so now just M and L (300 to 10000 WXPUsers)

metricsAprWXPNoSmallnoXL <- metricsAprWXP %>% filter(Size != "S" & Size != "XL")
metricsAprWXPNumeric2 <- metricsAprWXPNoSmallnoXL %>% select_if(is.numeric) %>% select(-n)
# subset metricsAprWXPNumeric (no small tenants) and run a simple kmeans
metricsAprWXPNumericBest2 <- metricsAprWXPNumeric2 %>% select(colNames)
set.seed(732)

#  time this part!
start.time <- Sys.time()

AprClust4 <- kmeans(x=metricsAprWXPNumericBest2, centers=3)
AprClust4$size
plot(AprClust4, data=metricsAprWXPNoSmallnoXL, class="Size")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# 12 min - lots of overlap between cluster 1 & 2 for M and L tenants

# generate Gap stat for up to 20 clusters, M & L tenants
start.time <- Sys.time()
theGapStat2 <- clusGap(metricsAprWXPNumericBest2, FUN = kmeans, K.max=20, nstart=5)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# XX min for K.max=20


gapDF2 <- as.data.frame(theGapStat2$Tab)
View(gapDF2)

# plot the gap stat vs # of clusters
plot(theGapStat2, frame = FALSE, xlab = "Number of clusters k")
print(theGapStat2, method = "firstmax")
fviz_gap_stat(theGapStat2)

#########
# perform same operations for Oct 2016. If it still yields one cluster, then I guess we are good
# to just choose randomly from all clusters M/L and XL for experiments???
# compare with Oct 2016

metricsOctWXP$Size=case_when(metricsOctWXP$NumWXPUsers < 300 ~ "S",
                             metricsOctWXP$NumWXPUsers < 2000 ~ "M",
                             metricsOctWXP$NumWXPUsers < 10000 ~ "L",
                             metricsOctWXP$NumWXPUsers >= 10000 ~ "XL")

metricsOctWXPNoSmall <- metricsOctWXP %>% filter(Size != "S")
metricsOctWXPNumeric <- metricsOctWXPNoSmall %>% 
  select_if(is.numeric) %>% 
  select(-contains("WXPO"), -WXPInboundDegreeMax)



metricsOctWXPNumericBest <- metricsOctWXPNumeric %>% select(colNames)
set.seed(732)

#  time this part!
start.time <- Sys.time()

OctClust <- kmeans(x=metricsOctWXPNumericBest, centers=3)
OctClust$size
plot(OctClust, data=metricsOctWXPNoSmall, class="Size")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# 9 min and still looks like 1 cluster, although maybe two? Check Gap stat to convince myself.

# generate Gap stat for up to 20 clusters, M & L tenants
start.time <- Sys.time()
theGapStatOct <- clusGap(metricsOctWXPNumericBest, FUN = kmeans, K.max=20, nstart=5)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# XX min for K.max=20


gapDFOct <- as.data.frame(theGapStatOct$Tab)
View(gapDFOct)

# plot the gap stat vs # of clusters
plot(theGapStatOct, frame = FALSE, xlab = "Number of clusters k")
print(theGapStatOct, method = "firstmax")

fviz_gap_stat(theGapStatOct)
