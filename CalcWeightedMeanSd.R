# tedscott
# 10.19.2016
# calc average retention rates for slices of WXP leading indicators
# using preaggrgated streamfile from cosmos

# load needed libraries
library(dplyr)
library(SDMTools)

# use CosmosToR package to download from cosmos streams directly to DF
library(CosmosToR)
vc <- vc_connection('https://cosmos14.osdinfra.net/cosmos/ACE.proc/')

# path to stream in cosmos
# adjust for new install stream versus long tenure + 1 day usage streams
filePath <- "/users/DIG-shared/WXPLeadingIndicators/AllTenure/win32/WXPLongTenureBaselineRetentionCalulations_2016_01_01_to_2016_03_31_30.30.90.ss"
#filePath <- "/users/DIG-shared/WXPLeadingIndicators/win32/WXPBaselineRetentionCalulations_2016_01_01_to_2016_03_31_30.30.90.ss"

retentionAll <- ss_all(vc, filePath)

# current list of slices for new installs
#slices <- c("All", 
#            "Trial", 
#            "Consumer", 
#            "Commercial", 
#            "NonSub", 
#            "UnknownSub", 
#            "Edu", 
#            "Male", 
#            "Female")

# long tenure slices + 1 day usage
slices <- c("All", "Consumer", "Commercial", "NonSub", "UnknownSub", "NewInstallUsage_1Day", "WithTenureUsage_1Day")


# put the segment in the grepl to get results
groupRet <- retentionAll %>% 
  filter(grepl(slices[1],Cohort)) %>%
  select(MsoAppName,Retention,Count) %>%
  group_by(MsoAppName) %>% 
  mutate(AvgRet = round(wt.mean(Retention, Count),1), SD = round(wt.sd(Retention, Count),1), N = sum(Count)) %>%
  select(MsoAppName, AvgRet, SD, N) %>%
  distinct(MsoAppName, .keep_all=TRUE) %>%
  mutate(slice = slices[1])

# loop through remaining
for(slice in slices[2:length(slices)]) {
  tempRet <- retentionAll %>% 
    filter(grepl(slice,Cohort)) %>%
    select(MsoAppName,Retention,Count) %>%
    group_by(MsoAppName) %>% 
    mutate(AvgRet = round(wt.mean(Retention, Count),1), SD = round(wt.sd(Retention, Count),1), N = sum(Count)) %>%
    select(MsoAppName, AvgRet, SD, N) %>%
    distinct(MsoAppName, .keep_all=TRUE) %>%
    mutate(slice = slice)
  groupRet <- rbind(groupRet, tempRet)
}

# adjust output as needed
#write.csv(groupRet, "WXPLongTenureBaselineRetentionCals.csv")
write.csv(groupRet, "WXPLongTenureBaselineRetentionCals.csv")