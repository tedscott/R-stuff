# quick power tests of some funnel ratios
setwd("e:/R-stuff/")

library(dplyr)

# data is from all tenants in April 2017 
# cduze's dualuse stream
# total N for tenants used in calcs is 8115 (slightly fewer than my tenant list)
DF <- read.csv("../random data/funnelStatsApril2017.csv")


deltaRatio <- 0.05

ActiveEnabledRatio5 <- power.t.test(n=NULL, DF$SD_ActiveEnabledRatio, sig.level=0.05,
                             power=0.8, alternative = "one.sided", delta=deltaRatio*DF$Mean_ActiveEnabledRatio)$n

WXPCloudAllCoudRatio5 <- power.t.test(n=NULL, DF$SD_WXPCloudToAllCloudRatio, sig.level=0.05,
                                     power=0.8, alternative = "one.sided", delta=deltaRatio*DF$Mean_WXPCloudToAllCloudRatio)$n
# spit them out
ActiveEnabledRatio5
WXPCloudAllCoudRatio5
# > ActiveEnabledRatio
# [1] 2540.733
# > WXPCloudAllCoudRatio
# [1] 349.3844

# now make it 2% difference
deltaRatio <- 0.02

ActiveEnabledRatio2 <- power.t.test(n=NULL, DF$SD_ActiveEnabledRatio, sig.level=0.05,
                                    power=0.8, alternative = "one.sided", delta=deltaRatio*DF$Mean_ActiveEnabledRatio)$n

WXPCloudAllCoudRatio2 <- power.t.test(n=NULL, DF$SD_WXPCloudToAllCloudRatio, sig.level=0.05,
                                      power=0.8, alternative = "one.sided", delta=deltaRatio*DF$Mean_WXPCloudToAllCloudRatio)$n
# spit them out
ActiveEnabledRatio2
WXPCloudAllCoudRatio2
# > ActiveEnabledRatio2
# [1] 15876.03
# > WXPCloudAllCoudRatio2
# [1] 2180.089
# > 