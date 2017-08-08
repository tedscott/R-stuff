# ======================== Script  Information =================================
# PURPOSE: Exploratory plots and ratio analysis
#
#
#
# NOTES:
#   1)
#
# PROJECT INFORMATION:
#   Name  : 1314099_SaturnExpansion
#
# HISTORY:----
#   Date		        Remarks
#	-----------	   ---------------------------------------------------------------
#	2017-01-12      Created.   <mialdea>
#
# ==============================  Environment Setup  ===========================
palette(c("black", "#FDB100", "#0078FD",  "#800080","yellowgreen", "#CC5500", "#400080", "#008000", "#663000", "#FF51FF", "#386CB0", "#BDF9FF", "#FFCA99"))
OfficeColors <- c("#00B0F6", "#00BF7D", "#F8766D", "#a6cee3", "#E76BF3") #lighter
#OfficeColors <- c("#2B579A", "#217346", "#B7472A", "#0173C7", "#7D3778") #official

names(OfficeColors) <- c("Word", "Excel", "PowerPoint", "Outlook", "OWA")

options(stringsAsFactors=FALSE, cores=3)
require(ggplot2)
library(scales)
require(plyr)

require(CosmosToR)
#===============================  Cosmos VC Connection  ========================
vc <- vc_connection('https://cosmos14.osdinfra.net/cosmos/ACE.proc/', TRUE)
# submit_iscope(vc, 'SELECT COUNT(DISTINCT id) AS Cnt FROM (SSTREAM "/users/DIG-shared/Mobile/Metrics/Outlook/Android/Outlook_HVAs_2017-01-01.ss") WHERE OpenedMailOutlook==true; OUTPUT TO CONSOLE;')
#
# submit_iscope(vc, 'SELECT COUNT(DISTINCT id) AS Cnt FROM (SSTREAM "/users/DIG-shared/Mobile/Metrics/Outlook/OutlookDailyUsers_2017_01_01.ss") WHERE !string.IsNullOrEmpty(deviceId); OUTPUT TO CONSOLE;')

d <- ss_all(vc, "/users/DIG-shared/Mobile/Metrics/mobileHVAPlusMAALong_20161231.ss")

tbl <- xtabs(~OSName+App+AppHVA, d)
plot(tbl, color=OfficeColors, main="HVA user", las=2)

(ftbl <- (ftable(tbl, row.vars =c("OSName", "App"))))
summary(tbl)

ftbl[,2]/(ftbl[,2]+ftbl[,1])

require(vcd)
doubledecker(tbl)
mosaic(tbl, split = TRUE, mar = c(left = 3.5),
       labeling_args = list(offset_labels = c(left = 0.5),
                            offset_varnames = c(left = 1, top = 0.5)),
       spacing_args = list(start = 0.5, rate = 1.5),
       main="", highlighting = "AppHVA", highlighting_fill=OfficeColors[1:2])
#===== Aggregated QMM Metric ===================================================
#cd <- ss_all(vc, "/users/DIG-shared/Mobile/Metrics/Aggregated/qmm_20170131.ss")
#cd$fQMM <- with(cd, UsrCntQMM/TotUsrCnt)

#ggplot(data=cd, aes(x=factor(YearMonth), y=fQMM, color=OSName, group=OSName, label=format(fQMM*100, dig=2))) +
#  geom_line(size=2) + geom_label(show.legend = FALSE) +
#  scale_y_continuous(labels = percent, limits=c(0, 0.5), name="QMM Metric") +
#  scale_x_discrete(name="") +
#  theme(legend.title = element_blank()) +
#  scale_color_manual(values=palette()[-1], guide = guide_legend(reverse=TRUE))
