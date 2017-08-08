
library(dplyr)
library(ggplot2)

fresnoRaw <- read.csv("e:/random data/FresnoUserInfluenceChangesFeb2017.csv")

summary(fresnoRaw)

fresnoSmall <- fresnoRaw
#fresnoSmall <- fresnoRaw %>% filter(PctChange < 1000)

fresnoSmall <- fresnoRaw %>% mutate(thing = ifelse(PctChange < 0, "Decrease", 
                                                     ifelse(PctChange == 0, "Same", 
                                                            ifelse(PctChange > 0, "Increase", "Error"))))

theplot <- ggplot(fresnoSmall, aes(x=WXPInfluenceScore, y=WXPOInfluenceScore, color=thing)) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(.~thing)
  #geom_histogram(bins=20, alpha=0.5, position="identity") +
  #geom_density(alpha=0.5)

theplot

summary(fresnoSmall$thing=="Increase")
summary(fresnoSmall$thing=="Decrease")
