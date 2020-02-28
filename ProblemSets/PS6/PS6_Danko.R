library(tidyverse)

ds <- read.csv("~/DScourseS20/train.csv")
colnames(ds)

#The point margin is 'target'. We want to predict the point spread using certain predictors
#We're interested in weighted season offensive rating, season defensive rating, and season pace on target for now, so we check for missing values
sum(is.na(ds$wSea_ORtg_x))
sum(is.na(ds$wSea_DRtg_x))
sum(is.na(ds$wSea_Pace_x))
nrow(ds)
#We have 6538 missing values, out of over 100000 observations, so we can drop them and have plenty left
ds2 <- ds[complete.cases(ds[, 5:7 ]),]

nrow(ds2)
#Notice that only around 6000 observations were dropped, this suggests that the missing data is for the same teams. A team missing an offensive rating is more likely to be missing a defensive rating, ect

#Plot weighted pace against target
ggplot(ds2, aes(wSea_Pace_x, target ))+geom_point()+geom_smooth(method= "lm")
#Well, hm. This seems to have no correlation at all.

#Plot weighted Offensive rating against target
ggplot(ds2, aes(Sea_ORtg_x, target))+geom_point()+geom_smooth(method="lm")
#This is clearly positively correlated. Let's try and narrow this down by defining a new variable that is the diference in ORtg between the two teams

#First, throw out columns where there are NAs in the other teams columns
ds2 <- ds[complete.cases(ds[, 36:38]),]
ds2 <- mutate(ds2, woffdiff = wSea_ORtg_x - wSea_ORtg_y)
ds2 <- mutate(ds2, wpacediff = wSea_Pace_x - wSea_Pace_y)
ds2 <- mutate(ds2, defdiff = wSea_DRtg_x - wSea_DRtg_y)
ggplot(ds2, aes(woffdiff, target))+geom_point()+geom_smooth(method="lm")

#There is a clear positive correlation between the difference in offensive rating and the score spread
ggplot(ds2, aes(defdiff, target))+geom_point()+geom_smooth(method="lm")

#This is strange, it appears that the correlation between the difference in defensive rating and the score spread is negative
ggplot(ds2, aes(wpacediff, target))+geom_point()+geom_smooth(method="lm")
#The corrlation here is still pretty insignificant

#Let's try out some interaction between defdiff and woffdiff to predict target
library(plot3D)
attach(ds2)
scatter2D(woffdiff, defdiff, labels, target, colvar = target, col = NULL, add=FALSE)

#Now let's interact pace and offensive difference
scatter2D(woffdiff, wpacediff, labels, target, colvar = target, col = NULL, add=FALSE)
