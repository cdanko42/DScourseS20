ds <- read.csv("ProblemSets/PS11/Data/main.csv")
ds2 <- read.csv("ProblemSets/PS11/Data/statepop.csv")

subset2019 <- ds[ds$Year == "2019",]
totkilled= sum(subset2019[, 10])
abckilled = sum(subset2019$abc*subset2019[,10])
killratio = abckilled/totkilled

totpop = sum(ds2$X2019pop)
abcpop =sum(ds2$X2019pop*ds2$abc)
popratio = abcpop/totpop
binom2019 <- binom.test(abckilled,totkilled, p=popratio )

subset2018 <- ds[ds$Year == "2018",]
totkilled2018= sum(subset2018[, 10])
abckilled2018 = sum(subset2018$abc2018*subset2018[,10])
killratio2018 = abckilled2018/totkilled2018

totpop2018 = sum(ds2$X2018pop)
abcpop2018 =sum(ds2$X2018pop*ds2$abc2018)
popratio2018 = abcpop2018/totpop2018
binom2018 <- binom.test(abckilled2018,totkilled2018, p=popratio2018)

subset2017 <- ds[ds$Year == "2017",]
totkilled2017= sum(subset2017[,10])
abckilled2017 = sum(subset2017$abc2017*subset2017[,10])
killratio2017 = abckilled2017/totkilled2017

totpop2017 = sum(ds2$X2017pop)
abcpop2017 =sum(ds2$X2017pop*ds2$abc2017)
popratio2017 = abcpop2017/totpop2017
binom2017 <- binom.test(abckilled2017,totkilled2017, p=popratio2017)

subset2016 <- ds[ds$Year == "2016",]
totkilled2016= sum(subset2016[, 10])
abckilled2016 = sum(subset2016$abc2016*subset2016[,10])
killratio2016 = abckilled2016/totkilled2016

totpop2016 = sum(ds2$X2016pop)
abcpop2016 =sum(ds2$X2016pop*ds2$abc2016)
popratio2016 = abcpop2016/totpop2016
binom2016 <- binom.test(abckilled2016,totkilled2016, p=popratio2016)

subset2015 <- ds[ds$Year == "2015",]
totkilled2015= sum(subset2015[, 10])
abckilled2015 = sum(subset2015$abc2015*subset2015[,10])
killratio2015 = abckilled2015/totkilled2015

totpop2015 = sum(ds2$X2015pop)
abcpop2015 =sum(ds2$X2015pop*ds2$abc2015)
popratio2015 = abcpop2015/totpop2015
binom2015 <- binom.test(abckilled2015,totkilled2015, p=popratio2015)

subset2014 <- ds[ds$Year == "2014",]
totkilled2014= sum(subset2014[, 10])
abckilled2014 = sum(subset2014$abc2014*subset2014[,10])
killratio2014 = abckilled2014/totkilled2014

totpop2014 = sum(ds2$X2014pop)
abcpop2014 =sum(ds2$X2014pop*ds2$abc2014)
popratio2014 = abcpop2014/totpop2014
binom2014 <- binom.test(abckilled2014,totkilled2014, p=popratio2014)

subset2013 <- ds[ds$Year == "2013",]
totkilled2013= sum(subset2013[, 10])
abckilled2013 = sum(subset2013$abc2013*subset2013[,10])
killratio2013 = abckilled2013/totkilled2013

totpop2013 = sum(ds2$X2013pop)
abcpop2013 =sum(ds2$X2013pop*ds2$abc2013)
popratio2013 = abcpop2013/totpop2013
binom2013 <- binom.test(abckilled2013,totkilled2013, p=popratio2013)

ds3 <- as.data.frame(c(binom2019$statistic, binom2019$parameter, binom2019$estimate,binom2019$null.value, binom2019$p.value))
ds3[,2] <- c(binom2018$statistic, binom2018$parameter, binom2018$estimate,binom2018$null.value, binom2018$p.value)
ds3[,3] <- c(binom2017$statistic, binom2017$parameter, binom2017$estimate,binom2017$null.value, binom2017$p.value)
ds3[,4] <- c(binom2016$statistic, binom2016$parameter, binom2016$estimate,binom2016$null.value, binom2016$p.value)
ds3[,5] <- c(binom2015$statistic, binom2015$parameter, binom2015$estimate,binom2015$null.value, binom2015$p.value)
ds3[,6] <- c(binom2014$statistic, binom2014$parameter, binom2014$estimate,binom2014$null.value, binom2014$p.value)
ds3[,7] <- c(binom2013$statistic, binom2013$parameter, binom2013$estimate,binom2013$null.value, binom2013$p.value)
ds3 <- as.data.frame(t(as.matrix(ds3)))
Year = c(2019,2018,2017,2016,2015,2014,2013)
ds3 <- cbind(ds3, Year)
ds3 <- ds3[, c(6,1,2,3,4,5)]
colnames(ds3) <- c("Year", "Subset Fatalities", "Total Fatalities", "Subset Fatality Ratio", "Population Ratio", "P-value")

library(tidyverse)
library(gt)
library(webshot)

#Generate table 1
ds3 %>%
  gt(ds3) %>%
  tab_header(
    title = "Summary of Binomial Tests"
  ) %>%
  gtsave("ProblemSets/PS11/Graphics/Table1.png")

abccount <- sum(subset2019$abc)
totcount <- nrow(subset2019)
binom2019 <- binom.test(abccount,totcount, p=popratio)

abccount2018 <- sum(subset2018$abc2018)
totcount2018 <- nrow(subset2018)
binom2018 <- binom.test(abccount2018,totcount2018, p=popratio2018)

abccount2017 <- sum(subset2017$abc2017)
totcount2017 <- nrow(subset2017)
binom2017 <- binom.test(abccount2017,totcount2017, p=popratio2017)

abccount2016 <- sum(subset2016$abc2016)
totcount2016 <- nrow(subset2016)
binom2016 <- binom.test(abccount2016,totcount2016, p=popratio2016)

abccount2015 <- sum(subset2015$abc2015)
totcount2015 <- nrow(subset2015)
binom2015 <- binom.test(abccount2015,totcount2015, p=popratio2015)

abccount2014 <- sum(subset2014$abc2014)
totcount2014 <- nrow(subset2014)
binom2014 <- binom.test(abccount2014,totcount2014, p=popratio2014)

abccount2013 <- sum(subset2013$abc2013)
totcount2013 <- nrow(subset2013)
binom2013 <- binom.test(abccount2013,totcount2013, p=popratio2013)

ds3 <- as.data.frame(c(binom2019$statistic, binom2019$parameter, binom2019$estimate,binom2019$null.value, binom2019$p.value))
ds3[,2] <- c(binom2018$statistic, binom2018$parameter, binom2018$estimate,binom2018$null.value, binom2018$p.value)
ds3[,3] <- c(binom2017$statistic, binom2017$parameter, binom2017$estimate,binom2017$null.value, binom2017$p.value)
ds3[,4] <- c(binom2016$statistic, binom2016$parameter, binom2016$estimate,binom2016$null.value, binom2016$p.value)
ds3[,5] <- c(binom2015$statistic, binom2015$parameter, binom2015$estimate,binom2015$null.value, binom2015$p.value)
ds3[,6] <- c(binom2014$statistic, binom2014$parameter, binom2014$estimate,binom2014$null.value, binom2014$p.value)
ds3[,7] <- c(binom2013$statistic, binom2013$parameter, binom2013$estimate,binom2013$null.value, binom2013$p.value)
ds3 <- as.data.frame(t(as.matrix(ds3)))
Year = c(2019,2018,2017,2016,2015,2014,2013)
ds3 <- cbind(ds3, Year)
ds3 <- ds3[, c(6,1,2,3,4,5)]
colnames(ds3) <- c("Year", "Subset Shootings", "Total Shootings", "Subset Shooting Ratio", "Population Ratio", "P-value")

#Generate table 2
ds3 %>%
  gt(ds3) %>%
  tab_header(
    title = "Summary of Binomial Tests"
  ) %>%
  gtsave("ProblemSets/PS11/Graphics/Table2.png")

library(MCMCpack)
library(ggplot2)
library(ggdistribute)


set.seed(150)
#generate markov chain for total number of shootings
posterior2 <- MCbinomialbeta(abckilled, totkilled, alpha=1,beta=1, mc=10000 )
summary(posterior2)
posterior2 <- as.data.frame(posterior2)
ggplot(posterior2, aes(x=pi))+
  geom_posterior()+
  xlim(.35,.6)+
  labs(title= "2019 Posterior Distribution for Mass Shooting Fatalities")+
  geom_segment(aes(x = popratio, xend=popratio, y = .25, yend = 0), colour='#000000', size=1,arrow = arrow(length = unit(0.5, "cm")))+
  ggsave("ProblemSets/PS11/Graphics/Figure1.png")
summary(posterior2)


#Format and generate table 3
posterior2$sum <- posterior2$pi
posterior2$sum[posterior2$sum >= popratio] <- 1
posterior2$sum[posterior2$sum != 1] <- 0

ds3 <- as.data.frame(c(mean(posterior2$pi), sd(posterior2$pi), sum(posterior2$sum)))
ds3 <- as.data.frame(t(as.matrix(ds3)))
colnames(ds3) <- c("Mean", "Standard Deviation", "Samples > Ratio" )

ds3 %>%
  gt() %>%
  tab_header(
    title = "Summary of Posterior with Flat Priors "
  ) %>%
  gtsave("ProblemSets/PS11/Graphics/Table3.png")

#Calculate alpha and beta for prior
lambda = (1-popratio)/popratio
N = 400
alpha = N/(1+ lambda)^2 -1
beta = lambda*alpha

#Plot beta and save it as figure 3
p = seq(0,1, length=100)
png(file = "ProblemSets/PS11/Graphics/Figure2.png")
plot(p, dbeta(p, alpha, beta), main="Beta Distribution With Chosen Parameters",ylab="density", type ="l", col=4)
lines(p, dbeta(p, 1, 1), col=1)
dev.off()

posterior3 <- MCbinomialbeta(abckilled, totkilled, alpha=alpha,beta= beta, mc=10000 )
summary(posterior3)
posterior3 <- as.data.frame(posterior3)
summary(posterior3)
ggplot(posterior3, aes(x=pi))+
  geom_posterior()+
  xlim(.35,.6)+
  labs(title= "2019 Posterior Distribution for Mass Shooting Fatalities With Strong Priors")+
  geom_segment(aes(x = popratio, xend=popratio, y = .25, yend = 0), colour='#000000', size=1,arrow = arrow(length = unit(0.5, "cm")))+
  ggsave("ProblemSets/PS11/Graphics/Figure3.png")

posterior3$sum <- posterior3$pi
posterior3$sum[posterior3$sum >= popratio] <- 1
posterior3$sum[posterior3$sum != 1] <- 0

#Format and calculate table 4
ds3 <- as.data.frame(c(mean(posterior3$pi), sd(posterior3$pi), sum(posterior3$sum)))
ds3 <- as.data.frame(t(as.matrix(ds3)))
colnames(ds3) <- c("Mean", "Standard Deviation", "Samples > Ratio" )

ds3 %>%
  gt() %>%
  tab_header(
    title = "Summary of Posterior with Strong Priors Imposed"
  ) %>%
  gtsave("ProblemSets/PS11/Graphics/Table4.png")

#Calculate alpha and beta for prior
lambda = (1-popratio)/popratio
N = 7590
alpha2 = N/(1+ lambda)^2 -1
beta2 = lambda*alpha2

#Plot beta and save it as figure 4
png(file = "ProblemSets/PS11/Graphics/Figure4.png")
plot(p, dbeta(p, alpha2, beta2), main="Beta Distribution Comparison",ylab="density", type ="l", col=4)
lines(p, dbeta(p, alpha, beta), col=3)
lines(p, dbeta(p, 1, 1), col=1)
dev.off()

#Impose these priors on MCMC model
posterior5 <- MCbinomialbeta(abckilled, totkilled, alpha=alpha2, beta= beta2, mc=10000 )
posterior5 <- as.data.frame(posterior5)
posterior5$sum <- posterior5$pi
posterior5$sum[posterior5$sum >= popratio] <- 1
posterior5$sum[posterior5$sum != 1] <- 0

#Create figure 5
ggplot(posterior5, aes(x=pi))+
  geom_posterior()+
  xlim(.5,.6)+
  labs(title= "2019  Posterior Distribution for Mass Shooting Fatalities With Very Strong Priors") +
  geom_segment(aes(x = popratio, xend=popratio, y = .25, yend = 0), colour='#000000', size=1,arrow = arrow(length = unit(0.5, "cm")))+
  ggsave("ProblemSets/PS11/Graphics/Figure5.png")

#Format and create table 5
#Format and calculate table 4
ds3 <- as.data.frame(c(mean(posterior5$pi), sd(posterior5$pi), sum(posterior5$sum)))
ds3 <- as.data.frame(t(as.matrix(ds3)))
colnames(ds3) <- c("Mean", "Standard Deviation", "Samples > Ratio" )

ds3 %>%
  gt() %>%
  tab_header(
    title = "Summary of Posterior with Strong Priors Imposed"
  ) %>%
  gtsave("ProblemSets/PS11/Graphics/Table5.png")
