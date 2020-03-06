library(stargazer)
library(tidyverse)
library(broom)
library(mice)

#read in table
df <- read.csv("ModelingOptimization/wages.csv")
colnames(df)

#Drop missing values for tenure and hgc 
df2 <- df[complete.cases(df[,2]),]
df2 <- df2[complete.cases(df[,4]),]
stargazer(df2, type = "latex", title = "", style = "default", summary = TRUE)

#create tenuresq variable
df2 <- mutate(df2, tenuresq=tenure^2)
df3 <- df2[complete.cases(df2[,1]),]

#Estimate model with dropped NA values for logwage
est <- lm(logwage ~ hgc + college + tenure +tenuresq+age+married,data=df3)
tidy(est)

#Create new dataset to mean impute into
df4 <- df2
df4$logwage[is.na(df4$logwage)] <- mean(df4$logwage,na.rm =TRUE)

#Estimate model with mean imputation
est2 <- lm(logwage ~ hgc + college + tenure +tenuresq+age+married,data=df4)
tidy(est2)

#perform single imputation
predicted_value <- mice(df2, m = 1, method = "pmm") 
df5 <- complete(predicted_value)         

#Estimate model with predicted mean imputation
est3 <- lm(logwage ~ hgc + college + tenure +tenuresq+age+married,data=df5)
tidy(est3)

#perform multiple imputation
predicted_value2 <- mice(df2, m = 5, method = "pmm") 
df6 <- complete(predicted_value2)  

#estimate model with multiple imputation
est4 <- lm(logwage ~ hgc + college + tenure +tenuresq+age+married,data=df6)
tidy(est4)

stargazer(est, est2, est3,est4, title="Regression Results",
          font.size = "tiny", column.sep.width = "1pt",
          dep.var.labels=c("Log Wage"),
          covariate.labels=c("HGC","College",
                             "Tenure","Tenuresq","Age","Married"), out = "regression.txt")
