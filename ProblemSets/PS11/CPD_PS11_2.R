library(readxl)
ds <- read_excel("ProblemSets/PS11/Data/shootings13_19.xlsx")
ds$abc <- ds$State
ds$abc [ds$abc == "California" ] <- 1 
ds$abc [ds$abc == "New Jersey"] <- 1 
ds$abc [ds$abc == "Connecticut"] <- 1  
ds$abc [ds$abc == "New York"] <- 1 
ds$abc [ds$abc == "Hawaii"] <- 1 
ds$abc [ds$abc == "New Jersey"] <- 1 
ds$abc [ds$abc == "Maryland"] <- 1 
ds$abc [ds$abc == "Massachusetts"] <- 1 
ds$abc [ds$abc == "Illinois"] <- 1 
ds$abc [ds$abc == "Rhode Island"] <- 1 
ds$abc [ds$abc == "Washington"] <- 1 
ds$abc [ds$abc == "Deleware"] <- 1 
ds$abc [ds$abc == "Pennsylvania"] <- 1 
ds$abc [ds$abc == "Minnesota"] <- 1 
ds$abc [ds$abc == "Colorado"] <- 1 
ds$abc [ds$abc == "Iowa"] <- 1 
ds$abc [ds$abc == "Michigan"] <- 1 
ds$abc [ds$abc == "Wisconsin"] <- 1 
ds$abc [ds$abc == "Oregon"] <- 1 
ds$abc [ds$abc == "New Mexico"] <- 1 
ds$abc [ds$abc == "Nevada"] <- 1 
ds$abc [ds$abc == "Nebraska"] <- 1 
ds$abc [ds$abc == "Florida"] <- 1 
ds$abc [ds$abc == "Vermont" ] <- 1
ds$abc [ds$abc == "District of Columbia" ] <- 1
ds$abc [ds$abc != "1"] <- 0
ds$abc <- as.integer(ds$abc)
ds <- ds[-c(2342),]

ds$abc2018 <- ds$abc
ds$abc2018[ds$State=="Nevada"] <- 0
ds$abc2018[ds$State=="New Mexico"] <- 0
ds$abc2018[ds$State=="Vermont"] <- 0

ds$abc2017 <- ds$abc2018
ds$abc2017[ds$State=="Florida"] <- 0
ds$abc2017[ds$State=="Nebraska"] <- 0

ds$abc2016 <- ds$abc2017
ds$abc2016[ds$State == "Nevada"] <- 1

ds$abc2015 <- ds$abc2017
ds$abc2015[ds$State == "Wisconsin"] <- 0

ds$abc2014 <- ds$abc2017
ds$abc2014[ds$State == "Oregon"] <- 0

ds$abc2013 <- ds$abc2014

#Read in
ds2 <- read_excel("ProblemSets/PS11/Data/statepop19.xlsx")
for (i in 1:8){
  ds2 <- ds2[-c(1),]
}

for (i in 1:8){
  ds2 <- ds2[-c(52),]
}
names(ds2)[1] <- "States"
names(ds2)[13] <- "2019pop"
names(ds2)[12] <- "2018pop"
names(ds2)[11] <- "2017pop"
names(ds2)[10] <- "2016pop"
names(ds2)[9] <- "2015pop"
names(ds2)[8] <- "2014pop"
names(ds2)[7] <- "2013pop"
ds2$`2019pop` <- as.integer(ds2$`2019pop`)
ds2$abc <- ds2$States
ds2$abc <- sub('.', '', ds2$abc)
ds2$States <- sub('.', '', ds2$States)
ds2$abc [ds2$abc == "California" ] <- 1 
ds2$abc [ds2$abc == "New Jersey"] <- 1 
ds2$abc [ds2$abc == "Connecticut"] <- 1  
ds2$abc [ds2$abc == "New York"] <- 1 
ds2$abc [ds2$abc == "Hawaii"] <- 1 
ds2$abc [ds2$abc == "New Jersey"] <- 1 
ds2$abc [ds2$abc == "Maryland"] <- 1 
ds2$abc [ds2$abc == "Nevada"] <- 1 
ds2$abc [ds2$abc == "Massachusetts"] <- 1 
ds2$abc [ds2$abc == "Illinois"] <- 1 
ds2$abc [ds2$abc == "Rhode Island"] <- 1 
ds2$abc [ds2$abc == "Washington"] <- 1 
ds2$abc [ds2$abc == "Deleware"] <- 1 
ds2$abc [ds2$abc == "Pennsylvania"] <- 1 
ds2$abc [ds2$abc == "Minnesota"] <- 1 
ds2$abc [ds2$abc == "Colorado"] <- 1 
ds2$abc [ds2$abc == "Iowa"] <- 1 
ds2$abc [ds2$abc == "Michigan"] <- 1 
ds2$abc [ds2$abc == "Wisconsin"] <- 1 
ds2$abc [ds2$abc == "Oregon"] <- 1 
ds2$abc [ds2$abc == "New Mexico"] <- 1 
ds2$abc [ds2$abc == "Nebraska"] <- 1 
ds2$abc [ds2$abc == "Florida"] <- 1 
ds2$abc [ds2$abc == "Vermont" ] <- 1
ds2$abc [ds2$abc == "District of Columbia" ] <- 1
ds2$abc [ds2$abc != "1"] <- 0
ds2$abc <- as.integer(ds2$abc)

ds2$abc2018 <- ds2$abc
ds2$abc2018[ds2$States=="Nevada"] <- 0
ds2$abc2018[ds2$States=="New Mexico"] <- 0
ds2$abc2018[ds2$States=="Vermont"] <- 0


ds2$abc2017 <- ds2$abc2018
ds2$abc2017[ds2$States=="Florida"] <- 0
ds2$abc2017[ds2$States=="Nebraska"] <- 0

ds2$abc2016 <- ds2$abc2017
ds2$abc2016[ds2$States == "Nevada"] <- 1

ds2$abc2015 <- ds2$abc2017
ds2$abc2015[ds2$States == "Wisconsin"] <- 0

ds2$abc2014 <- ds2$abc2017
ds2$abc2014[ds2$States == "Oregon"] <- 0

ds2$abc2013 <- ds2$abc2014


#Format year so we can subset it by year
library(lubridate)
ds$Year <- year(as.Date(ds$Date, format = "%Y - %b- %d"))

write.csv(ds, "ProblemSets/PS11/Data/main.csv")
write.csv(ds2, "ProblemSets/PS11/Data/statepop.csv")
