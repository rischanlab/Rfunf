
### Author Information #####
### Coded by Rischan Mafrur ###
### 01/01/2015 ###
### For Thesis Purpose ###
### Language : R ###
### Dataset : Private Data ###

setwd("D:/DATA/output2")
file_list <- list.files()

#install.packages("tidyr")

library(data.table)
library(tidyr)
library(ggplot2)
library(scales)


test = fread("ENFP_0719.csv")  # the data file is in working directory
test$time2 <- as.Date(test$time)
test <- subset(test, test$time2 >= "2014-07-01" & test$time2 <= "2014-07-05")

test$type[test$value1=="high"] <- "high activity"
test$type[test$value1=="low"] <- "low activity"
test$type[test$value1=="TRUE"] <- "screen ON"
test$type[test$value1=="charging"] <- "charging"

head(test)

test <- test[test$type != "wifi"]
test <- test[test$type != "location"]
test <- test[test$type != "bluetooth"]
test <- test[test$type != "runapps"]
test <- test[test$type != "activity"]
test <- test[test$type != "battery"]
test <- test[test$type != "screen"]

test = separate(test, time, c("days","time"), sep=" ")

test$days = as.POSIXct(strptime(test$days, "%Y-%m-%d"))
test$time = as.POSIXct(strptime(test$time, "%H:%M"))

# to plot
ggplot(test, aes(x=time, y=type, colour=type, shape=type)) +
  theme_bw() + 
  geom_point() + 
  facet_grid(days ~.) + 
  scale_x_datetime(breaks=date_breaks("1 hour"), labels = date_format("%H:%M"))



head(test)





# dataku <- structure(list(DataValue = c(0, 0, 0, 0, 0, 0, 0, 0, 0, NA, NA, 
#                              NA, NA, NA, NA, NA, NA, 0, 0, 0), SiteID = c(1, 1, 1, 1, 1, 1, 
#                                                                           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), VariableCode = c("Sucho", 
#                                                                                                                                       "Sucho", "Sucho", "Sucho", "Sucho", "Sucho", "Sucho", "Sucho", 
#                                                                                                                                       "Sucho", "Sucho", "Sucho", "Sucho", "Sucho", "Sucho", "Sucho", 
#                                                                                                                                       "Sucho", "Sucho", "Sucho", "Sucho", "Sucho"), DateTimeUTC = structure(c(15522, 
#                                                                                                                                                                                                               15523, 15524, 15525, 15526, 15527, 15528, 15529, 15530, 15531, 
#                                                                                                                                                                                                               15532, 15533, 15534, 15535, 15536, 15537, 15538, 15539, 15540, 
#                                                                                                                                                                                                               15541), class = "Date"), Latitude = c(50.77, 50.77, 50.77, 50.77, 
#                                                                                                                                                                                                                                                     50.77, 50.77, 50.77, 50.77, 50.77, 50.77, 50.77, 50.77, 50.77, 
#                                                                                                                                                                                                                                                     50.77, 50.77, 50.77, 50.77, 50.77, 50.77, 50.77), Longitude = c(15.55, 
#                                                                                                                                                                                                                                                                                                                     15.55, 15.55, 15.55, 15.55, 15.55, 15.55, 15.55, 15.55, 15.55, 
#                                                                                                                                                                                                                                                                                                                     15.55, 15.55, 15.55, 15.55, 15.55, 15.55, 15.55, 15.55, 15.55, 
#                                                                                                                                                                                                                                                                                                                     15.55)), .Names = c("DataValue", "SiteID", "VariableCode", "DateTimeUTC", 
#                                                                                                                                                                                                                                                                                                                                  "Latitude", "Longitude"), row.names = c(NA, 20L), class = "data.frame")
# data<- dataku
# 
# startDate = as.POSIXct("2012-07-01");
# endDate = as.POSIXct("2012-07-20");
# all_dates = seq(startDate, endDate, 86400); #86400 is num of seconds in a day
# 
# #the following code I'm trying to run inside a loop...
# for (j in 1:length(all_dates)) {
#   filterdate = all_dates[j];
#   my_subset = data[data$DateTimeUTC == filterdate,]
#   #now I want do do some processing on my_subset...
# }