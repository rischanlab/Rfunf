### Author : Rischan Mafrur ###
### 01/01/2015 ###
### This code is for thesis purpose ###
### Unsupervised Activity Recognition based on unlabled data ###
### Lanugage : R language ####

#preprocessing 2



f_activity <- function(d_activity){
  d_activity <- d_activity[,c(3,2)]
  d_activity$type <- "activity"
  d_activity <- d_activity[,c(1,3,2)]
  names(d_activity) <- c("time","type","value1")
  d_activity$value2 <- ""
  d_activity$value3 <- ""
  d_activity$value1 <- as.character(d_activity$value1)
  d_activity$value2 <- as.character(d_activity$value2)
  d_activity$value3 <- as.character(d_activity$value3)
  d_activity$time <- as.character(d_activity$time)
  return (d_activity)

}

f_battery <- function(d_battery){
  d_battery <- d_battery[,c(6,5)]
  d_battery$type <- "battery"
  d_battery <- d_battery[,c(1,3,2)]
  names(d_battery) <- c("time","type","value1")
  d_battery$value2 <- ""
  d_battery$value3 <- ""
  d_battery$value1 <- as.character(d_battery$value1)
  d_battery$value2 <- as.character(d_battery$value2)
  d_battery$value3 <- as.character(d_battery$value3)
  d_battery$time <- as.character(d_battery$time)
  return (d_battery)
  
}

f_bluetooth <- function(d_bluetooth){
  d_bluetooth <- d_bluetooth[,c(4,2)]
  d_bluetooth$type <- "bluetooth"
  d_bluetooth <- d_bluetooth[,c(1,3,2)]
  names(d_bluetooth) <- c("time","type","value1")
  d_bluetooth$value2 <- ""
  d_bluetooth$value3 <- ""
  d_bluetooth$value1 <- as.character(d_bluetooth$value1)
  d_bluetooth$value2 <- as.character(d_bluetooth$value2)
  d_bluetooth$value3 <- as.character(d_bluetooth$value3)
  d_bluetooth$time <- as.character(d_bluetooth$time)
  #library(dplyr)
  #d_bluetooth <- d_bluetooth %>% group_by(time) %>% summarise(type=type[1], value2=value2[1], value3=value3[1], value1=toString(value1))
  #d_bluetooth <- d_bluetooth[c(1,2,5,3,4)]
  
  return (d_bluetooth)
  
}

f_call <- function(d_call){
  d_call <- d_call[,c(4,2,3,5)]
  names(d_call) <- c("time","value1","value2","value3")
  d_call$type <- "call"
  d_call <- d_call[,c(1,5,2,3,4)]
  d_call$value1 <- as.character(d_call$value1)
  d_call$value2 <- as.character(d_call$value2)
  d_call$value3 <- as.character(d_call$value3)
  d_call$time <- as.character(d_call$time)
  
  return (d_call)
  
}

f_location <- function(d_location){
  d_location <- d_location[,c(2,3,4)]
  
  #value1 : latitude, #value2 : longitude
  names(d_location) <-c("time","value1","value2")
  d_location$type <- "location"
  d_location <- d_location[,c(1,4,2,3)]
  d_location$value3 <- ""
  d_location$value1 <- as.character(d_location$value1)
  d_location$value2 <- as.character(d_location$value2)
  d_location$value3 <- as.character(d_location$value3)
  d_location$time <- as.character(d_location$time)
  mydf <- d_location
  mydf$value1 <- as.numeric(mydf$value1)
  mydf$value2 <- as.numeric(mydf$value2)
  
  library(dplyr)
  d_location <- select(mydf, -type, -value3) %>%
    mutate(movement = ifelse(value1 == lag(value1) & value2 == lag(value2), "same",
                             ifelse(between(abs(value1 - lag(value1)), 0.0001, 0.0005) == TRUE &
                                      between(abs(value2 - lag(value2)), 0.0001, 0.0005) == TRUE, "little", "long")))
  d_location <- na.omit(d_location)
  d_location <- d_location[,c(1,4,2,3)]
  names(d_location) <-c("time","value1","value2","value3")
  d_location$type <- "location"
  d_location <- d_location[,c(1,5,2,3,4)]
  return (d_location)
  
}


f_runapps <- function(d_runapps){
  d_runapps <- d_runapps[,c(2,4,3)]
  d_runapps$type <- "runapps"
  d_runapps <- d_runapps[,c(1,4,2,3)]
  names(d_runapps) <- c("time","type","value1","value2")
  d_runapps$value3 <- ""
  d_runapps$value1 <- as.character(d_runapps$value1)
  d_runapps$value2 <- as.character(d_runapps$value2)
  d_runapps$value3 <- as.character(d_runapps$value3)
  d_runapps$time <- as.character(d_runapps$time)
  
  return (d_runapps)
  
}


f_screen <- function(d_screen){
  d_screen <- d_screen[,c(2,3)]
  d_screen$type <- "screen"
  d_screen <- d_screen[,c(1,3,2)]
  names(d_screen) <- c("time","type","value1")
  d_screen$value2 <- ""
  d_screen$value3 <- ""
  d_screen$value1 <- as.character(d_screen$value1)
  d_screen$value2 <- as.character(d_screen$value2)
  d_screen$value3 <- as.character(d_screen$value3)
  d_screen$time <- as.character(d_screen$time)
  
  #library(dplyr)
  #Aggregate values in the same time (minutes)
  #d_screen <- d_screen %>% group_by(time) %>% summarise(type=type[1], value2=value2[1], value3=value3[1], value1=toString(value1))
  return (d_screen)
}

f_sms <- function(d_sms){
  d_sms <- d_sms[,c(2:5)]
  names(d_sms) <- c("time","value2","value1","value3")
  d_sms$type <- "sms"
  d_sms <- d_sms[,c(1,5,3,2,4)]
  d_sms$value1 <- as.character(d_sms$value1)
  d_sms$value2 <- as.character(d_sms$value2)
  d_sms$value3 <- as.character(d_sms$value3)
  d_sms$time <- as.character(d_sms$time)
  
  return (d_sms)
  
}


f_wifi <- function(d_wifi){
  d_wifi <- d_wifi[,c(2,4,3,7)]
  d_wifi$type <- "wifi"
  d_wifi <- d_wifi[,c(1,5,2,3,4)]
  names(d_wifi) <- c("time","type","value1","value2","value3")
  d_wifi$value1 <- as.character(d_wifi$value1)
  d_wifi$value2 <- as.character(d_wifi$value2)
  d_wifi$value3 <- as.character(d_wifi$value3)
  d_wifi$time <- as.character(d_wifi$time)
  #d_wifi <- d_wifi[-which(d_wifi$value1 == ""), ]
  #library(dplyr)
  #Aggregate values in the same time (minutes)
  #d_wifi1 <- d_wifi %>% group_by(d_wifi$time) %>% summarise(type=type[1], value2=value2[1], value3=value3[1], value1=toString(value1))
  #d_wifi <- d_wifi[c(1,2,5,3,4)]
  
  return (d_wifi)
  
}




f_preprocessing2 <- function(path){
  setwd(path)
  d_activity <- read.csv("d_activity.csv")
  d_battery <- read.csv("d_battery.csv")
  d_bluetooth <- read.csv("d_bluetooth.csv")
  d_call <- read.csv("d_call.csv")
  d_location <- read.csv("d_location.csv")
  d_runapps <- read.csv("d_runapps.csv")
  d_screen <- read.csv("d_screen.csv")
  #d_search <- read.csv("d_search.csv")
  d_sms <- read.csv("d_sms.csv")
  d_wifi <- read.csv("d_wifi.csv")
  
  
  df_activity <- f_activity(d_activity)
  df_battery <- f_battery(d_battery)
  df_bluetooth <- f_bluetooth(d_bluetooth)
  df_call <- f_call(d_call)
  df_location <- f_location(d_location)
  df_runapps <- f_runapps(d_runapps)
  df_screen <- f_screen(d_screen)
  df_sms <- f_sms(d_sms)
  df_wifi <- f_wifi(d_wifi)
  
  df_tmp <- rbind(df_activity,df_battery,df_bluetooth,df_call,df_location,df_runapps,df_screen,df_sms,df_wifi)
  
  df_tmp$time2 <- as.Date(df_tmp$time)
  new_df <- subset(df_tmp, df_tmp$time2 >= "2014-07-01" & df_tmp$time2 <= "2014-08-30")
  
  df <- new_df[,-6]
  df_sort <- df[order(df$time),]
  return (df_sort)
}



# 
# df_activity <- df_activity[order(df_activity$time),]
# df_battery <- df_battery[order(df_battery$time),]
# df_bluetooth <- df_bluetooth[order(df_bluetooth$time),]
# df_call <- df_call[order(df_call$time),]
# df_location <- df_location[order(df_location$time),]
# df_runapps <- df_runapps[order(df_runapps$time),]
# df_screen <- df_screen[order(df_screen$time),]
# df_sms <- df_sms[order(df_sms$time),]
# df_wifi <- df_wifi[order(df_wifi$time),]
# 
# 
# 
# 
# df_ac_batery <- rbind(df_activity,df_battery)
# df_add_bluetooth <- rbind(df_ac_batery,df_bluetooth)
# df_add_call <- rbind(df_add_bluetooth,df_call)
# df_add_loc <- rbind(df_add_call,df_location)
# df_add_runapps <- rbind(df_add_loc,df_runapps)
# df_add_screen <- rbind(df_add_runapps,df_screen)
# df_add_sms <- rbind(df_ac_batery,df_sms)
# d_add_wifi <- rbind(df_add_sms,df_wifi)
# 
# df <- d_add_wifi

# file_list <- list.files("D:/DATA/output", full.names = TRUE)
# 
# 
# ESTJ_3022 <- f_preprocessing2("D:/DATA/output/ESTJ_3022")
# write.csv(ESTJ_3022, "D:/DATA/output2/ESTJ_3022.csv")

# for (file in file_list){
#   #print(eval(sprintf("D:/DATA/output/%s/d_location.csv",file)))
#   dir.create(print(eval(sprintf("D:/DATA/output2/%s",file))))
# }
file_list <- list.files("D:/DATA/output", full.names = TRUE)
for (file in file_list){
  file_proc <- f_preprocessing2(file)
  cat(sprintf("Storing data %s to csv file.......",file))
  cat("\n")
  new_path <- gsub('output', 'output2', file)
  write.csv(file_proc, sprintf("%s.csv",new_path))
}








# View(df_wifi)
# 
# library(dplyr)
# #Aggregate values in the same time (minutes)
# new_bluetooth <- df_bluetooth %>% group_by(time) %>% summarise(type=type[1], value2=value2[1], value3=value3[1], value1=toString(value1))
# bluetooth <- new_bluetooth[c(1,2,5,3,4)]
# 
# 
# 



# 
# setwd("D:/DATA/output/ENFP_0719")
# d_activity <- read.csv("d_activity.csv")
# d_battery <- read.csv("d_battery.csv")
# d_bluetooth <- read.csv("d_bluetooth.csv")
# d_call <- read.csv("d_call.csv")
# d_location <- read.csv("d_location.csv")
# d_runapps <- read.csv("d_runapps.csv")
# d_screen <- read.csv("d_screen.csv")
# #d_search <- read.csv("d_search.csv")
# d_sms <- read.csv("d_sms.csv")
# d_wifi <- read.csv("d_wifi.csv")
# 
# df_activity <- f_activity(d_activity)
# df_battery <- f_battery(d_battery)
# df_bluetooth <- f_bluetooth(d_bluetooth)
# df_call <- f_call(d_call)
# df_location <- f_location(d_location)
# df_runapps <- f_runapps(d_runapps)
# df_screen <- f_screen(d_screen)
# df_sms <- f_sms(d_sms)
# df_wifi <- f_wifi(d_wifi)
# 
# df_tmp <- rbind(df_activity,df_battery,df_bluetooth,df_call,df_location,df_runapps,df_screen,df_sms,df_wifi)
# 
# df_tmp$time2 <- as.Date(df_tmp$time)
# new_df <- subset(df_tmp, df_tmp$time2 >= "2014-07-01" & df_tmp$time2 <= "2014-08-30")
# 
# df <- new_df[,-6]
# df_sort <- df[order(df$time),]
# 
# head(df_sort)
# tail(df_sort)
# 
# 
# View(df_screen)

         
# # 
# # 
# # 
# # 
# # 
# # 
# file_proc <- f_preprocessing2("D:/DATA/output/ESTJ_3022")
# 
# write.csv(file_proc, "D:/DATA/output2/data.csv")


# setwd("D:/DATA/output/ENFP_0719")
# d_activity <- read.csv("d_activity.csv")
# d_battery <- read.csv("d_battery.csv")
# d_bluetooth <- read.csv("d_bluetooth.csv")
# d_call <- read.csv("d_call.csv")
# d_location <- read.csv("d_location.csv")
# d_runapps <- read.csv("d_runapps.csv")
# d_screen <- read.csv("d_screen.csv")
# #d_search <- read.csv("d_search.csv")
# d_sms <- read.csv("d_sms.csv")
# d_wifi <- read.csv("d_wifi.csv")
# 
# 
# df_activity <- f_activity(d_activity)
# df_battery <- f_battery(d_battery)
# df_bluetooth <- f_bluetooth(d_bluetooth)
# df_call <- f_call(d_call)
# df_location <- f_location(d_location)
# df_runapps <- f_runapps(d_runapps)
# df_screen <- f_screen(d_screen)
# df_sms <- f_sms(d_sms)
# df_wifi <- f_wifi(d_wifi)
# 
# # 
# # head(df_activity)
# # head(df_battery)
# # head(df_bluetooth)
# # head(df_call)
# # head(df_location)
# # head(df_runapps)
# # head(df_screen)
# # head(df_sms)
# # head(df_wifi)
# # 
# # 
# # tail(df_activity)
# # tail(df_battery)
# # tail(df_bluetooth)
# # tail(df_call)
# # tail(df_location)
# # tail(df_runapps)
# # tail(df_screen)
# # tail(df_sms)
# # tail(df_wifi)
# 
# 
# 
# df_activity <- df_activity[order(df_activity$time),]
# head(df_activity)
# df_battery <- df_battery[order(df_battery$time),]
# head(df_battery)
# df_bluetooth <- df_bluetooth[order(df_bluetooth$time),]
# head(df_bluetooth)
# df_call <- df_call[order(df_call$time),]
# head(df_call)
# df_location <- df_location[order(df_location$time),]
# head(df_location)
# df_runapps <- df_runapps[order(df_runapps$time),]
# head(df_runapps)
# df_screen <- df_screen[order(df_screen$time),]
# head(df_screen)
# df_sms <- df_sms[order(df_sms$time),]
# head(df_sms)
# df_wifi <- df_wifi[order(df_wifi$time),]
# head(df_wifi)
# 
# 
# 
# df_ac_batery <- rbind(df_activity,df_battery)
# df_add_bluetooth <- rbind(df_ac_batery,df_bluetooth)
# df_add_call <- rbind(df_add_bluetooth,df_call)
# df_add_loc <- rbind(df_add_call,df_location)
# df_add_runapps <- rbind(df_add_loc,df_runapps)
# df_add_screen <- rbind(df_add_runapps,df_screen)
# df_add_sms <- rbind(df_ac_batery,df_sms)
# d_add_wifi <- rbind(df_add_sms,df_wifi)
# 
# df <- d_add_wifi
# #df$time <- as.POSIXct(df$time, format="%Y-%m-%d %H:%M:%S")
# df_sort <- df[order(df$time),]



