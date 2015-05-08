
### Removing Some Features Data



# data_model <- read.csv("ENFP_0719_HP.csv", header=TRUE)
# head(data_model)
# 
# row_sub <- apply(data_model,1,function(row) all(row !="location"))
# data_model <- data_model[row_sub,]


# Without Location
setwd("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_location\\model\\CSVs")

file_list_test <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_location\\test\\CSVs", full.names = TRUE)
file_list_model <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_location\\model\\CSVs", full.names = TRUE)


model_list <- file_list_model
test_list <- file_list_test

f_without_location <- function(path){
  data <- read.csv(path, header =TRUE)
  row_sub <- apply(data,1,function(row) all(row !="location"))
  new_data <- data[row_sub,]
  return (new_data)
}

for (model in model_list){
  data <- f_without_location(model)
  cat(sprintf("Storing data %s to csv file.......",model))
  cat("\n")
  new_path <- gsub('CSVs', 'CSV_without_location', model)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}

for (test in test_list){
  data <- f_without_location(test)
  cat(sprintf("Storing data %s to csv file.......",test))
  cat("\n")
  new_path <- gsub('CSVs', 'CSV_without_location', test)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}



#Without Wifi
setwd("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_wifi\\model\\CSVs")
file_list_test <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_wifi\\test\\CSVs_old", full.names = TRUE)
file_list_model <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_wifi\\model\\CSVs_old", full.names = TRUE)


model_list <- file_list_model
test_list <- file_list_test


f_without_wifi <- function(path){
  data <- read.csv(path, header =TRUE)
  row_sub <- apply(data,1,function(row) all(row !="wifi"))
  new_data <- data[row_sub,]
  return (new_data)
}

for (model in model_list){
  data <- f_without_wifi(model)
  cat(sprintf("Storing data %s to csv file.......",model))
  cat("\n")
  new_path <- gsub('CSVs_old', 'CSVs', model)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}

for (test in test_list){
  data <- f_without_wifi(test)
  cat(sprintf("Storing data %s to csv file.......",test))
  cat("\n")
  new_path <- gsub('CSVs_old', 'CSVs', test)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}


#Without Activity
setwd("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_activity\\model\\CSVs")
file_list_test <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_activity\\test\\CSVs_old", full.names = TRUE)
file_list_model <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_activity\\model\\CSVs_old", full.names = TRUE)


model_list <- file_list_model
test_list <- file_list_test

f_without_activity <- function(path){
  data <- read.csv(path, header =TRUE)
  row_sub <- apply(data,1,function(row) all(row !="activity"))
  new_data <- data[row_sub,]
  return (new_data)
}

for (model in model_list){
  data <- f_without_activity(model)
  cat(sprintf("Storing data %s to csv file.......",model))
  cat("\n")
  new_path <- gsub('CSVs_old', 'CSVs', model)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}

for (test in test_list){
  data <- f_without_activity(test)
  cat(sprintf("Storing data %s to csv file.......",test))
  cat("\n")
  new_path <- gsub('CSVs_old', 'CSVs', test)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}



#Without Battery
setwd("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_battery\\model\\CSVs")
file_list_test <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_battery\\test\\CSVs_old", full.names = TRUE)
file_list_model <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_battery\\model\\CSVs_old", full.names = TRUE)


model_list <- file_list_model
test_list <- file_list_test

f_without_battery <- function(path){
  data <- read.csv(path, header =TRUE)
  row_sub <- apply(data,1,function(row) all(row !="battery"))
  new_data <- data[row_sub,]
  return (new_data)
}

for (model in model_list){
  data <- f_without_battery(model)
  cat(sprintf("Storing data %s to csv file.......",model))
  cat("\n")
  new_path <- gsub('CSVs_old', 'CSVs', model)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}

for (test in test_list){
  data <- f_without_battery(test)
  cat(sprintf("Storing data %s to csv file.......",test))
  cat("\n")
  new_path <- gsub('CSVs_old', 'CSVs', test)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}


#Without Run Apps
setwd("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_apps\\model\\CSVs")
file_list_test <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_runapps\\test\\CSVs_old", full.names = TRUE)
file_list_model <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_runapps\\model\\CSVs_old", full.names = TRUE)


model_list <- file_list_model
test_list <- file_list_test

f_without_runapps <- function(path){
  data <- read.csv(path, header =TRUE)
  row_sub <- apply(data,1,function(row) all(row !="runapps"))
  new_data <- data[row_sub,]
  return (new_data)
}

for (model in model_list){
  data <- f_without_runapps(model)
  cat(sprintf("Storing data %s to csv file.......",model))
  cat("\n")
  new_path <- gsub('CSVs_old', 'CSVs', model)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}

for (test in test_list){
  data <- f_without_runapps(test)
  cat(sprintf("Storing data %s to csv file.......",test))
  cat("\n")
  new_path <- gsub('CSVs_old', 'CSVs', test)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}



#Without SMS
setwd("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_sms\\model\\CSVs")
file_list_test <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_sms\\test\\CSVs_old", full.names = TRUE)
file_list_model <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_sms\\model\\CSVs_old", full.names = TRUE)
model_list <- file_list_model
test_list <- file_list_test

f_without_sms <- function(path){
  data <- read.csv(path, header =TRUE)
  row_sub <- apply(data,1,function(row) all(row !="sms"))
  new_data <- data[row_sub,]
  return (new_data)
}

for (model in model_list){
  data <- f_without_sms(model)
  cat(sprintf("Storing data %s to csv file.......",model))
  cat("\n")
  new_path <- gsub('CSVs_old', 'CSVs', model)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}

for (test in test_list){
  data <- f_without_sms(test)
  cat(sprintf("Storing data %s to csv file.......",test))
  cat("\n")
  new_path <- gsub('CSVs_old', 'CSVs', test)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}


#Without CALL
setwd("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_call\\model\\CSVs")
file_list_test <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_call\\test\\CSVs_old", full.names = TRUE)
file_list_model <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_call\\model\\CSVs_old", full.names = TRUE)
model_list <- file_list_model
test_list <- file_list_test

f_without_call <- function(path){
  data <- read.csv(path, header =TRUE)
  row_sub <- apply(data,1,function(row) all(row !="call"))
  new_data <- data[row_sub,]
  return (new_data)
}

for (model in model_list){
  data <- f_without_call(model)
  cat(sprintf("Storing data %s to csv file.......",model))
  cat("\n")
  new_path <- gsub('CSVs_old', 'CSVs', model)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}

for (test in test_list){
  data <- f_without_call(test)
  cat(sprintf("Storing data %s to csv file.......",test))
  cat("\n")
  new_path <- gsub('CSVs_old', 'CSVs', test)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}


#Without Activity and CALL
setwd("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_activity_call\\model\\CSVs")
file_list_test <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_activity_call\\test\\CSVs_old", full.names = TRUE)
file_list_model <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_activity_call\\model\\CSVs_old", full.names = TRUE)
model_list <- file_list_model
test_list <- file_list_test

f_without_activity_call <- function(path){
  data <- read.csv(path, header =TRUE)
  row_sub <- apply(data,1,function(row) all((row !="call") & (row !="activity")))
  new_data <- data[row_sub,]
  return (new_data)
}

for (model in model_list){
  data <- f_without_activity_call(model)
  cat(sprintf("Storing data %s to csv file.......",model))
  cat("\n")
  new_path <- gsub('CSVs_old', 'CSVs', model)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}

for (test in test_list){
  data <- f_without_activity_call(test)
  cat(sprintf("Storing data %s to csv file.......",test))
  cat("\n")
  new_path <- gsub('CSVs_old', 'CSVs', test)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}



#Without Bluetooth and SMS
setwd("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_bluetooth_sms\\model\\CSVs")
file_list_test <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_bluetooth_sms\\test\\CSVs_old", full.names = TRUE)
file_list_model <- list.files("D:\\Dropbox\\thesis\\PROJECT\\data\\research-result\\without_bluetooth_sms\\model\\CSVs_old", full.names = TRUE)
model_list <- file_list_model
test_list <- file_list_test

f_without_without_bluetooth_sms <- function(path){
  data <- read.csv(path, header =TRUE)
  row_sub <- apply(data,1,function(row) all((row !="bluetooth") & (row !="sms")))
  new_data <- data[row_sub,]
  return (new_data)
}

for (model in model_list){
  data <- f_without_without_bluetooth_sms(model)
  cat(sprintf("Storing data %s to csv file.......",model))
  cat("\n")
  new_path <- gsub('CSVs_old', 'CSVs', model)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}

for (test in test_list){
  data <- f_without_without_bluetooth_sms(test)
  cat(sprintf("Storing data %s to csv file.......",test))
  cat("\n")
  new_path <- gsub('CSVs_old', 'CSVs', test)
  write.csv(data, sprintf("%s",new_path),row.names=FALSE)
}
