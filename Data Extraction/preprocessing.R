### Author Information #####
### Coded by Rischan Mafrur ###
### 01/01/2015 ###
### For Thesis Purpose ###
### Language : R ###
### Dataset : Private Data ###

#Function for loading data
f_load_data <- function(path){
  library(shiny)
  library("RSQLite")
  library("rjson")
  library("ggplot2")
  library("scales")
  library("ggmap")
  library("rmarkdown")
  library("rmarkdown")
  
  setwd(path)
  driver <- dbDriver("SQLite")
  
  file_list <- list.files()
  
  for (file in file_list){
    
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      con <- dbConnect(driver,file)
      dataset <- dbGetQuery(con,"Select * from data")
    }
    
    # if the merged dataset does exist, append to it
    if (exists("dataset")){
      con <- dbConnect(driver,file)
      temp_dataset <-dbGetQuery(con,"Select * from data")
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
    
  }
  
  return (dataset)
}


#removing duplicate data
remove_duplicate <- function(data){
  return (data[!duplicated(data),])
}


#function Activity data Processing
f_activity_data <- function(dataset){
  dActivityProbe  <- subset(dataset, dataset$name=="ActivityProbe")
  #head(dActivityProbe)
  myData <- lapply(dActivityProbe$value, fromJSON)
  dActivityProbe$value <- as.character(do.call(rbind,lapply(myData, `[` ,'activityLevel')))   
  dActivityProbe$time <- do.call(rbind,lapply(myData, `[` ,'timestamp')) 
  dActivityProbe$time <- as.character(as.POSIXlt(as.numeric(dActivityProbe$time), origin="1970-01-01", tz = "GMT"))
  dA <- cbind(dActivityProbe$value,dActivityProbe$time)
  dActivityProbe <- as.data.frame(dA)
  names(dActivityProbe) <- c("Activity","Time")
  return (dActivityProbe)
}

#dunction for load apps data
f_apps_data <- function(dataset){
  dApplicationsProbe <- subset(dataset, dataset$name=="ApplicationsProbe")
  #head(dApplicationsProbe)
  myData <- lapply(dApplicationsProbe$value, fromJSON)
  dApplicationsProbe$value <- as.character(do.call(rbind,lapply(myData, `[` ,'publicSourceDir')))   
  dApplicationsProbe$time <- do.call(rbind,lapply(myData, `[` ,'timestamp'))   
  dApplicationsProbe$time <- as.character(as.POSIXlt(as.numeric(dApplicationsProbe$time), origin="1970-01-01", tz = "GMT"))
  dA <- cbind(dApplicationsProbe$value,dApplicationsProbe$time)
  dApplicationsProbe <- as.data.frame(dA)
  names(dApplicationsProbe) <- c("Application","Time")
  return (dApplicationsProbe)
}

#function for processing bluetooth data
f_bluetooth_data <- function(dataset){
  dBluetoothProbe <- subset(dataset, dataset$name=="BluetoothProbe")
  #head(dBluetoothProbe)
  myData <- lapply(dBluetoothProbe$value, fromJSON)  
  dBluetoothProbe$device <- as.character(do.call(rbind,lapply(myData, `[` ,'android.bluetooth.device.extra.NAME')))  
  dBluetoothProbe$RSSI <- as.character(do.call(rbind,lapply(myData, `[` ,'android.bluetooth.device.extra.RSSI')))  
  dBluetoothProbe$time <- do.call(rbind,lapply(myData, `[` ,'timestamp'))   
  dBluetoothProbe$time <- as.character(as.POSIXlt(as.numeric(dBluetoothProbe$time), origin="1970-01-01", tz = "GMT"))
  dA <- cbind(dBluetoothProbe$device,dBluetoothProbe$RSSI, dBluetoothProbe$time)
  dBluetoothProbe <- as.data.frame(dA)
  names(dBluetoothProbe) <- c("BluetoothName","RSSI","Time")
  #return (dBluetoothProbe)
  return (dBluetoothProbe)
}


#function for processing battery data
f_battery_data <- function(dataset){
  dBatteryProbe <- subset(dataset, dataset$name=="BatteryProbe")
  #head(dBatteryProbe)
  myData <- lapply(dBatteryProbe$value, fromJSON)  
  dBatteryProbe$charge_type <- as.character(do.call(rbind,lapply(myData, `[` ,'charge_type')))
  dBatteryProbe$charge_type[dBatteryProbe$charge_type=="0"] <- "AC"
  dBatteryProbe$charge_type[dBatteryProbe$charge_type=="1"] <- "USB"
  dBatteryProbe$charge_type[dBatteryProbe$charge_type=="4"] <- "Wireless"
  dBatteryProbe$health <- as.character(do.call(rbind,lapply(myData, `[` ,'health'))) 
  dBatteryProbe$health[dBatteryProbe$health=="1"] <- "unknown"
  dBatteryProbe$health[dBatteryProbe$health=="2"] <- "good"
  dBatteryProbe$health[dBatteryProbe$health=="3"] <- "overheat"
  dBatteryProbe$health[dBatteryProbe$health=="4"] <- "dead"
  dBatteryProbe$health[dBatteryProbe$health=="5"] <- "over_voltage"
  dBatteryProbe$health[dBatteryProbe$health=="6"] <- "unspecified_failure"
  dBatteryProbe$health[dBatteryProbe$health=="7"] <- "cold"
  dBatteryProbe$plugged <- as.character(do.call(rbind,lapply(myData, `[` ,'plugged'))) 
  dBatteryProbe$plugged[dBatteryProbe$plugged=="0"] <- "on_battery"
  dBatteryProbe$plugged[dBatteryProbe$plugged=="2"] <- "power_source"
  dBatteryProbe$status <- as.character(do.call(rbind,lapply(myData, `[` ,'status')))  
  dBatteryProbe$status[dBatteryProbe$status=="1"] <- "unknown"
  dBatteryProbe$status[dBatteryProbe$status=="2"] <- "charging"
  dBatteryProbe$status[dBatteryProbe$status=="3"] <- "discharging"
  dBatteryProbe$status[dBatteryProbe$status=="4"] <- "not_charging"
  dBatteryProbe$status[dBatteryProbe$status=="5"] <- "full"
  dBatteryProbe$time <- do.call(rbind,lapply(myData, `[` ,'timestamp'))
  dBatteryProbe$time <- as.character(as.POSIXlt(as.numeric(dBatteryProbe$time), origin="1970-01-01", tz = "GMT"))
  dA <- cbind(dBatteryProbe$charge_type,dBatteryProbe$health,dBatteryProbe$plugged,dBatteryProbe$status,dBatteryProbe$time)
  dBatteryProbe <- as.data.frame(dA)
  names(dBatteryProbe) <- c("charge_type","health","plugged","status","time")
  
  return (dBatteryProbe)
}

#function for processing call data
f_call_data <- function(dataset){
  dCallLogProbe <- subset(dataset, dataset$name=="CallLogProbe")
  #dCallLogProbe
  #head(dCallLogProbe)
  myData <- lapply(dCallLogProbe$value, fromJSON)  
  dCallLogProbe$date <- do.call(rbind,lapply(myData, `[` ,'date')) 
  dCallLogProbe$time_calling <- as.character(as.POSIXlt(as.numeric(substr(dCallLogProbe$date, 1, 10)), origin="1970-01-01", tz = "GMT"))
  dCallLogProbe$duration <- as.character(do.call(rbind,lapply(myData, `[` ,'duration'))) 
  dCallLogProbe$number <- substr(gsub("^.*:","",as.character(do.call(rbind,lapply(myData, `[` ,'number')))),2,41)
  dCallLogProbe$type <- as.character(do.call(rbind,lapply(myData, `[` ,'type')))  
  dCallLogProbe$type[dCallLogProbe$type=="1"] <- "incoming"
  dCallLogProbe$type[dCallLogProbe$type=="2"] <- "outgoing"
  dCallLogProbe$type[dCallLogProbe$type=="3"] <- "missed"
  dA <- cbind(dCallLogProbe$number, dCallLogProbe$type,dCallLogProbe$time_calling,dCallLogProbe$duration)
  dCallLogProbe <- as.data.frame(dA)
  names(dCallLogProbe) <- c("number","type","time","duration")
  return (dCallLogProbe)
}

#function for processing sms data
f_sms_data <- function(dataset){
  dSmsProbe <- subset(dataset, dataset$name=="SmsProbe")
  #dSmsProbe
  head(dSmsProbe)
  myData <- lapply(dSmsProbe$value, fromJSON)  
  dSmsProbe$date <- do.call(rbind,lapply(myData, `[` ,'date')) 
  dSmsProbe$time <- as.character(as.POSIXlt(as.numeric(substr(dSmsProbe$date, 1, 10)), origin="1970-01-01", tz = "GMT"))
  dSmsProbe$address <- as.character(do.call(rbind,lapply(myData, `[` ,'address')))  
  dSmsProbe$type <- as.character(do.call(rbind,lapply(myData, `[` ,'type')))  
  dSmsProbe$type[dSmsProbe$type=="0"] <- "all"
  dSmsProbe$type[dSmsProbe$type=="1"] <- "inbox"
  dSmsProbe$type[dSmsProbe$type=="2"] <- "sent"
  dSmsProbe$type[dSmsProbe$type=="3"] <- "draft"
  dSmsProbe$type[dSmsProbe$type=="4"] <- "outbox"
  dSmsProbe$type[dSmsProbe$type=="5"] <- "failed"
  dSmsProbe$type[dSmsProbe$type=="6"] <- "queued"
  dSmsProbe$status <- as.character(do.call(rbind,lapply(myData, `[` ,'status'))) 
  dSmsProbe$status[dSmsProbe$status=="-1"] <- "none"
  dSmsProbe$status[dSmsProbe$status=="0"] <- "complete"
  dSmsProbe$status[dSmsProbe$status=="32"] <- "pending"
  dSmsProbe$status[dSmsProbe$status=="64"] <- "failed"
  # dSmsProbe$body-byte-len <- as.character(do.call(rbind,lapply(myData, `[` ,'body-byte-len')))  
  # dSmsProbe$body-token-byte-len <- as.character(do.call(rbind,lapply(myData, `[` ,'body-token-byte-len')))  
  # dSmsProbe$body-token-count <- as.character(do.call(rbind,lapply(myData, `[` ,'body-token-count')))  
  dA <- cbind(dSmsProbe$time, dSmsProbe$type, dSmsProbe$address,dSmsProbe$status)
  dSmsProbe <- as.data.frame(dA)
  names(dSmsProbe) <- c("when", "type","address","status")
  return (dSmsProbe)
  
}

#function for processing contact data
# f_contact_data <- function(dataset){
#   dContactProbe <- subset(dataset, dataset$name=="ContactProbe")
#   dContactProbe <- subset(dataset, dataset$name=="ContactProbe")
#   myData <- lapply(dContactProbe$value, fromJSON)  
#   dContactProbe$date <- do.call(rbind,lapply(myData, `[` ,'date')) 
#   dContactProbe$time_calling <- as.character(as.POSIXlt(as.numeric(substr(dContactProbe$date, 1, 10)), origin="1970-01-01", tz = "GMT"))
#   dContactProbe$duration <- as.character(do.call(rbind,lapply(myData, `[` ,'duration'))) 
#   dContactProbe$number <- as.character(do.call(rbind,lapply(myData, `[` ,'number')))  
#   dContactProbe$type <- as.character(do.call(rbind,lapply(myData, `[` ,'type')))  
#   dContactProbe$type[dContactProbe$type=="1"] <- "incoming"
#   dContactProbe$type[dContactProbe$type=="2"] <- "outgoing"
#   dContactProbe$type[dContactProbe$type=="3"] <- "missed"
#   dA <- cbind(dContactProbe$type, dContactProbe$number,dContactProbe$time_calling,dContactProbe$duration)
#   dContactProbe <- as.data.frame(dA)
#   names(dContactProbe) <- c("type","number","time","duration")
#   summary(dContactProbe)
#   return (dContactProbe)
#   
# }

#function for processing hardware information data
f_hardware_data <- function(dataset){
  dHardwareInfoProbe <- subset(dataset, dataset$name=="HardwareInfoProbe")
  #head(dHardwareInfoProbe)
  myData <- lapply(dHardwareInfoProbe$value, fromJSON) 
  dHardwareInfoProbe$brand <- as.character(do.call(rbind,lapply(myData, `[` ,'brand')))
  dHardwareInfoProbe$model <- as.character(do.call(rbind,lapply(myData, `[` ,'model')))
  dHardwareInfoProbe$bluetoothMac <- as.character(do.call(rbind,lapply(myData, `[` ,'bluetoothMac')))
  dHardwareInfoProbe$wifiMac <- as.character(do.call(rbind,lapply(myData, `[` ,'wifiMac')))
  dA <- cbind(dHardwareInfoProbe$brand,dHardwareInfoProbe$model,dHardwareInfoProbe$bluetoothMac,dHardwareInfoProbe$wifiMac)
  dHardwareInfoProbe <- as.data.frame(dA)
  names(dHardwareInfoProbe) <- c("brand","model","bluetoothMac", "wifiMac")
  return (dHardwareInfoProbe)
  
}

#function for processing bookmark data
f_bookmark_data <- function(dataset){
  dBrowserBookmarksProbe <- subset(dataset, dataset$name=="BrowserBookmarksProbe")
  #head(dBrowserBookmarksProbe)
  myData <- lapply(dBrowserBookmarksProbe$value, fromJSON) 
  dBrowserBookmarksProbe$time <- do.call(rbind,lapply(myData, `[` ,'created'))  
  dBrowserBookmarksProbe$when <- as.character(as.POSIXlt(as.numeric(dBrowserBookmarksProbe$time), origin="1970-01-01", tz = "GMT"))
  dBrowserBookmarksProbe$title <- as.character(do.call(rbind,lapply(myData, `[` ,'title')))
  dBrowserBookmarksProbe$url <- as.character(do.call(rbind,lapply(myData, `[` ,'url')))
  dA <- cbind(dBrowserBookmarksProbe$when,dBrowserBookmarksProbe$title, dBrowserBookmarksProbe$url)
  dBrowserBookmarksProbe <- as.data.frame(dA)
  names(dBrowserBookmarksProbe) <- c("created","title","url")
  return (dBrowserBookmarksProbe)
  
  
}

#function for processing searching log data
f_search_data <- function(dataset){
  dBrowserSearchesProbe <- subset(dataset, dataset$name=="BrowserSearchesProbe")
  #head(dBrowserSearchesProbe)
  myData <- lapply(dBrowserSearchesProbe$value, fromJSON) 
  dBrowserSearchesProbe$time <- do.call(rbind,lapply(myData, `[` ,'date'))  
  dBrowserSearchesProbe$when <- as.character(as.POSIXlt(as.numeric(dBrowserSearchesProbe$time), origin="1970-01-01", tz = "GMT"))
  dBrowserSearchesProbe$query <- as.character(do.call(rbind,lapply(myData, `[` ,'search')))
  dA <- cbind(dBrowserSearchesProbe$when,dBrowserSearchesProbe$query)
  dBrowserSearchesProbe <- as.data.frame(dA)
  names(dBrowserSearchesProbe) <- c("when","query")
  return (dBrowserSearchesProbe)
  
}

#function for processing light sensors data
f_light_data <- function(dataset){
  dLightSensorProbe <- subset(dataset, dataset$name=="LightSensorProbe")
  #head(dLightSensorProbe)
  myData <- lapply(dLightSensorProbe$value, fromJSON) 
  dLightSensorProbe$time <- do.call(rbind,lapply(myData, `[` ,'timestamp'))  
  dLightSensorProbe$timestamp <- as.character(as.POSIXlt(as.numeric(dLightSensorProbe$time), origin="1970-01-01", tz = "GMT"))
  dLightSensorProbe$accuracy <- as.character(do.call(rbind,lapply(myData, `[` ,'accuracy')))
  dLightSensorProbe$lux <- as.character(do.call(rbind,lapply(myData, `[` ,'lux')))
  dA <- cbind(dLightSensorProbe$timestamp,dLightSensorProbe$accuracy, dLightSensorProbe$lux)
  dLightSensorProbe <- as.data.frame(dA)
  names(dLightSensorProbe) <- c("timestamp","accuracy","lux")
  return (dLightSensorProbe)
}

#function for processing magnetic field sensor data
f_magno_data <- function(dataset){
  dMagneticFieldSensorProbe <- subset(dataset, dataset$name=="MagneticFieldSensorProbe")
  #head(dMagneticFieldSensorProbe)
  myData <- lapply(dMagneticFieldSensorProbe$value, fromJSON) 
  dMagneticFieldSensorProbe$time <- do.call(rbind,lapply(myData, `[` ,'timestamp'))  
  dMagneticFieldSensorProbe$timestamp <- as.character(as.POSIXlt(as.numeric(dMagneticFieldSensorProbe$time), origin="1970-01-01", tz = "GMT"))
  dMagneticFieldSensorProbe$accuracy <- as.character(do.call(rbind,lapply(myData, `[` ,'accuracy')))
  dMagneticFieldSensorProbe$accuracy[dMagneticFieldSensorProbe$accuracy=="1"] <- "low"
  dMagneticFieldSensorProbe$accuracy[dMagneticFieldSensorProbe$accuracy=="2"] <- "medium"
  dMagneticFieldSensorProbe$accuracy[dMagneticFieldSensorProbe$accuracy=="3"] <- "high"
  dMagneticFieldSensorProbe$x <- as.character(do.call(rbind,lapply(myData, `[` ,'x')))
  dMagneticFieldSensorProbe$y <- as.character(do.call(rbind,lapply(myData, `[` ,'y')))
  dMagneticFieldSensorProbe$z <- as.character(do.call(rbind,lapply(myData, `[` ,'z')))
  dA <- cbind(dMagneticFieldSensorProbe$timestamp,dMagneticFieldSensorProbe$accuracy, dMagneticFieldSensorProbe$x,dMagneticFieldSensorProbe$y, dMagneticFieldSensorProbe$z)
  dMagneticFieldSensorProbe <- as.data.frame(dA)
  names(dMagneticFieldSensorProbe) <- c("timestamp","accuracy","x","y","z")
  return (dMagneticFieldSensorProbe)
  
}

#function for processing preassure data
f_pressure_data <- function(dataset){
  dPressureSensorProbe <- subset(dataset, dataset$name=="PressureSensorProbe")
  #head(dPressureSensorProbe)
  myData <- lapply(dPressureSensorProbe$value, fromJSON)
  dPressureSensorProbe$time <- do.call(rbind,lapply(myData, `[` ,'timestamp'))  
  dPressureSensorProbe$timestamp <- as.character(as.POSIXlt(as.numeric(dPressureSensorProbe$time), origin="1970-01-01", tz = "GMT"))
  dPressureSensorProbe$accuracy <- as.character(do.call(rbind,lapply(myData, `[` ,'accuracy')))
  dPressureSensorProbe$pressure <- as.character(do.call(rbind,lapply(myData, `[` ,'pressure')))
  dA <- cbind(dPressureSensorProbe$timestamp,dPressureSensorProbe$accuracy, dPressureSensorProbe$pressure)
  dPressureSensorProbe <- as.data.frame(dA)
  names(dPressureSensorProbe) <- c("timestamp","accuracy","pressure")
  return (dPressureSensorProbe)
  
}

#function for processing proximity data
f_proximity_data <- function(dataset){
  dProximitySensorProbe <- subset(dataset, dataset$name=="ProximitySensorProbe")
  #head(dProximitySensorProbe)
  myData <- lapply(dProximitySensorProbe$value, fromJSON)
  dProximitySensorProbe$time <- do.call(rbind,lapply(myData, `[` ,'timestamp'))  
  dProximitySensorProbe$timestamp <- as.character(as.POSIXlt(as.numeric(dProximitySensorProbe$time), origin="1970-01-01", tz = "GMT"))
  dProximitySensorProbe$accuracy <- as.character(do.call(rbind,lapply(myData, `[` ,'accuracy')))
  dProximitySensorProbe$distance <- as.character(do.call(rbind,lapply(myData, `[` ,'distance')))
  dA <- cbind(dProximitySensorProbe$timestamp,dProximitySensorProbe$accuracy, dProximitySensorProbe$distance)
  dProximitySensorProbe <- as.data.frame(dA)
  names(dProximitySensorProbe) <- c("timestamp","accuracy","distance")
  return (dProximitySensorProbe)
  
}

#function for processing running application data
f_runapps_data <- function(dataset){
  dRunningApplicationsProbe <- subset(dataset, dataset$name=="RunningApplicationsProbe")
  #head(dRunningApplicationsProbe)
  myData <- lapply(dRunningApplicationsProbe$value, fromJSON)
  
  dRunningApplicationsProbe$time <- do.call(rbind,lapply(myData, `[` ,'timestamp'))  
  dRunningApplicationsProbe$timestamp <- as.character(as.POSIXlt(as.numeric(dRunningApplicationsProbe$time), origin="1970-01-01", tz = "GMT"))
  dRunningApplicationsProbe$duration <- as.character(do.call(rbind,lapply(myData, function(x) x$duration)))
  dRunningApplicationsProbe$package <- as.character(do.call(rbind,lapply(myData, function(x) x$taskInfo$baseIntent$mComponent$mPackage)))
  dA <- cbind(dRunningApplicationsProbe$timestamp, dRunningApplicationsProbe$duration,dRunningApplicationsProbe$package)
  dRunningApplicationsProbe <- as.data.frame(dA)
  names(dRunningApplicationsProbe) <- c("timestamp","duration","package")
  return (dRunningApplicationsProbe)
  
}

#function for processing screend data
f_screen_data <- function(dataset){
  dScreenProbe <- subset(dataset, dataset$name=="ScreenProbe")
  #head(dScreenProbe)
  myData <- lapply(dScreenProbe$value, fromJSON)
  dScreenProbe$time <- do.call(rbind,lapply(myData, `[` ,'timestamp'))  
  dScreenProbe$timestamp <- as.character(as.POSIXlt(as.numeric(dScreenProbe$time), origin="1970-01-01", tz = "GMT"))
  dScreenProbe$screenOn <- as.character(do.call(rbind,lapply(myData, `[` ,'screenOn')))
  dA <- cbind(dScreenProbe$timestamp, dScreenProbe$screenOn)
  dScreenProbe <- as.data.frame(dA)
  names(dScreenProbe) <- c("timestamp","screenON")
  return (dScreenProbe)
}

#function for processing location data
f_location_data <- function(dataset){
  dSimpleLocationProbe <- subset(dataset, dataset$name=="SimpleLocationProbe")

  #dSimpleLocationProbe
  #head(dSimpleLocationProbe)
  # location <-dSimpleLocationProbe
  # location$latitude <- as.numeric(as.character(dSimpleLocationProbe$latitude))
  # location$longitude <- as.numeric(as.character(dSimpleLocationProbe$longitude))
  # 
  
  myData <- lapply(dSimpleLocationProbe$value, fromJSON)  
  dSimpleLocationProbe$Latitude <- as.character(do.call(rbind,lapply(myData, `[` ,'mLatitude')))  
  dSimpleLocationProbe$Longitude <- as.character(do.call(rbind,lapply(myData, `[` ,'mLongitude')))  
  dSimpleLocationProbe$time <- do.call(rbind,lapply(myData, `[` ,'timestamp'))  
  dSimpleLocationProbe$when <- as.character(as.POSIXlt(as.numeric(dSimpleLocationProbe$time), origin="1970-01-01", tz = "GMT"))
  dA <- cbind(dSimpleLocationProbe$when, dSimpleLocationProbe$Latitude , dSimpleLocationProbe$Longitude)
  dSimpleLocationProbe <- as.data.frame(dA)
  names(dSimpleLocationProbe) <- c("when","latitude","longitude")
  return (dSimpleLocationProbe)
  
  
}


#function for processing wifi data
f_wifi_data <- function(dataset){
  dWifiProbe <- subset(dataset, dataset$name=="WifiProbe")
  #head(dWifiProbe)
  myData <- lapply(dWifiProbe$value, fromJSON)
  dWifiProbe$time <- do.call(rbind,lapply(myData, `[` ,'timestamp'))  
  dWifiProbe$timestamp <- as.character(as.POSIXlt(as.numeric(dWifiProbe$time), origin="1970-01-01", tz = "GMT"))
  dWifiProbe$BSSID <- as.character(do.call(rbind,lapply(myData, `[` ,'BSSID')))
  dWifiProbe$SSID <- as.character(do.call(rbind,lapply(myData, `[` ,'SSID')))
  dWifiProbe$capabilities <- as.character(do.call(rbind,lapply(myData, `[` ,'capabilities')))
  dWifiProbe$frequency <- as.character(do.call(rbind,lapply(myData, `[` ,'frequency')))
  dWifiProbe$level <- as.character(do.call(rbind,lapply(myData, `[` ,'level')))
  dA <- cbind(dWifiProbe$timestamp, dWifiProbe$BSSID,dWifiProbe$SSID,dWifiProbe$capabilities,dWifiProbe$frequency,dWifiProbe$level)
  dWifiProbe <- as.data.frame(dA)
  names(dWifiProbe) <- c("timestamp","BSSID","SSID","capabilities","frequency","level")
  return (dWifiProbe)
  
}

# 
# 
# raw_data <- f_load_data(print(eval(sprintf("D:/DATA/raw/ENTJ_6454/edu.mit.media.funf.wifiscanner/default/archive",file))))
# #removing duplicate data
# dataset <- remove_duplicate(raw_data)
# 
# 
# dWifiProbe <- subset(dataset, dataset$name=="WifiProbe")
# head(dWifiProbe)
# myData <- lapply(dWifiProbe$value, fromJSON)
# dWifiProbe$time <- do.call(rbind,lapply(myData, `[` ,'timestamp'))  
# dWifiProbe$timestamp <- as.character(dWifiProbe$time)
# dWifiProbe$BSSID <- as.character(do.call(rbind,lapply(myData, `[` ,'BSSID')))
# dWifiProbe$SSID <- as.character(do.call(rbind,lapply(myData, `[` ,'SSID')))
# dWifiProbe$capabilities <- as.character(do.call(rbind,lapply(myData, `[` ,'capabilities')))
# dWifiProbe$frequency <- as.character(do.call(rbind,lapply(myData, `[` ,'frequency')))
# dWifiProbe$level <- as.character(do.call(rbind,lapply(myData, `[` ,'level')))
# dA <- cbind(dWifiProbe$timestamp, dWifiProbe$BSSID,dWifiProbe$SSID,dWifiProbe$capabilities,dWifiProbe$frequency,dWifiProbe$level)
# dWifiProbe <- as.data.frame(dA)
# names(dWifiProbe) <- c("timestamp","BSSID","SSID","capabilities","frequency","level")
# 
# #function for processing call data
# 
#   dCallLogProbe <- subset(dataset, dataset$name=="CallLogProbe")
#   #dCallLogProbe
#   head(dCallLogProbe)
#   tail(dCallLogProbe)
#  
#   myData <- lapply(dCallLogProbe$value, fromJSON)  
#   dCallLogProbe$date <- do.call(rbind,lapply(myData, `[` ,'date')) 
#   dCallLogProbe$time_calling <- as.character(as.POSIXct(as.numeric(substr(dCallLogProbe$date,1,10)), origin="1970-01-01", tz = "GMT"))
#   dCallLogProbe$duration <- as.character(do.call(rbind,lapply(myData, `[` ,'duration'))) 
#   dCallLogProbe$number <- substr(gsub("^.*:","",as.character(do.call(rbind,lapply(myData, `[` ,'number')))),2,41)
#   dCallLogProbe$type <- as.character(do.call(rbind,lapply(myData, `[` ,'type')))  
#   dCallLogProbe$type[dCallLogProbe$type=="1"] <- "incoming"
#   dCallLogProbe$type[dCallLogProbe$type=="2"] <- "outgoing"
#   dCallLogProbe$type[dCallLogProbe$type=="3"] <- "missed"
#   dA <- cbind(dCallLogProbe$number, dCallLogProbe$type,dCallLogProbe$time_calling,dCallLogProbe$duration)
#   dCallLogProbe <- as.data.frame(dA)
#   names(dCallLogProbe) <- c("number","type","time","duration")
# 
# 
# 
# #function for processing sms data
# 
#   dSmsProbe <- subset(dataset, dataset$name=="SmsProbe")
#   #dSmsProbe
#   head(dSmsProbe)
#   tail(dSmsProbe)
#   myData <- lapply(dSmsProbe$value, fromJSON)  
#   dSmsProbe$date <- do.call(rbind,lapply(myData, `[` ,'date')) 
#   dSmsProbe$time <- as.character(as.POSIXct(as.numeric(substr(dSmsProbe$date, 1, 10)), origin="1970-01-01", tz = "GMT"))
#   dSmsProbe$address <- as.character(do.call(rbind,lapply(myData, `[` ,'address')))  
#   dSmsProbe$type <- as.character(do.call(rbind,lapply(myData, `[` ,'type')))  
#   dSmsProbe$type[dSmsProbe$type=="0"] <- "all"
#   dSmsProbe$type[dSmsProbe$type=="1"] <- "inbox"
#   dSmsProbe$type[dSmsProbe$type=="2"] <- "sent"
#   dSmsProbe$type[dSmsProbe$type=="3"] <- "draft"
#   dSmsProbe$type[dSmsProbe$type=="4"] <- "outbox"
#   dSmsProbe$type[dSmsProbe$type=="5"] <- "failed"
#   dSmsProbe$type[dSmsProbe$type=="6"] <- "queued"
#   dSmsProbe$status <- as.character(do.call(rbind,lapply(myData, `[` ,'status'))) 
#   dSmsProbe$status[dSmsProbe$status=="-1"] <- "none"
#   dSmsProbe$status[dSmsProbe$status=="0"] <- "complete"
#   dSmsProbe$status[dSmsProbe$status=="32"] <- "pending"
#   dSmsProbe$status[dSmsProbe$status=="64"] <- "failed"
#   # dSmsProbe$body-byte-len <- as.character(do.call(rbind,lapply(myData, `[` ,'body-byte-len')))  
#   # dSmsProbe$body-token-byte-len <- as.character(do.call(rbind,lapply(myData, `[` ,'body-token-byte-len')))  
#   # dSmsProbe$body-token-count <- as.character(do.call(rbind,lapply(myData, `[` ,'body-token-count')))  
#   dA <- cbind(dSmsProbe$time, dSmsProbe$type, dSmsProbe$address,dSmsProbe$status)
#   dSmsProbe <- as.data.frame(dA)
#   names(dSmsProbe) <- c("when", "type","address","status")
# 
# 
# 
# 


# 
# # 
#   #running application
# # # 
#   raw_data <- f_load_data("D:/DATA/raw/ENFP_0719/edu.mit.media.funf.wifiscanner/default/archive")
#   #removing duplicate data
#   dataset <- remove_duplicate(raw_data)
#   #subsetting 
#   d_activity <- f_activity_data(dataset)
#   d_bluetooth <- f_bluetooth_data(dataset)
#   d_battery <- f_battery_data(dataset)
#   d_call <- f_call_data(dataset)
#   d_sms <- f_sms_data(dataset)
#   d_search <- f_search_data(dataset)
#   d_runapps <- f_runapps_data(dataset)
#   d_screen <- f_screen_data(dataset)
#   d_location <- f_location_data(dataset)
#   d_wifi <- f_wifi_data(dataset)
# 
#   write.csv(d_activity, print(eval(sprintf("D:/DATA/output/%s/d_activity.csv",file))))
#   write.csv(d_bluetooth, print(eval(sprintf("D:/DATA/output/%s/d_bluetooth.csv",file))))
#   write.csv(d_battery, print(eval(sprintf("D:/DATA/output/%s/d_battery.csv",file))))
#   write.csv(d_call, print(eval(sprintf("D:/DATA/output/%s/d_call.csv",file))))
#   write.csv(d_sms, print(eval(sprintf("D:/DATA/output/%s/d_sms.csv",file))))
#   write.csv(d_search, print(eval(sprintf("D:/DATA/output/%s/d_search.csv",file))))
#   write.csv(d_runapps, print(eval(sprintf("D:/DATA/output/%s/d_runapps.csv",file))))
#   write.csv(d_screen, print(eval(sprintf("D:/DATA/output/%s/d_screen.csv",file))))
#   write.csv(d_location, print(eval(sprintf("D:/DATA/output/%s/d_location.csv",file))))
#   write.csv(d_wifi, print(eval(sprintf("D:/DATA/output/%s/d_wifi.csv",file))))
# 
#   d_sms <- d_sms[order(d_sms$when),]
# 
# # # 
# # # #problem ENFP_0773
# 
#   


setwd("D:/DATA/kampret")
file_list <- list.files()
for (file in file_list){
  #print(eval(sprintf("D:/DATA/output/%s/d_location.csv",file)))
  dir.create(print(eval(sprintf("D:/DATA/output/%s",file))))
}



for (file in file_list){
    dataset <- NULL
    d_activity <- NULL
    d_bluetooth <- NULL
    d_battery <- NULL
    d_call <- NULL
    d_sms <- NULL
    d_search <- NULL
    d_runapps <- NULL
    d_screen <- NULL
    d_location <- NULL
    d_wifi <- NULL
    raw_data <- f_load_data(print(eval(sprintf("D:/DATA/kampret/%s/edu.mit.media.funf.wifiscanner/default/archive",file))))
    #removing duplicate data
    dataset <- remove_duplicate(raw_data)
    #subsetting 
    d_activity <- f_activity_data(dataset)
    d_bluetooth <- f_bluetooth_data(dataset)
    d_battery <- f_battery_data(dataset)
    d_call <- f_call_data(dataset)
    d_sms <- f_sms_data(dataset)
    d_search <- f_search_data(dataset)
    d_runapps <- f_runapps_data(dataset)
    d_screen <- f_screen_data(dataset)
    d_location <- f_location_data(dataset)
    d_wifi <- f_wifi_data(dataset)
    
    
    write.csv(d_activity, print(eval(sprintf("D:/DATA/output/%s/d_activity.csv",file))))
    write.csv(d_bluetooth, print(eval(sprintf("D:/DATA/output/%s/d_bluetooth.csv",file))))
    write.csv(d_battery, print(eval(sprintf("D:/DATA/output/%s/d_battery.csv",file))))
    write.csv(d_call, print(eval(sprintf("D:/DATA/output/%s/d_call.csv",file))))
    write.csv(d_sms, print(eval(sprintf("D:/DATA/output/%s/d_sms.csv",file))))
    write.csv(d_search, print(eval(sprintf("D:/DATA/output/%s/d_search.csv",file))))
    write.csv(d_runapps, print(eval(sprintf("D:/DATA/output/%s/d_runapps.csv",file))))
    write.csv(d_screen, print(eval(sprintf("D:/DATA/output/%s/d_screen.csv",file))))
    write.csv(d_location, print(eval(sprintf("D:/DATA/output/%s/d_location.csv",file))))
    write.csv(d_wifi, print(eval(sprintf("D:/DATA/output/%s/d_wifi.csv",file))))
 
  
}




# for (file in file_list){
#   #print(paste(file))
#   #print(eval(sprintf("D:/DATA/example/%s/edu.mit.media.funf.wifiscanner/default/archive",file)))
#   "D:/DATA/output/"paste(file, collapse = "")),showWarnings = TRUE)
#   print(eval(sprintf("D:/DATA/output/%s",file)))
# }
# 
# ?eval
# 
# paste("Today is", date(),"dads")
# paste(1:12)





#?system.time
#run all function and show time execution for each function
# 
# system.time(dataset <- f_load_data("D:/DATA/example"))
# system.time(d_activity <- f_activity_data(dataset))
# system.time(d_apps <- f_apps_data(dataset))
# system.time(d_bluetooth <- f_bluetooth_data(dataset))
# system.time(d_battery <- f_battery_data(dataset))
# system.time(d_call <- f_call_data(dataset))
# # system.time(d_contact <- f_contact_data(dataset))
# system.time(d_sms <- f_sms_data(dataset))
# system.time(d_hardware <- f_hardware_data(dataset))
# system.time(d_bookmark <- f_bookmark_data(dataset))
# system.time(d_search <- f_search_data(dataset))
# system.time(d_light <- f_light_data(dataset))
# system.time(d_magno <- f_magno_data(dataset))
# system.time(d_pressure <- f_pressure_data(dataset))
# system.time(d_proximity <- f_proximity_data(dataset))
# system.time(d_runapps <- f_runapps_data(dataset))
# system.time(d_screen <- f_screen_data(dataset))
# system.time(d_location <- f_location_data(dataset))
# system.time(d_wifi <- f_wifi_data(dataset))
# 


#loading data 
#raw_data <- f_load_data("D:/DATA/raw/ENFP_0719/edu.mit.media.funf.wifiscanner/default/archive")
#removing duplicate data
#dataset <- remove_duplicate(raw_data)
# d_activity <- f_activity_data(dataset)
# d_apps <- f_apps_data(dataset)
# d_bluetooth <- f_bluetooth_data(dataset)
# d_battery <- f_battery_data(dataset)
# d_call <- f_call_data(dataset)
# #d_contact <- f_contact_data(dataset)
# d_sms <- f_sms_data(dataset)
# d_hardware <- f_hardware_data(dataset)
# d_bookmark <- f_bookmark_data(dataset)
# d_search <- f_search_data(dataset)
# d_light <- f_light_data(dataset)
# d_magno <- f_magno_data(dataset)
# d_pressure <- f_pressure_data(dataset)
# d_proximity <- f_proximity_data(dataset)
# d_runapps <- f_runapps_data(dataset)
# head(d_runapps)
# d_screen <- f_screen_data(dataset)
# d_location <- f_location_data(dataset)
# d_wifi <- f_wifi_data(dataset)



# url <- c("facebook.com","twitter.com","facebook.com","facebook.com","twitter.com","facebook.com","chonnam.com")
# 
# seque <- seq(1,7)
# 
# df <- cbind(seque,url)
# 
# df <- as.data.frame(df)
# 
# df$label <- NULL
# df$label[df$url=="facebook.com"] <- "sosmed"
# df$label[df$url=="twitter.com"] <- "sosmed"
# df$label[df$url=="chonnam.com"] <- "kampus"


as.POSIXct(d_activity$time, format="%Y-%m-%d %H:%M:%S")

  dataset <- f_load_data(print(eval(sprintf("D:/DATA/raw/ENTJ_6454/edu.mit.media.funf.wifiscanner/default/archive",file))))

  dActivityProbe  <- subset(dataset, dataset$name=="ActivityProbe")
  head(dActivityProbe)
  myData <- lapply(dActivityProbe$value, fromJSON)
  dActivityProbe$value <- as.character(do.call(rbind,lapply(myData, `[` ,'activityLevel')))   
  dActivityProbe$time <- do.call(rbind,lapply(myData, `[` ,'timestamp')) 
  dActivityProbe$time <- as.character(as.POSIXct(as.numeric(dActivityProbe$time), origin="1970-01-01", tz = "GMT"))
  dA <- cbind(dActivityProbe$value,dActivityProbe$time)
  dActivityProbe <- as.data.frame(dA)
  names(dActivityProbe) <- c("Activity","Time")



