# This script merges all the processed based on a reference file stored in a SINGLE folder
# 
# 
# The code require the libraries: tidyverse, xts and readr

library(xts)
library(readr)
library(tidyverse)


######################Setting the working directory#########################
main.folder <- readline(prompt="Enter the path of the main folder (copy and paste from Windows Explorer): ")
setwd(main.folder)

# The reference file (files_matrix.csv) should have the following columns Day,Date,Route,Speed,CPC_Noise,Part,Strava_SZ,CPC,Strava,SZ,Noise
# each line correspond to the files to merge
# 'Day' -> the number of the sampling day
# 'Date' -> the date of the sampling day
# 'Route' -> the type of route, in this case Polluted or Track
# 'Speed'  -> The speed level
# 'CPC_Noise' -> the number of the devices CPC and Noise used 
# 'Part'  -> the designation for the participant
# 'Strava_SZ' -> the number of the devices phones and SZ used
# 'CPC' -> the exact name of the CPC file  of that run
# 'Strava' -> the exact name of the  GPS file of that run
# 'SZ' -> the exact name of the SweetZpot file of that run
# 'Noise' -> the exact name of the noise  file of that run (not necessary)


## if there are some changes in the structure, the importing that follows should be modified 
files_matrix <- read_csv("files_matrix.csv", 
                         col_types = cols(Date = col_date(format = "%d/%m/%Y"),
                                          Day = col_factor(levels = c("1","2", "3", "4", "5", "6", "7","8")), 
                                          Part = col_factor(levels = c("A","B", "X", "Y")), 
                                          Route = col_factor(levels = c("Polluted","Track")), 
                                          Speed = col_factor(levels = c("Low","High"))))





lst_files<-c("a","a","a","a")
names(lst_files)<-c("Strava","CPC","SZ","Noise")

#####################Loop for processing the files################################
# The loop goes line by line merging the content of the different files using the timestamp as a referece
# The result is a single data frame called df_full
#
# the timestamp for the CPC files are calculated using the start time of the GPS file
# Assuming that the procedure was followed (starting Strava and CPC at the same time)
# The lines related to the Noise files have been included in this script and commented '#' to be omitted during the execution
#
#
# Lines should be omitted if the files are not available with the "#"
# This script has been adapted to process GPS-CPC and GPS-SZ independently





for (i in (1:28)) {
  tmp_strava<-files_matrix$Strava[i]
  lst_files["Strava"]<-list.files(recursive = TRUE,pattern = tmp_strava)
  tmp_data_strava <- read_csv(lst_files["Strava"], col_types = cols(X1 = col_skip()))
  
  tmp_CPC<-files_matrix$CPC[i]
  lst_files["CPC"]<-list.files(recursive = TRUE,pattern = tmp_CPC)
  tmp_data_CPC <- read_csv(lst_files["CPC"], col_types = cols(X1 = col_skip(), desc = col_factor(levels = c("F","D"))))
  tmp_data_CPC$time<-min(tmp_data_strava$time)+tmp_data_CPC$sec
  
  tmp_SZ<-files_matrix$SZ[i]
  lst_files["SZ"]<-list.files(recursive = TRUE,pattern = tmp_SZ)
  tmp_data_SZ<-read_csv(lst_files["SZ"], col_types = cols(X1 = col_skip()))
  
  #tmp_Noise<-files_matrix$Noise[i]
  #lst_files["Noise"]<-list.files(recursive = TRUE,pattern = tmp_Noise)
  #tmp_data_Noise <- read_csv(lst_files["Noise"],col_types = cols(X1 = col_skip(), X10 = col_skip()))
  
  ts_strava<-xts(tmp_data_strava,order.by =tmp_data_strava$time)
  ts_CPC<-xts(tmp_data_CPC,order.by =tmp_data_CPC$time)
  ts_SZ<-xts(tmp_data_SZ,order.by =tmp_data_SZ$timestamp)
  #ts_Noise<-xts(tmp_data_Noise,order.by =tmp_data_Noise$time)
  
  x<-c(0:(as.numeric(max(tmp_data_strava$time))-as.numeric(min(tmp_data_strava$time))))
  
  Participant<-rep(as.character(files_matrix$Part[i]),length(x))
  TypeSpeed<-rep(as.character(files_matrix$Speed[i]),length(x))
  TypeRoute<-rep(as.character(files_matrix$Route[i]),length(x))
  Day<-rep(as.character(files_matrix$Day[i]),length(x))
  DateShort<-rep(as.character(files_matrix$Date[i]),length(x))
  x1<-data.frame(Participant,TypeSpeed,TypeRoute,Day,DateShort)
  
  
  ts_compiled<-xts(x1, order.by=(min(tmp_data_strava$time)+x))
  ts_compiled<-merge(ts_compiled,ts_strava,fill = NA,join = "left")
  ts_compiled<-merge(ts_compiled,ts_CPC,fill = NA,join = "left")
  ts_compiled<-merge(ts_compiled,ts_SZ,fill = NA,join = "left")
  #ts_compiled<-merge(ts_compiled,ts_Noise,fill = NA,join = "left")
    
  if (i==1) {
      ts_full<-ts_compiled
    }else{
      ts_full<-rbind(ts_full,ts_compiled)
    }
  rm(ts_compiled)
  }


df_full<-as.data.frame(ts_full)
df_full$Day<-as.numeric(levels(df_full$Day))[df_full$Day]
df_full$conc<-as.numeric(levels(df_full$conc))[df_full$conc]
df_full$speed<-as.numeric(levels(df_full$speed))[df_full$speed]
df_full$speed<-df_full$speed*3.6
df_full$lat<-as.numeric(levels(df_full$lat))[df_full$lat]
df_full$long<-as.numeric(levels(df_full$long))[df_full$long]
df_full$alt<-as.numeric(levels(df_full$alt))[df_full$alt]
df_full$distance<-as.numeric(levels(df_full$distance))[df_full$distance]
#df_full$LAeq<-as.numeric(levels(df_full$LAeq))[df_full$LAeq]
#df_full$LAPeak<-as.numeric(levels(df_full$LAPeak))[df_full$LAPeak]
#df_full$LCeq<-as.numeric(levels(df_full$LCeq))[df_full$LCeq]
#df_full$LCPeak<-as.numeric(levels(df_full$LCPeak))[df_full$LCPeak]
df_full$HR<-as.numeric(levels(df_full$HR))[df_full$HR]
df_full$flow<-as.numeric(levels(df_full$flow))[df_full$flow]
df_full$breathRate<-as.numeric(levels(df_full$breathRate))[df_full$breathRate]
df_full$breathing<-as.numeric(levels(df_full$breathing))[df_full$breathing]
df_full$latitute<-as.numeric(levels(df_full$latitute))[df_full$latitute]
df_full$longitude<-as.numeric(levels(df_full$longitude))[df_full$longitude]
df_full$time<-as.POSIXct(df_full$time, format = "%Y-%m-%d %H:%M:%S",tz = "UTC")
df_full$time.1<-as.POSIXct(df_full$time.1, format = "%Y-%m-%d %H:%M:%S",tz = "UTC")
#df_full$time.2<-as.POSIXct(df_full$time.2, format = "%Y-%m-%d %H:%M:%S",tz = "UTC")

####### The resulting file is exported to be processed in QGIS ######

write.csv(df_full, file = "DF_full.cvs")




start_times <- read_csv("start_times.csv", col_types = cols(Day = col_integer(), End_I = col_double(), End_O = col_double(), Part = col_character(), Short_date = col_character(), Start_I = col_double(), Start_O = col_double()))

for (u in (1:7)) {

  start_times[[u,4]]<-min(df_full$time[(df_full[,"Participant"]==start_times$Part[u])&(df_full[,"Day"]==start_times$Day[u])],na.rm = T)+start_times[[u,4]]
  start_times[[u,5]]<-min(df_full$time[(df_full[,"Participant"]==start_times$Part[u])&(df_full[,"Day"]==start_times$Day[u])],na.rm = T)+start_times[[u,5]]
  start_times[[u,6]]<-min(df_full$time[(df_full[,"Participant"]==start_times$Part[u])&(df_full[,"Day"]==start_times$Day[u])],na.rm = T)+start_times[[u,6]]
  start_times[[u,7]]<-min(df_full$time[(df_full[,"Participant"]==start_times$Part[u])&(df_full[,"Day"]==start_times$Day[u])],na.rm = T)+start_times[[u,7]]

}

start_times$Start_I<-.POSIXct(start_times$Start_I,tz="UTC")
start_times$Start_O<-.POSIXct(start_times$Start_O,tz="UTC")
start_times$End_I<-.POSIXct(start_times$End_I,tz="UTC")
start_times$End_O<-.POSIXct(start_times$End_O,tz="UTC")

write.csv(start_times, file = "Times_start.cvs")

