# This script processes all the sweetzpot breathing.txt files from ONE Single device in one folder
# The names of the files does not have to match a specific format
# 
# The code require the library tidyverse

library(tidyverse)
cat("\f")
######################Setting the working directory#########################
main.folder <- readline(prompt="Enter the path of the main folder (copy and paste from Windows Explorer): ")
setwd(main.folder)

####################List of files to process###########
# The algorithm looks for the csv files in the specified folder with a size above 3 KB
# and creates two folders called "summary_files" and "raw_files"
dev_num <- readline(prompt="Enter the number of AIR-POLL device you are processing: ")
lst_files<-list.files(recursive = TRUE,pattern = "^[Breath]")
lst_files<-lst_files[sapply(lst_files, file.size) > 3000]
qty_files<-length(lst_files)

dir.create("summary_files")
dir.create("raw_files")

#####################Loop for processing the files################################
# The loop goes file by file calculating a summary and creating a raw CSV file with the headings
# The files are stored separately with the following name SZ_AIR-POLL-##_YYMMDD, where '#' is the device that was specified


for (i in (1:qty_files)) {
  tmp_file<-lst_files[i]
  #### import the txt to a table change "Breathing.txt" for the name of the target file
  raw_data <- read_table2(tmp_file, col_names = FALSE, col_types = cols(X8 = col_character(), X9 = col_logical()))
  
  ##### names from the XML file
  names(raw_data)<-c("heartRate",	"hrv",	"breathing",	"flow",	"breathRate",	"latitute",	"longitude",	"timestamp",	"forBreathingPattern")
  
  ##### time stamps as POSICxt
  #raw_data$time<-.POSIXct(as.numeric(raw_data$timestamp)/1000, tz="UTC")
  
  
  ###### calculating difference
  raw_data$timestamp<-as.numeric(raw_data$timestamp)
  mintime<-min(raw_data$timestamp%/%1000)
  raw_data$sec<-floor((raw_data$timestamp-min(raw_data$timestamp))%/%1000)
  
  
  ###### creating the dataset with 1 Hz means, lat and long use the median, the boolean take the value with highest frequency
  
  summary_data<-raw_data%>%group_by(sec)%>%summarise(HR=mean(heartRate),
                                                     hrv=mean(hrv),breathing=mean(breathing),flow=mean(flow),
                                                     breathRate=mean(breathRate),latitute=median(latitute),
                                                     longitude=median(longitude),
                                                     #timestamp=.POSIXct((mintime+sec*1000)),
                                                     bool=(0.5<=mean(as.numeric(forBreathingPattern))))
  
  summary_data$timestamp<-.POSIXct((mintime+summary_data$sec))
  tmp_date<-summary_data$timestamp[1]
  tmp_date<-format(tmp_date, "%y%m%d-%H%M") 
  tmp_sname<-paste("summary_files/SZ_AIR-POLL-",dev_num,"_",tmp_date,"_summary.csv",sep = "")
  tmp_rname<-paste("raw_files/SZ_AIR-POLL-",dev_num,"_",tmp_date,"_raw.csv",sep = "")
  write.csv(raw_data, file = tmp_rname)
  write.csv(summary_data, file = tmp_sname)
  rm(raw_data)
  rm(summary_data)
  }
print("Done!,         Please check the files")