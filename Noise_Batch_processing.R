library(readr)
library(lubridate)
cat("\f")

main.folder <- readline(prompt="Enter the path of the main folder (copy and paste from Windows Explorer): ")
setwd(main.folder)
lst_files<-list.files(recursive = TRUE,pattern = "\\.csv$")
lst_files<-lst_files[sapply(lst_files, file.size) > 3000]
qty_files<-length(lst_files)

dir.create("processed")
for (i in (1:qty_files)) {
  tmp_file<-lst_files[i]
  dev_num<-gsub("-","",substr(tmp_file, 12, 13))
  
  raw_data <- read_csv(tmp_file, col_types = cols(Date = col_character(), Time = col_character()))
  raw_data$time<-paste(raw_data$Date," ",raw_data$Time)
  raw_data$time<-as.POSIXct(raw_data$time,tz="Europe/London",format = "%d/%m/%Y %H:%M:%S")
  tmp_date<-raw_data$time[1]
  tmp_date<-format(tmp_date, "%y%m%d")
  
  tmp_rname<-paste("processed/N_AIR-POLL-",dev_num,"_",tmp_date,"_raw.csv",sep = "")
  write.csv(raw_data, file = tmp_rname)
  rm(raw_data)
  
  
}
print("Done!,         Please check the files")

