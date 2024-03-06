





# This script processes all the files from all the devices in one folder
# The name of the files should follow this format:
# airpoll#_**_DDMMYYYY,
# Where '#' is ne number of the CPC device,e.g. 6,8,9,10. and '*' is any valid character
# all the files have to be .CSV, with only three fields: "time","conc","desc"
# 'time' is the original time from the original exported CPC csv file
# 'conc' is the original conc, that should be already cleaned manually
# 'desc' is an additional field to specify if a measurement has been dicarted 'D' or a failure 'F'
# The code require the libraries tidyverse and lubridate

library(tidyverse)
library(lubridate)
cat("\f")
######################Setting the working directory#########################
main.folder <- readline(prompt="Enter the path of the main folder (copy and paste from Windows Explorer): ")
setwd(main.folder)

#####################List of files to process################################
# The algorithm looks for the csv files in the specified folder with a size above 3 KB
# and creates a folder called "processed_files"

lst_files<-list.files(recursive = FALSE,pattern = "\\.csv$")
lst_files<-lst_files[sapply(lst_files, file.size) > 3000]
qty_files<-length(lst_files)
dir.create("processed_files")

#####################Loop for processing the files################################
# The loop goes file by file calculating the elapsed time in seconds since the first reading and
# saves a file ready to be used with the following name CPC_AIR-POLL-##_YYMMDD

for (i in (1:qty_files)) {
  tmp_file<-lst_files[i]
  dev_num<-gsub("_","",substr(tmp_file, 8, 9))
  mydf <- read_csv(tmp_file,col_types = cols(Desc = col_factor(levels = c("D","F"))))
  names(mydf)<-c("time","conc","desc")
  mydf$time<-as.POSIXct(mydf$time, format = "%H:%M:%S",tz = "UTC")
  mydf$sec<-difftime(mydf$time,min(mydf$time),units = "secs")
  tmp_date<-as.POSIXct(gsub("_","",substr(tmp_file, 13, 21)),format = "%d%m%Y")
  tmp_date<-format(tmp_date, "%y%m%d") 
  tmp_name<-paste("processed_files/CPC_AIR-POLL-",dev_num,"_",tmp_date,".csv",sep = "")
  write.csv(mydf, file = tmp_name)
  rm(mydf)
}
print("Done! Check the files")
