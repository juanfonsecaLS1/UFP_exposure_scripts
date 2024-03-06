

# This script processes all the files GPX or TCX from ONE Single device in one folder
# The names of the files does not have to match a specific format
# 
# The code require the libraries tidyverse, lubridate and XML, xml2


library(xml2)
library(XML)
library(tidyverse)
library(lubridate)

cat("\f")

######################Setting the working directory#########################
main.folder <- readline(prompt="Enter the path of the main folder (copy and paste from Windows Explorer): ")
setwd(main.folder)
#######DEFINE THE TYPE OF FILE TO PROCESS##################################
files_type<-readline(prompt="Enter the file type 1 -> GPX or 2 -> TCX : ")

#####################Loop for processing the files################################
# The loop goes file by file converting the GPX or TCX files into CSV and
# saves a file ready to be used with the following name GPX_AIR-POLL-##_YYMMDD or TCX_AIR-POLL-##_YYMMDD 
# where '#' is the device that was specified

if (files_type==1|files_type==2){
  ############ IT IS ASSUMED THAT THE FILES ARE GOING TO BE PROCESSED BY DEVICE, THIS DEFINE THE NUMBER OF THE DEVICE#########
  dev_num <- readline(prompt="Enter the number of AIR-POLL device you are processing: ")
  if (files_type==1){
    ######### THIS LOOKS FOR THE GPX FILES TO SAVE AS CSV AND DEFINES TEMPORARY PARAMETERS FOR THE LOOP############
    lst_files<-list.files(recursive = FALSE,pattern = "\\.gpx$")
    lst_files<-lst_files[sapply(lst_files, file.size) > 3000]
    qty_files<-length(lst_files)
    
    for (i in (1:qty_files)) {
      tmp_file<-lst_files[i]
      #### importing and processing the GPX file
      mydf<-readGPX(tmp_file, tracks = TRUE)
      mydf<-mydf$tracks[[1]][[1]]
      names(mydf)<- c('long','lat', 'alt','time')
      ##### cleansing the dataset###
      
      
      mydf$alt<-as.numeric(mydf$alt)
      
      mydf$time<-as.character(mydf$time)
      mydf$time<-gsub("T"," ",mydf$time)
      mydf$time<-gsub("Z","",mydf$time)
      mydf$time<-as.POSIXct(mydf$time, format = "%Y-%m-%d %H:%M:%S",tz = "UTC")
      mydf$time<-with_tz(mydf$time,"Europe/London")
      
      tmp_date<-mydf$time[1]
      tmp_date<-format(tmp_date, "%y%m%d-%H%M") 
      tmp_name<-paste("GPX_AIR-POLL-",dev_num,"_",tmp_date,".csv",sep = "")
      write.csv(mydf, file = tmp_name)
      rm(mydf)
      
      
    }
    
  }
    if(files_type==2)  {
      ######### THIS LOOKS FOR THE TCX FILES TO SAVE AS CSV AND DEFINES TEMPORARY PARAMETERS FOR THE LOOP############
      lst_files<-list.files(recursive = FALSE,pattern = "\\.tcx$")
      lst_files<-lst_files[sapply(lst_files, file.size) > 3000]
      qty_files<-length(lst_files)
      
      for (i in (1:qty_files)) {
        tmp_file<-lst_files[i]
        #### importing and processing the XML (TCX) file
        doc <- xmlParse(tmp_file)
        xmlToDataFrame(nodes <- getNodeSet(doc, "//ns:Trackpoint", "ns"))
        nodes <- getNodeSet(doc, "//ns:Trackpoint", "ns")
        mydf  <- plyr::ldply(nodes, as.data.frame(xmlToList))
        names(mydf)<- c('time', 'lat', 'long', 'alt', 'distance','speed')
        ##### cleansing the dataset###
        
        mydf$lat<-as.numeric(levels(mydf$lat))[mydf$lat]
        mydf$long<-as.numeric(levels(mydf$long))[mydf$long]
        mydf$alt<-as.numeric(levels(mydf$alt))[mydf$alt]
        mydf$distance<-as.numeric(levels(mydf$distance))[mydf$distance]
        mydf$speed<-as.numeric(levels(mydf$speed))[mydf$speed]
        
        mydf$time<-as.character(mydf$time)
        mydf$time<-gsub("T"," ",mydf$time)
        mydf$time<-gsub("Z","",mydf$time)
        mydf$time<-as.POSIXct(mydf$time, format = "%Y-%m-%d %H:%M:%S",tz = "UTC")
        mydf$time<-with_tz(mydf$time,"Europe/London")
        
        tmp_date<-mydf$time[1]
        tmp_date<-format(tmp_date, "%y%m%d-%H%M") 
        tmp_name<-paste("TCX_AIR-POLL-",dev_num,"_",tmp_date,".csv",sep = "")
        write.csv(mydf, file = tmp_name)
        rm(mydf)
        rm(nodes)
      }
      
    }
  print("Done! Check the files")
} else {print("Try again. You must select 1 or 2")}






