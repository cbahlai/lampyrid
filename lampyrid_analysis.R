# lampyrid analysis
#bring data in from figshare
lampyrid<-read.csv(file="https://ndownloader.figshare.com/files/3686040",
                   header=T)

#clean data
#fix dates, make them ISO'ed
library(lubridate)
lampyrid$newdate<-mdy(lampyrid$DATE)
#extract year
lampyrid$year<-year(lampyrid$newdate)
#extract day of year. DOY is very useful for a phenology type analyses
#because you don't have to deal with day-of-month numbers starting over 
#in the middle of a phenological event.
lampyrid$DOY<-yday(lampyrid$newdate)

#download weather data from KBS weather station
weather<-read.table(file="http://lter.kbs.msu.edu/datatables/7.csv",
                    header=T, sep=",")
