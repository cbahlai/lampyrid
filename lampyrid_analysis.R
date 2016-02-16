# lampyrid
lampyrid<-read.csv(file="https://ndownloader.figshare.com/files/3686040", header=T)
library(lubridate)
lampyrid$newdate<-mdy(lampyrid$DATE)
lampyrid$year<-year(lampyrid$newdate)
lampyrid$DOY<-yday(lampyrid$newdate)
