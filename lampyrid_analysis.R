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
lampyrid$week<-week(lampyrid$newdate)

#let's look for the data problems we found we used OpenRefine and see if
#we can impliment our cleaning operations here- that way we have a complete
#record of EVERYTHING that happened to these data. Recall there were issues 
#with TREAT_DESC
#let's look at these columns individually and fix errors as we find them
#and we should also check for weirdness in our numeric values

summary(lampyrid)
#looks like there's one missing data point (NA) for adults. Let's ditch
#it so it doesn't cause any problems in subsequent analyses
lampyrid<-na.omit(lampyrid)
summary(lampyrid)

#looks good. Okay, TREAT_DESC:

summary(as.factor(lampyrid$TREAT_DESC))
#wow, we've got some spelling errors. Let's clean that up

lampyrid$TREAT_DESC<-gsub("Early succesional community", "Early successional community", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Early sucessional community", "Early successional community", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Succesional", "Successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Sucessional", "Successional", lampyrid$TREAT_DESC)
#also convert this column to factor (gsub sometimes turns it into character type)
lampyrid$TREAT_DESC<-as.factor(lampyrid$TREAT_DESC)
summary(lampyrid$TREAT_DESC)

#do the same for HABITAT
summary(as.factor(lampyrid$HABITAT))
#checks out. Let's make sure R is seeing it as a factor, and also rep and station while we're at it

lampyrid$HABITAT<-as.factor(lampyrid$HABITAT)
lampyrid$REPLICATE<-as.factor(lampyrid$REPLICATE)
lampyrid$STATION<-as.factor(lampyrid$STATION)

#one more check to see if the data looks clean
summary(lampyrid)


#download weather data from KBS weather station
weather<-read.table(file="http://lter.kbs.msu.edu/datatables/7.csv",
                    header=T, sep=",", na.strings="")
#extract day of year, so we have a continuous variable running for each year.
#since we're in a temperate northern climate, this is convenient- not too 
#much insect action happening at the december-january transition, so we 
#can use the yearly break as a blocking variable for rowing season.
#it's convenient living where we do! 

weather$DOY<-yday(weather$date)
weather$week<-week(weather$date)
#do a few simple plots to make sure the data makes sense -this is
#a good way to check that the importation was sucessful

plot(weather$DOY, weather$air_temp_mean)
plot(weather$DOY, weather$precipitation)

#because we don't have lampyrid records before 2004, let's cut out the data
#from before 2003 so we can process the weaqther data more quickly. Also our
#lampyrid data stops at the end of 2015 and for some reason the new
#weather station data breaks our code. DANGIT. so we'll cut off the weather
#data that's causing us problems- we don't need it anyway
weather<-subset(weather, weather$year>2003& weather$year<2016)

#lets also get rid of the vairables we don't need:
weather$flag_precip<-NULL
weather$flag_air_temp_mean<-NULL
weather$flag_air_temp_max<-NULL
weather$flag_air_temp_min<-NULL

#also, these data are sorted in decending order. It's easier to think of this 
#stuff in ascending order, so let's sort the data by year and DOY

weather<-weather[order(weather$year, weather$DOY),]

#lets's pre-process these weather data so we get rid of missing values
# we can write a function to do this for us.
#if missing data is rare, it is probably safe to assume that missing
#temperatures are similar to the weather on the day before or after.
#for the sake of simplicity, let's replace a missing value with the 
#value for that variable for the day before

#first, define the function

replace.missing<-function(vec){
  #create a vector to put our new values into
  New = c()
  for (i in 1:(length(vec))){
    if (is.na(vec[i])){
      vec[i]<-vec[i-1]
      #if the data is missing, sub in the value from the measurement before

    } else{
      #if the value is not missing, just pass it through to the result vector
      vec[i]<-vec[i]
    }
    New=c(New, vec[i])
  }
  if (any(is.na(New))){
    replace.missing(New)
  }
  return(New)
}


# now create new variables with the cleaned data we're interested in
weather$air_temp_max_clean<-replace.missing(weather$air_temp_max)

weather$air_temp_min_clean<-replace.missing(weather$air_temp_min)

#check that nothing weird happened with the data when we created this new variable

#did it get all the NAs?
summary(weather$air_temp_max_clean)
summary(weather$air_temp_min_clean)

#these plots should produce perfect 1:1 lines

plot(weather$air_temp_max, weather$air_temp_max_clean)
plot(weather$air_temp_min, weather$air_temp_min_clean)

#they do! That means our function doesn't break anything. YAY!

#we'll need to operate on a dataset that's sorted in a decending way
#for this because it's easier to think about accumulations that way


# calculate the degree day accumulation for the first half of the day dd1,
#assuming a sine wave structure of temperature over the day
#use a development threshold of 10C, well, because it's a nice number
#to work with
#we'll use the model presented in Allen 1976 which uses daily max and min temperatures
#and assumes temperature follows a sine wave

allen<-function(maxi, mini, thresh){
  #if threshold is not given, assume it's 10 Celcius
  if(missing(thresh)) {
    thresh<-10
  } else {
    thresh<-thresh
  }
  dd1<-c()
  dd2<-c()
    for (i in 1:length(maxi)){
     if (maxi[i]>= thresh & mini[i]<thresh) {
      #first half of day
      #amplitude of temperature difference
      alpha1<-(maxi[i]-mini[i])/2
      #average temperature
      avg1<-(maxi[i]+mini[i])/2
      #theta is time point when temperatur crosses the threshold
      #assuming temperature is roughly following the sine curve
      theta1<-asin((thresh-avg1)/alpha1)
      #use these to calculate degree day accumulation over first half of day
      dd1.T<-(1/(2*pi))*((avg1-thresh)*(pi/2 - theta1)+alpha1*cos(theta1))
      dd1<-c(dd1, dd1.T)
      #second half of day
      #two possible cases, min temperature on day i+1 could be below thereshold or above
      #for below threshold:
      if (mini[i+1]<thresh){
        #amplitude of temperature difference
        alpha2<-(maxi[i]-mini[i+1])/2
        #average temperature
        avg2<-(maxi[i]+mini[i+1])/2
        #theta is time point when temperatur crosses the threshold
        #assuming temperature is roughly following the sine curve
        theta2<-asin((thresh-avg2)/alpha2)
        #use these to calculate degree day accumulation over first half of day
        dd2.T<-(1/(2*pi))*((avg2-thresh)*(pi/2 - theta2)+alpha2*cos(theta2))
        dd2<-c(dd2, dd2.T)
      } else { #for above threshold
        #second half of day
        avg2<-(maxi[i]+mini[i+1])/2
        dd2.T<-(avg2-thresh)/2
        dd2<-c(dd2, dd2.T)
      }
      
     } else if (mini[i]>=thresh){
       #first half of day
       avg1<-(maxi[i]+mini[i])/2
       dd1.T<-(avg1-thresh)/2
       dd1<-c(dd1, dd1.T)
       #second half of day, as above, two possible cases
       if (mini[i+1]>=thresh){
         avg2<-(maxi[i]+mini[i+1])/2
         dd2.T<-(avg2-thresh)/2
         dd2<-c(dd2, dd2.T)
       } else{
         #amplitude of temperature difference
         alpha2<-(maxi[i]-mini[i+1])/2
         #average temperature
         avg2<-(maxi[i]+mini[i+1])/2
         #theta is time point when temperatur crosses the threshold
         #assuming temperature is roughly following the sine curve
         theta2<-asin((thresh-avg2)/alpha2)
         #use these to calculate degree day accumulation over first half of day
         dd2.T<-(1/(2*pi))*((avg2-thresh)*(pi/2 - theta2)+alpha2*cos(theta2))
         dd2<-c(dd2, dd2.T)
       }
       
    }
      else  {
        #if temperature doesn't get over threshold, no degree days accumulated
        #first half of day
        dd1<-c(dd1, 0)
        #second half of day
        dd2<-c(dd2, 0)
    }
    #total accumulation over the day is just first half of day plus second
    
  }
  
  return(dd1+dd2)
  
}

#do some checks to make sure the function is working properly

weather$dd<-allen(weather$air_temp_max_clean, weather$air_temp_min_clean, 10)

#plot to make sure nothing weird is happening- look for more degree days midyear,
#and NO negative values. Looks like we're WINNING!
plot(weather$DOY, weather$dd)

#now write a new function to calculate accumulated degree days


accum.allen<-function(maxi, mini, thresh, DOY){
  dd<-allen(maxi, mini, thresh)
  dd.accum<-c()
  for (i in 1:length(dd)){
    #hmm, need a way to sum up over the year, starting anew for each year.
    #this should do it
    if (DOY[i]==1){
      dd.accum.day=0
    }
    #the accumulation on day i is the degree day accumulation before
    #plus the dd accumulated on that day
    dd.accum.day<-dd.accum.day+dd[i]
    #add that day's accumulation to the vector
    dd.accum<-c(dd.accum, dd.accum.day)
  }
  return (dd.accum)
}
 
#same sort of checks. Run the function for our data

weather$dd.accum<-accum.allen(weather$air_temp_max_clean, weather$air_temp_min_clean, 10, weather$DOY)
 #and plot that thing to look for problems:
plot(weather$DOY, weather$dd.accum)
#looks good! victory!!!

#so, now we have two datasets that both have information we need in them.
#let's put it all together in one frame
lampyrid.weather<-merge(lampyrid, weather, by=c("year", "DOY", "week"), all.x=TRUE)

library(ggplot2)

lampyrid.doy<-ggplot(lampyrid.weather, aes(DOY, ADULTS, 
                                           color=factor(year)))+
  geom_point()

lampyrid.doy
lampyrid.week<-ggplot(lampyrid.weather, aes(week, ADULTS, 
                                            color=factor(year)))+
  geom_point()
lampyrid.week

# we're interested in looking at more general trends. We'll need to produce 
#summary data to do this

library(plyr)
captures.by.year<-ddply(lampyrid.weather, c("year"), summarise,
      total=sum(ADULTS), traps=length(ADULTS), avg=sum(ADULTS/length(ADULTS)))

captures.by.week.year<-ddply(lampyrid.weather, c("year", "week"), summarise,
                              total=sum(ADULTS), traps=length(ADULTS), 
                             avg=sum(ADULTS/length(ADULTS)),
                             ddacc=max(dd.accum))


lampyrid.summary.week<-ggplot(captures.by.week.year, aes(week, avg, 
                                            color=factor(year)))+
  geom_point()+geom_smooth(se=FALSE)
lampyrid.summary.week

lampyrid.summary.ddacc<-ggplot(captures.by.week.year, aes(ddacc, avg, 
                                                         color=factor(year)))+
  geom_point()+geom_smooth(se=FALSE)
lampyrid.summary.ddacc


