# lampyrid analysis
#bring data in from figshare
lampyrid<-read.csv(file="https://ndownloader.figshare.com/files/3686040",
                   header=T)

#we're going to be looking at the responses of lampyrids to environmental conditions
#this means making some choices about when to stat counting
#what day of the year should we start the analysis on? 
#giving it a start day of Mar 1
start<-60

#coding the variable like this makes it easy to re-run the code with different start dates
#to see what the effect of the start date has on our conclusions. this type of testing is 
#often referred to as 'sensitivity analysis'- ie seeing how sensitive your conclusions are to
#your  assumptions, guesses or starting points.

#clean data
#fix dates, make them ISO'ed
library(lubridate)
library(ISOweek)
lampyrid$newdate<-mdy(lampyrid$DATE)
#extract year
lampyrid$year<-year(lampyrid$newdate)
#extract day of year. DOY is very useful for a phenology type analyses
#because you don't have to deal with day-of-month numbers starting over 
#in the middle of a phenological event.
lampyrid$DOY<-yday(lampyrid$newdate)
#use ISO week, so we start counting on Monday, not Jan 1, COOL! Our sampling usually 
#takes place Wed-Friday, so if we use week of year stating on Jan 1, there is a good chance that
#samples taken within a sampling week would get grouped incorrectly when we go to do the analysis.
lampyrid$week<-isoweek(lampyrid$newdate)

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

lampyrid$TREAT_DESC<-gsub("Early succesional community", "Early successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Early sucessional community", "Early successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Succesional", "Successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Sucessional", "Successional", lampyrid$TREAT_DESC)
#also shorten biologically based (organic) and conventional till for plotting purposes 
lampyrid$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Conventional till", "Conventional", lampyrid$TREAT_DESC)

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

#so we have a small issue with these data. The counts will be strongly zero-biased because we 
# give each subsample its own observation. When it comes to modelling and plotting, we're going to
#want to have the subsamples combined (summed), but because sometimes we lost traps (weather, accidental loss)
#not all plots will have the same number of subsamples
#we will process our data set so that we've got our subsamples combined by plot date etc and create a vector with counts
library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
lampyrid1<-melt(lampyrid, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY", "week"))
#cast the data to count up the fireflies
lampyrid2<-dcast(lampyrid1, year+DOY+week+TREAT_DESC+HABITAT+REPLICATE~., sum)
#cast the data to count the traps
lampyrid3<-dcast(lampyrid1, year+DOY+week+TREAT_DESC+HABITAT+REPLICATE~., length)

#let's rename these new vectors within the data frame
names(lampyrid2)[7]<-"ADULTS"
names(lampyrid3)[7]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from lampyrid3
lampyrid<-lampyrid2
lampyrid$TRAPS<-lampyrid3$TRAPS

#download weather data from KBS weather station
weather<-read.table(file="http://lter.kbs.msu.edu/datatables/7.csv",
                    header=T, sep=",", na.strings="")
#extract day of year, so we have a continuous variable running for each year.
#since we're in a temperate northern climate, this is convenient- not too 
#much insect action happening at the december-january transition, so we 
#can use the yearly break as a blocking variable for rowing season.
#it's convenient living where we do! 

weather$DOY<-yday(weather$date)
weather$week<-isoweek(weather$date)
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
#then add it to dd2
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

accum.allen<-function(maxi, mini, thresh, DOY, startday){
  #if startday is not given, assume it's day 1
  if(missing(startday)) {
    startday<-1
  } else {
    startday<-startday
  }
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
    
    #but if the degdays are accumulating before the startday, we want to forget them
    if (DOY[i]<startday){
      dd.accum.day=0
    }
    #add that day's accumulation to the vector
    dd.accum<-c(dd.accum, dd.accum.day)
  }
  return (dd.accum)
}
 
#same sort of checks. Run the function for our data

weather$dd.accum<-accum.allen(weather$air_temp_max_clean, weather$air_temp_min_clean, 10, weather$DOY, start)
 #and plot that thing to look for problems:
plot(weather$DOY, weather$dd.accum)
#looks good! victory!!!

#let's also compute degree day accumulation from the beginning of year- we may need to see how the winter affected 
#the lampyrids if we can't explain all the variation
weather$dd.accum0<-accum.allen(weather$air_temp_max_clean, weather$air_temp_min_clean, 10, weather$DOY, 1)


#but what about preciptiation? our lit search indicated that there could be
#important things going on with rain and lampyrids. First let's calculate the 
#precipitation accumulation over the week, then let's look at the number of rainy 
#days in a week

accum.precip<-function (precip, week){
    precip.acc<-c()
    counter<-week[1]
    accumulation<-0
    for (i in 1:length(precip)){
      if(week[i]==counter){
        accumulation<-accumulation + precip[i]
      }else{
        counter<-week[i]
        accumulation<-precip[i]
      }
      precip.acc<-c(precip.acc, accumulation)
    }
    return(precip.acc)
}

#run the precipitation accumulation function
weather$prec.accum<-accum.precip(weather$precipitation, weather$week)


#looks good! now let's count rainy days
#this is a simple thing, doesn't really need a function to encode for it, but what the heck
#might as well be consistent with how we've handled processing other weather data
#encoding rain days as 0/1 will allow us to simply sum up the number of rainy days for whatever time 
#period we like

rainy.days<-function (precip, week){
  rainy.days<-c()
  for (i in 1:length(precip)){
    if(precip[i]>0){
      raindays<-1
    }else{
      raindays<-0
    }
    rainy.days<-c(rainy.days, raindays)
  }
  return(rainy.days)
}

#and now the rain day counter
weather$rain.days<-rainy.days(weather$precipitation, weather$week)

#finally, we need to be able to compute the accumulated precipitation over the season from a given timepoint
#another function? I think SO! base this one on the degree day accumulation function 


accum.precip.time<-function(precip, DOY, startday){
  #if startday is not given, assume it's day 1
  if(missing(startday)) {
    startday<-1
  } else {
    startday<-startday
  }
  prec.accum<-c()
  for (i in 1:length(DOY)){
    #hmm, need a way to sum up over the year, starting anew for each year.
    #this should do it
    if (DOY[i]==1){
      prec.accum.day=0
    }
    #the accumulation on day i is the precip accumulation before
    #plus the precip accumulated on that day
    prec.accum.day<-prec.accum.day+precip[i]
    
    #but if the precip is accumulating before the startday, we want to forget them
    if (DOY[i]<startday){
      prec.accum.day=0
    }
    #add that day's accumulation to the vector
    prec.accum<-c(prec.accum, prec.accum.day)
  }
  return (prec.accum)
}

weather$prec.accum.0<-accum.precip.time(weather$precipitation, weather$DOY, start)
#and plot that thing to look for problems:
plot(weather$DOY, weather$prec.accum.0)

#now we need to summarize the weather data so it gives us the information we want at a weekly resolution
#just like we have for the fireflies
library(plyr)

weather1<-ddply(weather, c("year", "week"), summarise,
                Tmax=max(air_temp_max_clean), Tmin=min(air_temp_min_clean), 
                dd.accum=max(dd.accum), prec.accum=max(prec.accum), 
                rain.days=sum(rain.days), prec.accum.0=max(prec.accum.0))


#so, now we have two datasets that both have information we need in them.
#let's put it all together in one frame


lampyrid.weather<-merge(lampyrid, weather1, by=c("year","week"), all.x=TRUE)

#let's take a look at our data now and see what patterns we can see

library(ggplot2)

#create a palatte based on colour brewer. We want to use 'Spectral' for year data
#but we have one extra year, so we have to create a palette manually
#just extract the hex from colorbrewer, and find an additional shade that works on one of the ends

pal<-c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695', '#050561')
pal1<-c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2', '#a3297a')

#plot raw 
lampyrid.doy<-ggplot(lampyrid.weather, aes(DOY, ADULTS, fill=as.factor(year)))+
  scale_fill_manual(values=pal)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  facet_wrap(~year)+
  guides(fill=FALSE)+
  xlab("Day")+
  ylab("# Adults captured")
lampyrid.doy

#save to pdf
pdf("lampyriddoy.pdf", height=6, width=8)
lampyrid.doy
dev.off()

#plot by sample week
lampyrid.week<-ggplot(lampyrid.weather, aes(week, ADULTS, fill=factor(year)))+
  scale_fill_manual(values=pal)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  facet_wrap(~year)+
  guides(fill=FALSE)+
  xlab("Week")+
  ylab("# Adults captured")
lampyrid.week

#save to pdf
pdf("lampyridweek.pdf", height=6, width=8)
lampyrid.week
dev.off()

# we're interested in looking at more general trends. We'll need to produce 
#summary data to do this


captures.by.year<-ddply(lampyrid.weather, c("year"), summarise,
                        total=sum(ADULTS), traps=sum(TRAPS), avg=sum(ADULTS)/sum(TRAPS), ddacc=max(dd.accum))

captures.by.week.year<-ddply(lampyrid.weather, c("year", "week"), summarise,
                             total=sum(ADULTS), traps=sum(TRAPS), 
                             avg=sum(ADULTS)/sum(TRAPS),
                             ddacc=max(dd.accum), rain.days=max(rain.days))


#look at captures by week, over the growing season, by year
lampyrid.summary.week<-ggplot(captures.by.week.year, aes(week, avg, 
                                                         fill=as.factor(year)))+
  scale_fill_manual(values=pal)+
  geom_smooth(colour="black", se=FALSE)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Year"))+
  theme(legend.key=element_blank())+
  xlab("\nWeek")+
  ylab("Adults per trap\n")

lampyrid.summary.week

#save to pdf
pdf("lampyridsummaryweek.pdf", height=6, width=8)
lampyrid.summary.week
dev.off()

#look at captures by degree day accumulation to see if our activity pattern is clearer

lampyrid.summary.ddacc<-ggplot(captures.by.week.year, aes(ddacc, avg, 
                                                          fill=as.factor(year)))+
  scale_fill_manual(values=pal)+
  geom_smooth(colour="black", se=FALSE)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Year"))+
  theme(legend.key=element_blank())+
  xlab("\nDegree day accumulation")+
  ylab("Adults per trap\n")

lampyrid.summary.ddacc

#save to pdf
pdf("lampyridsummaryddacc.pdf", height=6, width=8)
lampyrid.summary.ddacc
dev.off()

#we want to stack these figures together because they are a driect comparison of the predictivity of these two factors
#since this is a ggplot, we'll need to use arrangegrob. we can alter the panels before feeding them to arrangegrob
#to remove redundant information and to add labels
library(gridExtra)


#remove legend from panel A, add label
lampyrid.summary.week1<-lampyrid.summary.week+guides(fill=FALSE)+annotate("text", x=20, y=4.2, label="A", size=14)
#remove Y axis title from panel B, add label
lampyrid.summary.ddacc1<-lampyrid.summary.ddacc+ylab(NULL)+annotate("text", x=255, y=4.2, label="B", size=14)
#stack it together
grid.arrange(arrangeGrob(lampyrid.summary.week1, lampyrid.summary.ddacc1, ncol=2, widths=c(0.49, 0.55)))


#save to pdf
pdf("lampyridsummaryweekandddacc.pdf", height=6, width=10)
grid.arrange(arrangeGrob(lampyrid.summary.week1, lampyrid.summary.ddacc1, ncol=2, widths=c(0.49, 0.55)))
dev.off()


#we want to look at captures by treatment 
#when we look at it by plant community (habitat), things get a little wackier because of the three year crop rotation. 
#It looks like we get very good beahvior of the loess when we use TREAT_DESC

captures.by.treatment<-ddply(lampyrid.weather, c("year", "TREAT_DESC"), summarise,
                             total=sum(ADULTS), traps=sum(TRAPS), avg=sum(ADULTS)/sum(TRAPS))

# let's look at captures by treatment in the broadest sense first

treatment.boxplot<-ggplot(captures.by.treatment, aes(factor(TREAT_DESC), avg))+
  scale_fill_brewer(palette="Set3")+
  geom_boxplot(aes(fill=factor(TREAT_DESC)), colour="black")+
  theme_bw(base_size = 20)+
  guides(fill=FALSE)+
  xlab("\nTreatment")+
  ylab("Adults per trap\n")+
  theme(axis.text.x=element_text(angle=90))

treatment.boxplot

#save to pdf
pdf("treatmentboxplot.pdf", height=6, width=8)
treatment.boxplot
dev.off()

#looks to me like we are most likely to capture fireflies in annual herbaceous crops with the least soil disturbance
#alfalfa, and no till. Hmm.


#and now we look at captures by treatment over the years

lampyrid.summary.treatment<-ggplot(captures.by.treatment, aes(year, avg, 
                                                              fill=as.factor(TREAT_DESC)))+
  scale_fill_brewer(palette="Set3")+
  geom_smooth(colour="black", se=FALSE)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Treatment"))+
  theme(legend.key=element_blank())+
  xlab("\nYear")+
  ylab("Adults per trap\n")
lampyrid.summary.treatment

#save to pdf
pdf("lampyridsummarytreatment.pdf", height=6, width=8)
lampyrid.summary.treatment
dev.off()

#an interesting population cycling pattern emerges, but it doesn't look like there's major changes of crop use
#At least not at the yearly resolution
#we can investigate this futher with a multivariate analysis later
#regardless of how we plot it, we see an interesting pattern in the population variation- basically, a 6-7 year oscillation.
#so the question is, is there and obvious environmental cause?

#we want to look at captures by treatment relative to degree day accumulation too- are peaks earlier or later by crop? 

captures.by.treatment.dd<-ddply(lampyrid.weather, c("year","week","TREAT_DESC"), summarise,
                             total=sum(ADULTS), traps=sum(TRAPS), avg=sum(ADULTS)/sum(TRAPS), ddacc=max(dd.accum))



lampyrid.summary.treatment.dd<-ggplot(captures.by.treatment.dd, aes(ddacc, avg, 
                                                                    fill=as.factor(TREAT_DESC)))+
  scale_fill_brewer(palette="Set3")+
  geom_point(colour="black", pch=21, size=4)+
  geom_smooth(colour="black", se=FALSE)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Treatment"))+
  theme(legend.key=element_blank())+
  xlab("\nDegree day accumulation")+
  ylab("Adults per trap\n")
lampyrid.summary.treatment.dd

#save to pdf
pdf("lampyridsummarytreatmentdd.pdf", height=6, width=8)
lampyrid.summary.treatment.dd
dev.off()

#it looks like peaks by degree day accumulation is roughly synced by crop. We'll need to quantify how crop 
#use varies between crops but it looks like these factors do not interact with time. Good! makes our analysis
#more strightforward

#Let's see if there's anyting obvious in the weather data that explains the population cycling over time 
#that we saw above

#compute yearly weather summary from weather data (do't want this calulation to be affectred by length of sampling season)
weather.by.year<-ddply(weather1, c("year"), summarise,
                        precip=sum(prec.accum), rain.days=sum(rain.days), ddacc=max(dd.accum))

#plot degree day accumulations by year, see if that explains it

ddacc.summary.year<-ggplot(weather.by.year, aes(x=as.factor(year), y=ddacc, fill=as.factor(year)))+
  scale_fill_manual(values=pal)+
  geom_bar(stat="identity", colour="black")+
  theme_bw(base_size = 20)+
  guides(fill=FALSE)+
  ylab("\nDegree day accumulation\n")+
  xlab("\nYear\n")+
  theme(axis.text.x=element_text(angle=90))

ddacc.summary.year


#what about amount of precipitation? say number of rainy days
rainday.summary.year<-ggplot(weather.by.year, aes(x=as.factor(year), y=rain.days, fill=as.factor(year)))+
  scale_fill_manual(values=pal)+
  geom_bar(stat="identity", colour="black")+
  theme_bw(base_size = 20)+
  guides(fill=FALSE)+
  ylab("\nNumberof rainy days\n")+
  xlab("\nYear\n")+
  theme(axis.text.x=element_text(angle=90))

rainday.summary.year


#and total precipitation
precip.summary.year<-ggplot(weather.by.year, aes(x=as.factor(year), y=precip, fill=as.factor(year)))+
  scale_fill_manual(values=pal)+
  geom_bar(stat="identity", colour="black")+
  theme_bw(base_size = 20)+
  guides(fill=FALSE)+
  ylab("\nTotal precipitation (mm)\n")+
  xlab("\nYear\n")+
  theme(axis.text.x=element_text(angle=90))

precip.summary.year


#is there a relationship between rain and degree day accumulation? 
plot(weather.by.year$precip,weather.by.year$ddacc)
#not much, though there are a few hot-dry and a few cold-wet years
#I don't think we need to go down this rabbit hole for the present analysis
 








