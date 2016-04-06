# lampyrid analysis
#bring data in from figshare
lampyrid<-read.csv(file="https://ndownloader.figshare.com/files/3686040",
                   header=T)

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
#use ISO week, so we start counting on Monday, not Jan 1, COOL!
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
 
#same sort of checks. Run the function for our data, giving it a sart day of Mar 15 (DOY 74)

weather$dd.accum<-accum.allen(weather$air_temp_max_clean, weather$air_temp_min_clean, 10, weather$DOY, 74)
 #and plot that thing to look for problems:
plot(weather$DOY, weather$dd.accum)
#looks good! victory!!!

#but what about preciptiation? our lit search indicated that there could be
#important things going on with rain and lampyrids. Firstl let's calculate the 
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

#now we need to summarize the weather data so it gives us the information we want at a weekly resolution
#just like we have for the fireflies
library(plyr)

weather1<-ddply(weather, c("year", "week"), summarise,
                Tmax=max(air_temp_max_clean), Tmin=min(air_temp_min_clean), 
                dd.accum=max(dd.accum), prec.accum=max(prec.accum), 
                rain.days=sum(rain.days))


#so, now we have two datasets that both have information we need in them.
#let's put it all together in one frame


lampyrid.weather<-merge(lampyrid, weather1, by=c("year","week"), all.x=TRUE)

#let's take a look at our data now and see what patterns we can see

library(ggplot2)

#plot raw 
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


captures.by.year<-ddply(lampyrid.weather, c("year"), summarise,
                        total=sum(ADULTS), traps=sum(TRAPS), avg=sum(ADULTS)/sum(TRAPS), ddacc=max(dd.accum))

captures.by.week.year<-ddply(lampyrid.weather, c("year", "week"), summarise,
                             total=sum(ADULTS), traps=sum(TRAPS), 
                             avg=sum(ADULTS)/sum(TRAPS),
                             ddacc=max(dd.accum), rain.days=max(rain.days))


#look at captures by week, over the growing season, by year
lampyrid.summary.week<-ggplot(captures.by.week.year, aes(week, avg, 
                                                         color=factor(year)))+
  geom_point()+geom_smooth(se=FALSE)
lampyrid.summary.week

#look at captures by degree day accumulation

lampyrid.summary.ddacc<-ggplot(captures.by.week.year, aes(ddacc, avg, 
                                                          color=factor(year)))+
  geom_point()+geom_smooth(se=FALSE)
lampyrid.summary.ddacc

#we want to look at captures by treatment 
#when we look at it by plant community (habitat), things get a little wackier because of the three year crop rotation. 
#It looks like we get very good beahvior of the loess when we use TREAT_DESC

captures.by.treatment<-ddply(lampyrid.weather, c("year", "TREAT_DESC"), summarise,
                             total=sum(ADULTS), traps=sum(TRAPS), avg=sum(ADULTS)/sum(TRAPS))

lampyrid.summary.treatment<-ggplot(captures.by.treatment, aes(year, avg, 
                                                              color=factor(TREAT_DESC)))+
  geom_point()+geom_smooth(se=FALSE)
lampyrid.summary.treatment


captures.by.habitat<-ddply(lampyrid.weather, c("year", "HABITAT"), summarise,
                           total=sum(ADULTS), traps=length(ADULTS), avg=sum(ADULTS)/length(ADULTS))

lampyrid.summary.habitat<-ggplot(captures.by.habitat, aes(year, avg, 
                                                          color=factor(HABITAT)))+
  geom_point()+geom_smooth(se=FALSE)
lampyrid.summary.habitat

#we want to look at captures by treatment relative to degree day accumulation too- are peaks earlier or later by crop? 

captures.by.treatment.dd<-ddply(lampyrid.weather, c("year","week","TREAT_DESC"), summarise,
                             total=sum(ADULTS), traps=sum(TRAPS), avg=sum(ADULTS)/sum(TRAPS), ddacc=max(dd.accum))

lampyrid.summary.treatment.dd<-ggplot(captures.by.treatment.dd, aes(ddacc, avg, 
                                                              color=factor(TREAT_DESC)))+
  geom_point()+geom_smooth(se=FALSE)
lampyrid.summary.treatment.dd

#regardless of how we plot it, we see an interesting pattern in the population variation- basically, a 6-7 year oscillation.
#so the question is, is there and obvious environmental cause?
#compute yearly weather summary from weather data (dont want this calulation to be affectred by length of sampling season)
weather.by.year<-ddply(weather1, c("year"), summarise,
                        precip=sum(prec.accum), rain.days=sum(rain.days), ddacc=max(dd.accum))

#plot degree day accumulations by year, see if that explains it

ddacc.summary.year<-ggplot(weather.by.year, aes(x=as.factor(year), y=ddacc, fill=as.factor(year)))+
  geom_bar(stat="identity")
ddacc.summary.year

#what about amount of precipitation? say number of rainy days
rainday.summary.year<-ggplot(weather.by.year, aes(x=as.factor(year), y=rain.days, fill=as.factor(year)))+
  geom_bar(stat="identity")
rainday.summary.year

#and total precipitation
precip.summary.year<-ggplot(weather.by.year, aes(x=as.factor(year), y=precip, fill=as.factor(year)))+
  geom_bar(stat="identity")
precip.summary.year

plot(weather.by.year$rain.days,weather.by.year$ddacc)
 
treatment.boxplot<-ggplot(captures.by.treatment, aes(factor(TREAT_DESC), total))+
  geom_boxplot(aes(fill=factor(TREAT_DESC)))
treatment.boxplot


#multivariate analysis. So we want to see if the habitat use patterns of the lampyrids have
#changed, both within season and through the years
#to do this, we'll need to reshape the data into two different matrices where we have 
#abundance of fireflies by TREAT_DESC at yearly and weekly resolutions- a cros-tab,
#wide format data. 

#start by building the matrices
#we can use our previously melted data fram 'lampyrid1' and cast it as needed
#because of unequal numbers of reps between forest and main sites, but same number of subsamples 
#per rep, we'll treat subsamples as rep for this analysis and pool by rep instead

#cast at the yearly resolution first
landscape.year<-dcast(lampyrid1, year+STATION~TREAT_DESC, sum)
landscape.week<-dcast(lampyrid1, year+week+STATION~TREAT_DESC, sum)

#there are some weeks where zero fireflies were captured. We need to remove these 
#weeks from the matrix before we can continue-

landscape.week$sums<-rowSums(landscape.week[4:13])
landscape.week<-landscape.week[which(landscape.week$sums>0),]
landscape.week$sums<-NULL

#now we need to create 'environmental' matricies- corresponding environmental 
#variables that may offer explanations about what is going on when we run our 
#multivariate analysis
#we already computed 'weather.by.year' but will need to also compute the same for 
#our weekly analysis
weather.by.week<-ddply(weather1, c("year", "week"), summarise,
                       precip=sum(prec.accum), rain.days=sum(rain.days), ddacc=max(dd.accum))

#now create the environmental matrix, preserving order from the community matricies by
#creating them from the community matrix

env.landscape.year<-landscape.year[,1:2]
env.landscape.week<-landscape.week[,1:3]

#we now need to pull our weather summary data into these matrices
env.landscape.year<-merge(env.landscape.year, weather.by.year, by=c("year"), all.x=TRUE)
env.landscape.week<-merge(env.landscape.week, weather.by.week, by=c("year", "week"), all.x=TRUE)

#finally strip out the env data
landscape.year<-landscape.year[,3:12]
landscape.week<-landscape.week[,4:13]

#Ok! data is ready for some NMDSing! WOOO
library(vegan)

ord.year<-metaMDS(landscape.year, autotransform=TRUE)
ord.year
#so, MetaMDS assumes the x axis of our matrix is species and y is sites. We are
#screwing with this by instead looking at sites over samples for one species. So when I call "sites"
#here I'm actually calling sampling times. Just thought you should know

plot(ord.year, disp='sites', type='n')
ordihull(ord.year,groups=env.landscape.year$year,draw="polygon",col="grey90",label=T)
points(ord.year, display='sites', select=which(env.landscape.year$year=="2004"), col="red")
points(ord.year, display='sites', select=which(env.landscape.year$year=="2005"), col="orange")
points(ord.year, display='sites', select=which(env.landscape.year$year=="2006"), col="yellow")
points(ord.year, display='sites', select=which(env.landscape.year$year=="2007"), col="green")
points(ord.year, display='sites', select=which(env.landscape.year$year=="2008"), col="cyan")
points(ord.year, display='sites', select=which(env.landscape.year$year=="2009"), col="blue")
points(ord.year, display='sites', select=which(env.landscape.year$year=="2010"), col="violet")
points(ord.year, display='sites', select=which(env.landscape.year$year=="2011"), col="purple")
points(ord.year, display='sites', select=which(env.landscape.year$year=="2012"), col="black")
points(ord.year, display='sites', select=which(env.landscape.year$year=="2013"), col="brown")
points(ord.year, display='sites', select=which(env.landscape.year$year=="2014"), col="pink")
points(ord.year, display='sites', select=which(env.landscape.year$year=="2015"), col="tan")
ordilabel(ord.year, display="species", cex=0.75, col="black")


#repeat with week?

ord.week<-metaMDS(landscape.week, autotransform=TRUE)
ord.week
#extract data for ggplot
ord.week.data<- data.frame(MDS1 = ord.week$points[,1], MDS2 = ord.week$points[,2])
#so, MetaMDS assumes the x axis of our matrix is species and y is sites. We are
#screwing with this by instead looking at sites over samples for one species. So when I call "sites"
#here I'm actually calling sampling times. Just thought you should know
ordfit.week<-envfit(ord.week~as.factor(year)+week+precip+ddacc, data=env.landscape.week, perm=1000)
summary(ordfit.week)
ordfit.week
ordfit.week.data<-as.data.frame(ordfit.week$vectors$arrows*sqrt(ordfit.week$vectors$r))
ordfit.week.data$species<-rownames(ordfit.week.data)

#lets's make this go! https://oliviarata.wordpress.com/2014/04/17/ordinations-in-ggplot2/ is tutorial I'm using here

#ggplot(ord.week.data, aes(MDS1, MDS2, color=as.factor(env.landscape.week$year)))+
#  geom_point()+
#  geom_segment(data=ordfit.week.data,aes(x=0,xend=NMDS1,y=0,yend=NMDS2),
               #arrow = arrow(length = unit(0.5, "cm")),
#               colour="grey",inherit_aes=FALSE) + 
# geom_text(data=ordfit.week.data,aes(x=NMDS1,y=NMDS2,label=species),size=5)+
#  coord_fixed()



#finally, let's do some generalized linear modelling to see what's important and if we can explain what's going on
#we've clearly got a quadratic resonse to degree day accumulation, and since we're dealing with count data, we should model 
#it using a poisson structure (or negative binomial if we've got a high residual deviance)
#we'll use the MASS package

library(MASS)
#create a squared term so we can build a model with a quadratic in it
lampyrid.weather$dd.accum2<-(lampyrid.weather$dd.accum)^2




#After some initial fiddling, we find out that rain.days is a better predictor than precipitation accumulation, and given that these are 
#seriously autocorrelated, let's just use rain days
#we know TREAT_DESC is probably not important in interacting with dd.acc as we did not observe major tends by treatment when we looked at 
#trends in captures by degree day accumulation by  treatment so we won't look for interactions
#finally, because of convergence problems using glm.nb, we determined theta (dispersion parameter) iteratively
#using glm with a negative binomial family instead. Less elegant and more labour intensive- but really brought residual deviance and AIC
#values down, indicating a much better fit

lam_model<-glm(ADULTS~dd.accum+dd.accum2*(as.factor(year))+TREAT_DESC, 
               offset=TRAPS, data=lampyrid.weather, family=negative.binomial(0.6))
summary(lam_model)


#Let's just do a quick look to see how our model predictions look
x<-(1:length(lampyrid.weather$DOY))
lampyrid.weather$predicted<-(exp(predict(lam_model,lampyrid.weather)))

plot(x, lampyrid.weather$predicted, ylim=c(0, 100))
plot(x, lampyrid.weather$ADULTS, ylim=c(0, 100))

test<-lm(predicted~0+ADULTS, data=lampyrid.weather)
summary(test)

#Let's see how well the model works when we look at data with a lower resolution (to damp out a bit of sampling variability)

lampyrid.weather.summary<-ddply(lampyrid.weather, c("year", "week", "TREAT_DESC"), summarise,
                             ADULTS=sum(ADULTS), TRAPS=sum(TRAPS), predicted=sum(predicted),
                             avg=sum(ADULTS)/sum(TRAPS), avgpred=sum(predicted)/sum(TRAPS),
                             dd.accum=max(dd.accum), rain.days=max(rain.days))


lampyrid.summary.ddacc<-ggplot(lampyrid.weather.summary, aes(dd.accum, avg, 
                                                          color=factor(year)))+
  geom_point()+
  geom_smooth(se=FALSE)
lampyrid.summary.ddacc


lampyrid.summary.ddacc.PRED<-ggplot(lampyrid.weather.summary, aes(dd.accum, avg, 
                                                     color=factor(year)))+
  geom_point()+
  geom_smooth(aes(dd.accum, avgpred), se=FALSE)
lampyrid.summary.ddacc.PRED

#Cool! So now we want to see how the peak is varying by year, and see if there are any environmental parameters that explain it
#we first need to extract the coefficients from the lam_model

coef<-as.data.frame(summary(lam_model)$coefficients)
#get rid of those pesky t and P statistics
coef<-coef[,1:2]



ddcoef<-coef$Estimate[2]
dd2coef<-coef$Estimate[3]
ddcoef.err<-coef$"Std. Error"[2]
dd2coef.err<-coef$"Std. Error"[3]

#create a vector of years
year<-(2004:2015)

#create vector of coefficients
#remember 2004 is the 'intercept' vector so it's unmodified, we'll give it a year 
#modifier and error of zero

yearcoef<-c(0, coef$Estimate[24:34])
yearcoef.err<-c(0, coef$"Std. Error"[24:34])

#create a new data frame to integrate the coeficients with the year vector
peaks<-as.data.frame(cbind(year, yearcoef, yearcoef.err))

#peak will occur at -ddcoeficient/(2(dd2coeficient+year coeficient))
peaks$peak<- -ddcoef/(2*(dd2coef+yearcoef))

#peak error calculated using the general error propagation formula
#this will be a bit inelegant, but I calculated the partial derrivatives 
#relative to each variable myself!
peaks$peak.err<-sqrt((2*(dd2coef+yearcoef))^(-2) *ddcoef.err^2+
                       (ddcoef/(2*(dd2coef+yearcoef))^2)^2*(dd2coef.err^2+yearcoef.err^2))

#let's visualize this!

peaks.year<-ggplot(peaks, aes(x=as.factor(year), y=peak, fill=as.factor(year)))+
  geom_bar(stat="identity")+geom_errorbar(aes(ymin=peak-peak.err, ymax=peak+peak.err))
peaks.year

climate.test<-lm(peak~year, data=peaks)
summary(climate.test)
