#
# Program: Vermont.R
#
# Purpose:  Comparing smoothing methods and regression methods of a one-day
#           forecast for the daily peak demand of Vermont
#
# Description: Several forecasting methods will be evaluated and compared with
#              one another. The two types of forecasting methods/models that  
#              will be analyzed are smoothing methods and regression models.
#              The benchmark is redefined as the datasets were changed. In 
#              addition, more variables are added.
#
# Written by: Yuan Wang
#             on March 30, 2020
# 
# ------------------------------------------------------------------------------

rm(list=ls())
setwd("/Users/yuanwong/Downloads/forecasting")
wd=getwd()

Sys.setlocale("LC_TIME", "C")
library(timeSeries)
library(dplyr)
library(lubridate)
library(forecast)
library(zoo)
library(ggplot2)
library(psy)
library(astsa)
library(lmtest)

read.df <- function(years, path="/Users/yuanwong/Downloads/forecasting") {
  df_comb <- data.frame()
  for (year in years) { 
    df_yr <- read.csv(paste(path, year, "_VT.csv", sep=""),
                      colClasses=c("character",rep("numeric",13)),
                      skip=1)
    names(df_yr)<-c("Date","Hour","DA_VERMONT","Vermont","DA_LMP","DA_EC","DA_CC","DA_MLC","LMP","EC","CC","MLC","DryBulb","DewPt")
    if(year<2016) {
      df_yr$Date <- format(as.Date(df_yr$Date,"%m/%d/%Y"), "%Y-%m-%d")
    } else {
      df_yr$Date <- format(as.Date(df_yr$Date,"%d-%b-%y"), "%Y-%m-%d")
    }
    df_comb <- rbind(df_comb,df_yr)
    df_comb[df_comb$Vermont==0,3:14]<-df_comb[(which(df_comb$Vermont==0))-168,3:14]
    
  }
  df_comb <-df_comb[!format(as.Date(df_comb$Date),"%m-%d") == "02-29",]
  df_comb[df_comb$Date=="2011-11-06" & df_comb$Hour==2,3:14]<-df_comb[(which(df_comb$Date=="2011-11-06" & df_comb$Hour==2))-168,3:14]
  df_comb[df_comb$Date=="2012-11-04" & df_comb$Hour==2,3:14]<-df_comb[(which(df_comb$Date=="2012-11-04" & df_comb$Hour==2))-168,3:14]
  df_comb[df_comb$Date=="2013-11-03" & df_comb$Hour==2,3:14]<-df_comb[(which(df_comb$Date=="2013-11-03" & df_comb$Hour==2))-168,3:14]
  df_comb[df_comb$Date=="2014-11-02" & df_comb$Hour==2,3:14]<-df_comb[(which(df_comb$Date=="2014-11-02" & df_comb$Hour==2))-168,3:14]
  df_comb[df_comb$Date=="2015-11-01" & df_comb$Hour==2,3:14]<-df_comb[(which(df_comb$Date=="2015-11-01" & df_comb$Hour==2))-168,3:14]
  return(df_comb)
}

df <- read.df(2011:2019)

df.train <- read.df(2011:2016)
df.valid <- read.df(2017:2018)
df.test <- read.df(2019)

## daily maximum value for each variable

df.max <- aggregate(df[,3:14],by=list(Date=df$Date),FUN=max)

df.train_max <- df.max[1:2190,]

## corresponding values of each variables at daily peak demand

df.peak <-do.call(rbind,lapply(split(df,df$Date),function(chunk) chunk[which.max(chunk$Vermont),]))
rownames(df.peak)<-c()

df.train_peak <- df.peak[1:2190,]

Vermont <- df.peak$Vermont

## daily mean of each variable

df.mean <- merge(merge(aggregate(DA_VERMONT~Date,data=df,FUN=mean),
                             aggregate(Vermont~Date,data=df,FUN=max),by="Date"),
                       aggregate(df[,5:14],by=list(Date=df$Date),FUN=mean),by="Date")
df.train_mean <- df.mean[1:2190,]

# creating HDD and CDD 

DryBulb <- df.mean$DryBulb
plot(DryBulb,Vermont)
#ggplot(df.mean, aes(x = DryBulb, y = Vermont)) + geom_point() + scale_x_continuous(name="Temp Dry Bulb", limits=c(30, 80)) +
#  scale_y_continuous(name="Vermont daily demand", limits=c(500, 800))
HDD=52.5-DryBulb
CDD=DryBulb-57.5
HDD[HDD<=0]<-0
CDD[CDD<=0]<-0

# creating 3 lags for HDD and CDD

HDD.1<-lag(HDD,1)
CDD.1<-lag(CDD,1)
HDD.2<-lag(HDD,2)
CDD.2<-lag(CDD,2)
HDD.3<-lag(HDD,3)
CDD.3<-lag(CDD,3)

# additional weather data

additional1 <- read.csv("2092252.csv",header=TRUE,sep=",")
additional1$DATE= as.Date(additional1$DATE)
additional1 <- additional1[!format(as.Date(additional1$DATE),"%m-%d") == "02-29",]
additional1$DailyAverageRelativeHumidity[is.na(additional1$DailyAverageRelativeHumidity)]=0
Humidity <- aggregate(additional1$DailyAverageRelativeHumidity,by=list(Date=additional1$DATE),FUN=max)
Humidity <- Humidity$x
plot(Humidity,Vermont)
HI <- Humidity^(1/2)*(DryBulb-52.5)
#ggplot(data = df.mean, aes(x = HI, y = df.mean$Vermont)) + geom_point() + scale_x_continuous(name="Temp Dry Bulb", limits=c(0, 200)) +
#  scale_y_continuous(name="Vermont daily demand", limits=c(500, 900))
HI <- HI - 150 
HI[HI < 0] <- 0
plot(HI,Vermont, xlab="Heat Index (HI)", ylab="Daily peak demand of Vermont")

additional <- read.csv("2029335.csv",header=TRUE,sep=",")
additional <- select(additional,-c("STATION","NAME","TAVG"))
additional <- additional[!format(as.Date(additional$DATE),"%m-%d") == "02-29",]
additional.train <- additional[1:2190,]

AWND <- additional$AWND
PRCP <- additional$PRCP
plot(AWND,Vermont)
wc <- AWND^(1/2)*(57.5-DryBulb)
wc[wc<0] <- 0
plot(wc,Vermont,xlab="Wind Chill (wc)", ylab="Daily peak demand of Vermont")

weathertype0=select(additional,-c(1,2,3,4,5))
wt1=apply(weathertype0[,c(1,2)],1,max)
wt2=apply(weathertype0[,c(4,6)],1,max)
wt3=weathertype0$WT08
wt4=weathertype0$WT09
wt5=weathertype0$WT11
wt6=apply(weathertype0[,c(10,11,13)],1,max)
wt7=weathertype0$WT15
wt8=weathertype0$WT17
wt9=apply(weathertype0[,c(15,18)],1,max)
wt10=weathertype0$WT21
wt=data.frame(wt1,wt2,wt3,wt4,wt5,wt6,wt7,wt8,wt9,wt10)
wt[is.na(wt)]<-0

# Dummy variables for days of the week and months

Date <- df.peak$Date

dow<-weekdays(as.Date(Date))
dow <- factor(dow,
              levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
dow <- ifelse(dow=="Sunday",1,ifelse(dow=="Monday",2,ifelse(dow=="Tuesday",3,ifelse(dow=="Wednesday",4,ifelse(dow=="Thursday",5,ifelse(dow=="Friday",6,7))))))
dow <- as.factor(dow)

mon <- months(as.Date(Date),abbreviate=T)
mon <- ifelse(mon=="Jan",1,ifelse(mon=="Feb",2,ifelse(mon=="Mar",3,ifelse(mon=="Apr",4,ifelse(mon=="May",5,ifelse(mon=="Jun",6,ifelse(mon=="Jul",7,ifelse(mon=="Aug",8,ifelse(mon=="Sep",9,ifelse(mon=="Oct",10,ifelse(mon=="Nov",11,12)))))))))))
mon <- as.factor(mon)

# Dummy variables for fixed-date holidays and moving holidays

FH<-ifelse(format(as.Date(Date),"%m-%d") == "01-01",1,
           ifelse(format(as.Date(Date),"%m-%d") == "12-25",1,
                  ifelse(format(as.Date(Date),"%m-%d") == "12-24",1,
                         ifelse(format(as.Date(Date),"%m-%d") == "11-11",1,
                                ifelse(format(as.Date(Date),"%m-%d") == "08-16",1,
                                       ifelse(format(as.Date(Date),"%m-%d") == "07-04",1,0))))))

VH<-ifelse(timeDate(Date)==USMLKingsBirthday(2011),1,
           ifelse(timeDate(Date)==USMLKingsBirthday(2012),1,
                  ifelse(timeDate(Date)==USMLKingsBirthday(2013),1,
                         ifelse(timeDate(Date)==USMLKingsBirthday(2014),1,
                                ifelse(timeDate(Date)==USMLKingsBirthday(2015),1,
                                       ifelse(timeDate(Date)==USMLKingsBirthday(2016),1,
                                              ifelse(timeDate(Date)==USPresidentsDay(2011),1,
                                                     ifelse(timeDate(Date)==USPresidentsDay(2012),1,
                                                            ifelse(timeDate(Date)==USPresidentsDay(2013),1,           
                                                                   ifelse(timeDate(Date)==USPresidentsDay(2014),1,
                                                                          ifelse(timeDate(Date)==USPresidentsDay(2015),1,
                                                                                 ifelse(timeDate(Date)==USPresidentsDay(2016),1,
                                                                                        ifelse(timeDate(Date)==USMemorialDay(2011),1,
                                                                                               ifelse(timeDate(Date)==USMemorialDay(2012),1,
                                                                                                      ifelse(timeDate(Date)==USMemorialDay(2013),1,           
                                                                                                             ifelse(timeDate(Date)==USMemorialDay(2014),1,
                                                                                                                    ifelse(timeDate(Date)==USMemorialDay(2015),1,
                                                                                                                           ifelse(timeDate(Date)==USMemorialDay(2016),1,         
                                                                                                                                  ifelse(timeDate(Date)==USLaborDay(2011),1,
                                                                                                                                         ifelse(timeDate(Date)==USLaborDay(2012),1,
                                                                                                                                                ifelse(timeDate(Date)==USLaborDay(2013),1,           
                                                                                                                                                       ifelse(timeDate(Date)==USLaborDay(2014),1,
                                                                                                                                                              ifelse(timeDate(Date)==USLaborDay(2015),1,
                                                                                                                                                                     ifelse(timeDate(Date)==USLaborDay(2016),1,             
                                                                                                                                                                            ifelse(timeDate(Date)==USThanksgivingDay(2011),1,
                                                                                                                                                                                   ifelse(timeDate(Date)==USThanksgivingDay(2012),1,
                                                                                                                                                                                          ifelse(timeDate(Date)==USThanksgivingDay(2013),1,           
                                                                                                                                                                                                 ifelse(timeDate(Date)==USThanksgivingDay(2014),1,
                                                                                                                                                                                                        ifelse(timeDate(Date)==USThanksgivingDay(2015),1,
                                                                                                                                                                                                               ifelse(timeDate(Date)==USThanksgivingDay(2016),1,0))))))))))))))))))))))))))))))


# dataframe with variables important for regression

df.regress<-data.frame(Date,Vermont,DryBulb,HDD,CDD,HDD.1,HDD.2,HDD.3,CDD.1,CDD.2,CDD.3,wt,dow,mon,FH,VH,wc,HI)

df.train_regress<-df.regress[1:2190,]
df.valid_regress<-df.regress[2191:2920,]
df.test_regress<-df.regress[2921:3285,]

# read data as time series

read.data.day <- function(years, path="/Users/yuanwong/Downloads/forecasting") {
  
  day <- character(); Vermont <- numeric()
  
  for (year in years) { 
    dem <- read.csv(paste(path, year, "_VT.csv", sep=""),
                    colClasses=c("character",rep("numeric",13)),
                    skip=1)
    names(dem)<-c("Date","Hour","DA_VERMONT","Vermont","DA_LMP","DA_EC","DA_CC","DA_MLC","LMP","EC","CC","MLC","DryBulb","DewPt")
    
    if(year<2016) {
      dem$Date <- format(as.Date(dem$Date,"%m/%d/%Y"), "%Y-%m-%d")
    } else {
      dem$Date <- format(as.Date(dem$Date,"%d-%b-%y"), "%Y-%m-%d")
    }
    dem <-dem[!format(as.Date(dem$Date),"%m-%d") == "02-29",]
    dem[dem$Vermont==0,3:14]<-dem[(which(dem$Vermont==0))-168,3:14]
    dem[dem$Date=="2011-11-06" & dem$Hour==2,3:14]<-dem[(which(dem$Date=="2011-11-06" & dem$Hour==2))-168,3:14]
    dem[dem$Date=="2012-11-04" & dem$Hour==2,3:14]<-dem[(which(dem$Date=="2012-11-04" & dem$Hour==2))-168,3:14]
    dem[dem$Date=="2013-11-03" & dem$Hour==2,3:14]<-dem[(which(dem$Date=="2013-11-03" & dem$Hour==2))-168,3:14]
    dem[dem$Date=="2014-11-02" & dem$Hour==2,3:14]<-dem[(which(dem$Date=="2014-11-02" & dem$Hour==2))-168,3:14]
    dem[dem$Date=="2015-11-01" & dem$Hour==2,3:14]<-dem[(which(dem$Date=="2015-11-01" & dem$Hour==2))-168,3:14]
    dem$Date <- as.character(dem$Date) 
    
    day  <- c(day,dem$Date)
    Vermont <- c(Vermont,dem$Vermont)
    
    VT <- timeSeries(Vermont, day, format="%Y-%m-%d")
  }
  by <- timeSequence(from = start(VT), to = end(VT), by = "day")
  VT <- aggregate(VT,by,max)
  return(VT)
}

VT.day <-read.data.day(2011:2019)

VT.train <- read.data.day(2011:2016)
VT.valid <- read.data.day(2017:2018)
VT.test <- read.data.day(2019)

##redo naive method

VT.day.msts <- msts(VT.day$TS.1,seasonal.periods=c(7,365),start=c(2011,1),end=c(2020,0))
VT.train.msts <- window(VT.day.msts,end=c(2017,0))
VT.valid.msts <- window(VT.day.msts,start=c(2017,1),end=c(2019,0))
VT.test.msts <- window(VT.day.msts,start=c(2019,1))

# naive/no-change/random walk/persistence

naive1 <- naive(VT.train.msts, h=length(VT.valid.msts))
accuracy(naive1,VT.valid.msts)

bias1  <- mean(naive1$mean - VT.valid.msts)
pbias1 <- mean((naive1$mean - VT.valid.msts)/
                 VT.valid.msts)*100
mape1 <- mean(abs((naive1$mean - VT.valid.msts)/
                    VT.valid.msts))*100

# seasonal naive / seasonal no-change

naiveS <- snaive(VT.train.msts, h=1)
accuracy(naiveS,VT.valid.msts)

bias2  <- mean(naiveS$mean - VT.valid.msts)
pbias2 <- mean((naiveS$mean - VT.valid.msts)/
                 VT.valid.msts)*100
mape2 <- mean(abs((naiveS$mean - VT.valid.msts)/
                    VT.valid.msts))*100

# three-day moving average (rolling three-day mean)

naive3 <- rollmean(VT.day.msts, 3, align="right")
naive3.train <- window(naive3,end=c(2017,0))
naive3.valid <- window(naive3,start=c(2017,1),end=c(2019,0))
accuracy(naive3.valid,VT.valid.msts)

bias3  <- mean(naive3.valid - VT.valid.msts)
pbias3 <- mean((naive3.valid - VT.valid.msts)/
                 VT.valid.msts)*100
mape3  <- mean(abs((naive3.valid - VT.valid.msts)/
                     VT.valid.msts))*100

# seasonalities and trend of the data

plot(stl(VT.train.msts,"periodic"))

######################################################################################################
######################################################################################################


##  Smoothing methods and models                             

#ses

ses <- ses(VT.train.msts,h=1)
print(ses$model)

ses.valid <- ses(VT.train.msts,h=length(VT.valid.msts))
print(t(accuracy(ses.valid,VT.valid.msts)[,1:5]))

plot(VT.day.msts,type="l",main="Simple Exponential Smoothing")
lines(ses$fitted,col="steelblue",lwd=1.5)
abline(v=2017,lty=3)
abline(v=2019,lty=3)
lines(ses.valid$mean,col="steelblue",lwd=1.5)
legend("bottomleft",legend=c("observed",paste("alpha=",round(ses$model$par[1],2),sep="")),
       col=c("black","steelblue"),
       lwd=c(1.5,1.5))

# holt

holt.simple<- holt(VT.train.msts,h=length(VT.valid.msts),initial="simple")
holt.opt<- holt(VT.train.msts,h=length(VT.valid.msts),damped=F)
holt.damped<- holt(VT.train.msts,h=length(VT.valid.msts),damped=T)
print(holt.simple$model);
print(holt.opt$model);
print(holt.damped$model);
plot(holt.simple,ylim=c(550,950)); lines(VT.valid.msts,col="red")
legend("bottomleft",legend=c("fore","obs"),lty=c(1,1),
       col=c("blue","red"))
plot(holt.opt,ylim=c(550,950)); lines(VT.valid.msts,col="red")
legend("bottomleft",legend=c("fore","obs"),lty=c(1,1),
       col=c("blue","red"))
plot(holt.damped,ylim=c(550,950)); lines(VT.valid.msts,col="red")
legend("bottomleft",legend=c("fore","obs"),lty=c(1,1),
       col=c("blue","red"))
print(accuracy(holt.simple,VT.valid.msts)[,1:5])
print(accuracy(holt.opt,VT.valid.msts)[,1:5])
print(accuracy(holt.damped,VT.valid.msts)[,1:5])            # same as ses!

# Holt Winters

VT.day.ts<-ts(VT.day,start = c(2011,1),frequency = 365)
training <- window(VT.day.ts,end=c(2016,365))
validation <- window(VT.day.ts,start=c(2017,1),end=c(2018,365))

VT.day.ts1 <- ts(VT.day, start=c(1,1), frequency=7)
training1 <- window(VT.day.ts1,end=c(313,6))
validation1 <- window(VT.day.ts1,start=c(313,7),end=c(418,1))

hw.add <- hw(training1,seasonal="additive")
hw.mult <- hw(training1,seasonal="multiplicative")
hw.add.damped <- hw(training1,seasonal="additive",damped=T)
hw.mult.damped <- hw(training1,seasonal="multiplicative",damped=T)
cat("Values of optimally chosen Holt-Winters (add) parameters are\n")
print(hw.add$model)
cat("Values of optimally chosen Holt-Winters (add) parameters damped are\n")
print(hw.add.damped$model)
cat("\n Values of optimally chosen Holt-Winters (mult) parameters are\n")
print(hw.mult$model)
cat("\n Values of optimally chosen Holt-Winters (mult) parameters damped are\n")
print(hw.mult.damped$model)

fore1 <- hw(training1,seasonal="additive",h=length(validation1))
fore1.2 <- hw(training1,seasonal="additive",h=length(validation1),damped=T)
fore2 <- hw(training1,seasonal="multiplicative",h=length(validation1))
fore2.2 <- hw(training1,seasonal="multiplicative",h=length(validation1),damped=T)

plot(fore1); lines(validation1,col="red")
legend("bottomleft",legend=c("fore","obs"),lty=c(1,1),
       col=c("blue","red"),cex=0.7)
plot(fore1.2); lines(validation1,col="red")
legend("bottomleft",legend=c("fore","obs"),lty=c(1,1),
       col=c("blue","red"),cex=0.7)

plot(fore2); lines(validation1,col="red")
legend("bottomleft",legend=c("fore","obs"),lty=c(1,1),
       col=c("blue","red"),cex=0.7)
plot(fore2.2); lines(validation1,col="red")
legend("bottomleft",legend=c("fore","obs"),lty=c(1,1),
       col=c("blue","red"),cex=0.7)

options(digits=2)
cat("\n                        Additive                               Multiplicative\n")
print(t(rbind(accuracy(fore1,validation1),accuracy(fore1.2,validation1),accuracy(fore2,validation1),accuracy(fore2.2,validation1))))

par(mfrow=c(2,1),cex=0.8)
barplot(c(accuracy(fore1,validation1)[2,1:5],
          accuracy(fore1.2,validation1)[2,1:5]),ylim=c(-400,400))
title(sub="       Additive                        Additive with dampening",
      cex.sub=1, 
      main="Out-of-sample error",cex.main=1.2)
barplot(c(accuracy(fore2,validation1)[2,1:5],
          accuracy(fore2.2,validation1)[2,1:5]))
title(sub="       Multiplicative            Multiplicative with dampening",
      cex.sub=1)

par(mfrow=c(1,1))

# Fixed Parameters for ets

mnm<-ets(training1,allow.multiplicative.trend=T)
print(mnm)

trainingets <- training1
for (i in 1:730) {
  fore <- forecast(head(VT.day.ts1,n=2190+i),model=mnm,h=1,use.initial.values=T)
  dm<-ts(fore$mean)
  trainingets <- ts(c(trainingets,dm),start = start(trainingets),frequency = frequency(trainingets))
  }
fore <- forecast(trainingets,model=mnm,h=1)

predets <- window(trainingets,start=c(313,7),end=c(418,1))
plot(predets,ylim=c(550,950), main="Forecast of ETS",col="blue")
lines(validation1,col="red")
legend("bottomleft",legend=c("obs","pred"),lty=c(1,1),
       col=c("red","blue"),cex=0.7)
print(accuracy(predets,validation1))

#Fixed parameters for tbats

training0 <- VT.train.msts
tfit <- tbats(training0, use.box.cox = TRUE )
print(tfit)
for (i in 1:730) {
  fcasts.t <- forecast(head(VT.day.msts,n=2190+i),model=tfit,h=1,use.initial.values=T)
  dm<-ts(fcasts.t$mean)
  training0 <- ts(c(training0,dm),start = start(training0),frequency = frequency(training0))
}
fcasts.t <- forecast(training0,model=tfit,h=1)

predtbats <- window(training0,start=c(2017,1),end=c(2019,0))
plot(predtbats,ylim=c(550,950),main="Forecast of TBATS",col="blue")
lines(VT.valid.msts,col="red")
legend("bottomleft",legend=c("obs","pred"),lty=c(1,1),
       col=c("red","blue"),cex=0.7)
print(accuracy(predtbats,VT.valid.msts))



##  Regression          

VT.train <- read.data.day(2011:2016)
VT.valid <- read.data.day(2017:2018)
VT.test <- read.data.day(2019)

HDDt <- timeSeries(df.train_regress$HDD,date(df.train_regress$Date),format="%Y-%m-%d")
CDDt <- timeSeries(df.train_regress$CDD,date(df.train_regress$Date),format="%Y-%m-%d")
DryBulbt <- timeSeries(df.train_regress$DryBulb,date(df.train_regress$Date),format="%Y-%m-%d")

# model with time only (R2=0.0322)

lm0<-lm(Vermont~as.numeric(Date),data=df.train_regress)
plot(df.train_regress[[2]],ylab="daily demand in Vermont (MW/h)",
     xlab="days after 1 Jan 2011 until 31 Dec 2016")
abline(a=lm0$coef[1],b=lm0$coef[2],col="red")
summary(lm0)

# Temperature based variables

plot(series(DryBulbt),series(VT.train),col=substr(df.train_regress[[1]],1,4),
     ylab="daily demand in Vermont (MW/h)",
     xlab="Dry Bulb Temperature (deg Fahrenheit)",pch=21)
legend("bottomleft",col=3:8,legend=2011:2016,pch=21)

plot(series(CDDt),series(VT.train),col=substr(df.train_regress[[1]],1,4),
     ylab="daily demand in Vermont (MW/h)",xlab="CDD",pch=23)
plot(series(HDDt),series(VT.train),col=substr(df.train_regress[[1]],1,4),
     ylab="daily demand in Vermont (MW/h)",xlab="HDD",pch=23)

plot(df.train_regress$HDD.1,df.train_regress$Vermont,xlab="lag-1 HDD",ylab="daily demand in Vermont (MW/h)")
plot(df.train_regress$HDD.2,df.train_regress$Vermont,xlab="lag-2 HDD",ylab="daily demand in Vermont (MW/h)")
plot(df.train_regress$HDD.3,df.train_regress$Vermont,xlab="lag-3 HDD",ylab="daily demand in Vermont (MW/h)")
plot(df.train_regress$CDD.1,df.train_regress$Vermont,xlab="lag-1 CDD",ylab="daily demand in Vermont (MW/h)")
plot(df.train_regress$CDD.2,df.train_regress$Vermont,xlab="lag-2 CDD",ylab="daily demand in Vermont (MW/h)")
plot(df.train_regress$CDD.3,df.train_regress$Vermont,xlab="lag-3 CDD",ylab="daily demand in Vermont (MW/h)")

boxplot(Vermont~ dow,data=df.train_regress,
        names=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"),
        ylab="daily demand in Vermont (MW/h)")

# model with dry bulb temperature (R2=0.2)

mreg0 <- lm(Vermont ~ DryBulb,data=df.train_regress, x=T, y=T)
plot(mreg0$x[,2],mreg0$y,
     ylab="daily demand in Vermont (MW/h)",xlab="Dry Bulb Temperature (deg Fahrenheit)",
     main="2011-2016: training")
lines(mreg0$x[,2],mreg0$fitted.values,col="red")
legend("bottomleft",legend=c("observed","fitted"),col=c("black","red"),
       pch=1)
print(summary(mreg0))

# model with dry bulb temperature<52.5 (R2=0.622)

mreg1 <- lm(Vermont ~ DryBulb, data=df.train_regress,x=T, y=T, subset=(DryBulb<52.5))
plot(mreg1$x[,2],mreg1$y,
     ylab="daily demand in Vermont (MW/h)",
     xlab="Dry Bulb Temperature (deg Fahrenheit)",
     main="2011-2016: when DryBulb Temp<52.5F only")
points(mreg1$x[,2],mreg1$fitted.values,col="red")
legend("bottomleft",legend=c("observed","fitted"),col=c("black","red"),
       pch=1)
print(summary(mreg1))

# model with dry bulb temperature>=57.5 (R2=0.504)

mreg2 <- lm(Vermont ~ DryBulb, data=df.train_regress, x=T, y=T, subset=(DryBulb>=57.5))
plot(mreg2$x[,2],mreg2$y,
     ylab="daily demand in Vermont (MW/h)",
     xlab="Dry Bulb Temperature (deg Fahrenheit)",
     main="2011-2016: when Dry Bulb Temp>=57.5F only")
points(mreg2$x[,2],mreg2$fitted.values,col="red")
legend("bottomright",legend=c("observed","fitted"),col=c("black","red"),
       pch=1)
print(summary(mreg2))

# model with dry bulb temperature>=57.5 and days of week (R2=0.679)

mreg2a <- lm(Vermont ~ DryBulb + factor(dow),
             data=df.train_regress, x=T, y=T, subset=(DryBulb>=57.5))
plot(mreg2a$x[,2],mreg2a$y,
     ylab="daily demand in Vermont (MW/h)",xlab="Dry Bulb Temperature (deg Fahrenheit)",
     main="2011-2016: when Temp>=57.5F only, with days of week")
points(mreg2a$x[,2],mreg2a$fitted.values,
       col=as.numeric(factor(df.train_regress$dow)[df.train_regress$DryBulb>=57.5]),pch=15)
legend("bottomright",legend=(levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
       col=as.numeric(factor(levels(factor(df.train_regress$dow)))),pch=rep(15,6),
       title="Fitted")
print(summary(mreg2a))

# model with dry bulb temperature>=57.5 and days of week and holidays (R2=0.689)

mreg2b <- lm(Vermont ~ DryBulb + factor(dow) +
               factor(FH) + factor(VH), 
             data=df.train_regress, x=T, y=T, subset=(DryBulb>=57.5))
plot(mreg2b$x[,2],mreg2b$y,
     ylab="daily demand in Vermont (MW/h)",xlab="Dry Bulb Temperature (deg Fahrenheit)",
     main="2011-2016: when Temp>=57.5F only, with days of week and holidays")
points(mreg2b$x[,2],mreg2b$fitted.values,
       col=as.numeric(factor(df.train_regress$dow)[df.train_regress$DryBulb>=57.5]),pch=15)
legend("bottomright",legend=(levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
       col=as.numeric(factor(levels(factor(df.train_regress$dow)))),pch=rep(15,6),
       title="Fitted")
print(summary(mreg2b))

# model with dry bulb temperature>=57.5, days of week, holidays and CDD with lags (R2=0.699)

mreg2c <- lm(Vermont ~ DryBulb + factor(dow) +
               factor(FH) + factor(VH) +
               CDD.1 + CDD.2 + CDD.3 ,
             data=df.train_regress, x=T, y=T, subset=(DryBulb>=57.5))
plot(mreg2c$x[,2],mreg2c$y,
     ylab="daily demand in Vermont (MW/h)",
     xlab="Dry Bulb Temperature (deg Celsius)",
     main="2011-2016: when Temp>=57.5F only, 
     adding 3 lags CDD to days of week and holidays)")
points(mreg2c$x[,2],mreg2c$fitted.values, 
       col=as.numeric(factor(df.train_regress$dow)[df.train_regress$DryBulb>=57.5]),pch=15)
legend("bottomright",legend=(levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
       col=as.numeric(factor(levels(factor(df.train_regress$dow)))),pch=rep(15,6),
       title="Fitted")
print(summary(mreg2c))

# model with dry bulb temperature<52.5, days of week, holidays and HDD with lags (R2=0.698)

mreg2cc <- lm(Vermont ~ DryBulb + factor(dow) +
                factor(FH) + factor(VH) +
               HDD.1 + HDD.2 + HDD.3 ,
              data=df.train_regress, x=T, y=T, subset=(DryBulb<52.5))
summary(mreg2cc)

# model with time, days of week, holidays, CDD with lags and HDD with lags (R2=0.739)

mreg3 <- lm(Vermont ~ as.numeric(Date) + factor(dow) +
              factor(FH) + factor(VH) +
              CDD + CDD.1 + CDD.2 + CDD.3 +
              HDD + HDD.1 + HDD.2 + HDD.3 ,
            data=df.train_regress, x=T, y=T)
plot(mreg3$x[,14],mreg3$y,
     ylab="daily demand in Vermont (MW/h)",xlab="HDD",
     main=paste("2011-2016: training
                ",
                as.character(mreg3$call)[2]),cex.main=0.75)
points(mreg3$x[,14],mreg3$fitted.values,col="red")
legend("bottomright",legend=c("observed","fitted"),col=c("black","red"),
       pch=1)
plot(mreg3$x[,11],mreg3$y,
     ylab="daily demand in Vermont (MW/h)",xlab="CDD",
     main=paste("2011-2016: training
                ",
                as.character(mreg3$call)[2]),cex.main=0.75)
points(mreg3$x[,11],mreg3$fitted.values,col="red")
legend("bottomright",legend=c("observed","fitted"),col=c("black","red"),
       pch=1)
print(summary(mreg3))

#plot(mreg3$y,type="l")
#points(mreg3$fitted.values,type="l",col="blue")

#df.valid_regress<-df.valid_regress[,c(1,16,17,18)]
#predict.lm(mreg3,df.)

# model with time, days of week, holidays, CDD with lags, HDD with lags and windchill (R2=0.903)
# HDD lag 3 is not significant

boxplot(Vermont~mon,df.train_regress,names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
        ylab="daily peak demand in Vermont",xlab="Months (mon)")

mreg4 <- lm(Vermont ~ as.numeric(Date) + factor(dow) + factor(mon) +
              factor(FH) + factor(VH) +
              CDD + CDD.1 + CDD.2+
              HDD + HDD.2 + HDD.3 +
              wc+HI,
            data=df.train_regress, x=T, y=T)
plot(mreg4$x[,2],mreg3$y,
     ylab="daily demand in Vermont (MW/h)",xlab="Time",
     main="2011-2016: Regression model on training set",
                cex.main=0.75)
points(mreg3$fitted.values,col="red")
legend("topright",legend=c("observed","fitted"),col=c("black","red"),
       pch=1)
print(summary(mreg4))

# residual diagnostic of last regression model
acf(residuals(mreg4))
plot(mreg4, which=1, col=c("black"))
plot(mreg4, which=3, col=c("black"))
qqnorm(residuals(mreg4))
qqline(residuals(mreg4))

# noise is autocorrelated according to Durbin Watson test

print(dwtest(mreg4))

# Since we are using the lag 3 of HDD, we are bound to have missing values
# the traning set will therefore not consider the first 3 observations

# weekdays

DMon <- ifelse(dow==2,1,0)
DMon.train <- DMon[4:2190]
DMon.valid <- DMon[2191:2920]
DTue <- ifelse(dow==3,1,0)
DTue.train <- DTue[4:2190]
DTue.valid <- DTue[2191:2920]
DWed <- ifelse(dow==4,1,0)
DWed.train <- DWed[4:2190]
DWed.valid <- DWed[2191:2920]
DThu <- ifelse(dow==5,1,0)
DThu.train <- DThu[4:2190]
DThu.valid <- DThu[2191:2920]
DFri <- ifelse(dow==6,1,0)
DFri.train <- DFri[4:2190]
DFri.valid <- DFri[2191:2920]
DSat <- ifelse(dow==7,1,0)
DSat.train <- DSat[4:2190]
DSat.valid <- DSat[2191:2920]

# Month

DFeb <- ifelse(mon==2,1,0)
DFeb.train <- DFeb[4:2190]
DFeb.valid <- DFeb[2191:2920]
DMar <- ifelse(mon==3,1,0)
DMar.train <- DMar[4:2190]
DMar.valid <- DMar[2191:2920]
DApr <- ifelse(mon==4,1,0)
DApr.train <- DApr[4:2190]
DApr.valid <- DApr[2191:2920]
DMay <- ifelse(mon==5,1,0)
DMay.train <- DMay[4:2190]
DMay.valid <- DMay[2191:2920]
DJun <- ifelse(mon==6,1,0)
DJun.train <- DJun[4:2190]
DJun.valid <- DJun[2191:2920]
DJul <- ifelse(mon==7,1,0)
DJul.train <- DJul[4:2190]
DJul.valid <- DJul[2191:2920]
DAug <- ifelse(mon==8,1,0)
DAug.train <- DAug[4:2190]
DAug.valid <- DAug[2191:2920]
DSep <- ifelse(mon==9,1,0)
DSep.train <- DSep[4:2190]
DSep.valid <- DSep[2191:2920]
DOct <- ifelse(mon==10,1,0)
DOct.train <- DOct[4:2190]
DOct.valid <- DOct[2191:2920]
DNov <- ifelse(mon==11,1,0)
DNov.train <- DNov[4:2190]
DNov.valid <- DNov[2191:2920]
DDec <- ifelse(mon==12,1,0)
DDec.train <- DDec[4:2190]
DDec.valid <- DDec[2191:2920]

Date.train <- Date[4:2190]
Date.valid <- Date[2191:2920]

FH.train <- FH[4:2190]
FH.valid <- FH[2191:2920]
VH.train <- VH[4:2190]
VH.valid <- VH[2191:2920]

wc.train <- wc[4:2190]
wc.valid <- wc[2191:2920]

CDD.train <- CDD[4:2190]
CDD.valid<- CDD[2191:2920]
CDD.1.train <- CDD.1[4:2190]
CDD.1.valid <- CDD.1[2191:2920]
CDD.2.train <- CDD.2[4:2190]
CDD.2.valid <- CDD.2[2191:2920]
CDD.3.train <- CDD.3[4:2190]
CDD.3.valid <- CDD.3[2191:2920]
HDD.train <- HDD[4:2190]
HDD.valid <- HDD[2191:2920]
HDD.2.train <- HDD.2[4:2190]
HDD.2.valid <- HDD.2[2191:2920]
HDD.3.train <- HDD.3[4:2190]
HDD.3.valid <- HDD.3[2191:2920]
HI.train <- HI[4:2190]
HI.valid<- HI[2191:2920]

plot(df.train_mean$DryBulb,df.train_mean$Vermont)
plot(df.valid_regress$DryBulb,df.valid_regress$Vermont)

#Autoarima first time prediction
fit <- auto.arima(VT.train[4:2190], max.D = 8, xreg=cbind(
  as.numeric(as.Date(Date.train)),
  DMon.train,DTue.train,DWed.train,DThu.train,DFri.train,DSat.train,
  DFeb.train,DMar.train,DApr.train,DMay.train,DJun.train,DJul.train,DSep.train,DOct.train,DNov.train,DDec.train,
  FH.train, VH.train, 
  CDD.train, CDD.1.train,CDD.2.train, HDD.train, HDD.2.train, HDD.3.train, 
  wc.train, HI.train)) 
print(fit)
acf(residuals(fit),
    main="With proper error structure (using auto.arima)")
checkresiduals(fit)

adjreg <- sarima(VT.train[4:2190], 1,0,0, xreg=cbind(as.numeric(as.Date(Date.train)),
                                                     DMon.train,DTue.train,DWed.train,DThu.train,DFri.train,DSat.train,
                                                     DFeb.train,DMar.train,DApr.train,DMay.train,DJun.train,DJul.train,DSep.train,DOct.train,DNov.train,DDec.train,
                                                     FH.train, VH.train, 
                                                     CDD.train, CDD.1.train,CDD.2.train, HDD.train, HDD.2.train, HDD.3.train, 
                                                     wc.train, HI.train))
#prediction with auto.arima
newxreg1=cbind(
  as.numeric(as.Date(Date.valid)), 
  DMon.valid,DTue.valid,DWed.valid,DThu.valid,DFri.valid,DSat.valid,
  DFeb.valid,DMar.valid,DApr.valid,DMay.valid,DJun.valid,DJul.valid,DSep.valid,DOct.valid,DNov.valid,DDec.valid,
  FH.valid, VH.valid, 
  CDD.valid, CDD.1.valid,CDD.2.valid, HDD.valid, HDD.2.valid, HDD.3.valid,  
  wc.valid,HI.valid)

pred.AR <- predict(fit,n.ahead=1,newxreg=newxreg1)
pred.Ar <- timeSeries(pred.AR$pred,date(df.valid_regress$Date),format="%Y-%m-%d")
valid.pred <- timeSeries(df.valid_regress$Vermont,date(df.valid_regress$Date),format="%Y-%m-%d")
#print(accuracy(pred.Ar,validation)[,1:5])
biasAR  <- mean(pred.Ar - VT.valid.msts)
pbiasAR <- mean((pred.Ar - VT.valid.msts)/
                  VT.valid.msts)*100
mapeAR <- mean(abs((pred.Ar - VT.valid.msts)/
                     VT.valid.msts))*100

HDDv<- timeSeries(HDD.valid,date(df.valid_regress$Date),format="%Y-%m-%d")
CDDv<- timeSeries(CDD.valid,date(df.valid_regress$Date),format="%Y-%m-%d")
HIv <- timeSeries(HI.valid,date(df.valid_regress$Date),format="%Y-%m-%d")
WCv <- timeSeries(wc.valid,date(df.valid_regress$Date),format="%Y-%m-%d")
difference<- pred.Ar-valid.pred
plot(difference,plot.type="s",at="chic")
lines(HDDv,col="red")
lines(CDDv,col="blue")
lines(HIv,col="orange")
lines(WCv,col="green")

plot(pred.Ar,ylim=c(550,950),plot.type="s",at="chic")
lines(valid.pred,col="red")
legend("bottomleft",legend=c("obs","pred"),lty=c(1,1),
       col=c("red","black"),cex=0.7)
