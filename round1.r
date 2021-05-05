#
# Program: Vermont.R
#
# Purpose: Computing some naive forecasts of the electricity demand of Vermont
#          from a dataset covering the years 2011 to 2019
#
# Description: The inital dataset presents the demand of Vermont as an hourly 
#              data. The hourly demand will be aggregated to obtain a daily peak 
#              demand. An exploratory data analysis will be executed and a first
#              forecasting benchmark will be determined using 3 naive methods.
#              
#
# Written by: Tianze Jiang
#             Rahul Tarkeshwar Pathak
#             Sumin Tan
#             Yuan Wang
#             on January 2020
# 
# ------------------------------------------------------



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

# read hourly data as a dataframe with 02-29 removed

read.df_i <- function(years, path="/Users/yuanwong/Downloads/forecasting") {
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
  }
  # removing leap days 
  df_comb <-df_comb[!format(as.Date(df_comb$Date),"%m-%d") == "02-29",]
  return(df_comb)
}

df_i <- read.df_i(2011:2019)

# read hourly data as a timeseries with 02-29 removed

read.data.hour_i <- function(years, path="C:/Users/sumin/Desktop/") {
  
  day <- character(); hour <- Vermont <- numeric()
  
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
    dem$Date <- as.character(dem$Date)    
    
    day  <- c(day,dem$Date)
    hour <- c(hour,dem$Hour)
    Vermont <- c(Vermont,dem$Vermont)
    
    myday  <- paste(day, hour, sep="-")        
    VT <- timeSeries(Vermont, myday, format="%Y-%m-%d-%H")
  }
  return(VT)
}

VT.hour_i <- read.data.hour_i(2011:2019)

# Visualize the hourly data of 2011-2019 through a plot 

plot(VT.hour_i, ylab="Vermont hourly demand (in MW)", at="pretty")

# Data exploration

summary(df_i)

# second sunday of march has a missing value due to daylight saving from 2011 to 2015 

df_i[df_i$Vermont==0,]

# compare the missing value to the week before and the week after for the imputation

week1 <- series(window(VT.hour_i,
                       start=timeDate("2011-03-13-01", format="%Y-%m-%d-%H"),
                       end=timeDate("2011-03-19-24", format="%Y-%m-%d-%H")))
week2 <- series(window(VT.hour_i,
                       start=timeDate("2011-03-06-01", format="%Y-%m-%d-%H"),
                       end=timeDate("2011-03-12-24", format="%Y-%m-%d-%H")))
week3 <- series(window(VT.hour_i,
                       start=timeDate("2011-03-20-01", format="%Y-%m-%d-%H"),
                       end=timeDate("2011-03-26-24", format="%Y-%m-%d-%H")))
plot(week1, axes=F,
     lty=1, type="l", ylim=c(min(week1,week2,week3),max(week1,week2,week3)),
     ylab="Vermont hourly demand (in MW)", xlab="")
lines(week2, lty=3, lwd=1.8)
lines(week3,lty=2, lwd=1.8)
axis(1, at=seq(1+12, 24*7+12, by=24),
     labels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
legend("bottomright", legend=c("Mar 13-19, 2011","Mar 06-12 2011", "Mar 20-16 2011"), 
       lty=c(1,3,2))

# Second daylight saving on the first sunday of November from 2011 to 2015 where 2 hours are combined

df_i[df_i$Date=="2011-11-06" & df_i$Hour==2,]
df_i[df_i$Date=="2012-11-04" & df_i$Hour==2,]
df_i[df_i$Date=="2013-11-03" & df_i$Hour==2,]
df_i[df_i$Date=="2014-11-02" & df_i$Hour==2,]
df_i[df_i$Date=="2015-11-01" & df_i$Hour==2,]

week4 <- series(window(VT.hour_i,
                       start=timeDate("2011-10-31-01", format="%Y-%m-%d-%H"),
                       end=timeDate("2011-11-05-24", format="%Y-%m-%d-%H")))
week5 <- series(window(VT.hour_i,
                       start=timeDate("2011-11-06-01", format="%Y-%m-%d-%H"),
                       end=timeDate("2011-11-12-24", format="%Y-%m-%d-%H")))
week6 <- series(window(VT.hour_i,
                       start=timeDate("2011-11-13-01", format="%Y-%m-%d-%H"),
                       end=timeDate("2011-11-19-24", format="%Y-%m-%d-%H")))
plot(week5, axes=F,
     lty=1, type="l", ylim=c(min(week4,week5,week6),max(week4,week5,week6)),
     ylab="Vermont hourly demand (in MW)", xlab="")
lines(week4, lty=3, lwd=1.8)
lines(week6,lty=2, lwd=1.8)
axis(1, at=seq(1+12, 24*7+12, by=24),
     labels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
legend("topright", legend=c("Nov 06-12, 2011","Oct 31-Nov 05 2011", "Nov 13-19 2011"), 
       lty=c(1,3,2))

# Read data after imputation for daylight saving

read.df <- function(years, path="C:/Users/sumin/Desktop/") {
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

df.train <- read.df(2011:2017)
df.valid <- read.df(2018)
df.test <- read.df(2019)

read.data.hour <- function(years, path="/Users/yuanwong/Downloads/forecasting") {
  
  day <- character(); hour <- Vermont <- numeric()
  
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
    hour <- c(hour,dem$Hour)
    Vermont <- c(Vermont,dem$Vermont)
    
    myday  <- paste(day, hour, sep="-")        
    VT <- timeSeries(Vermont, myday, format="%Y-%m-%d-%H")
  }
  return(VT)
}

VT.hour <- read.data.hour(2011:2019)

# Visualize time serie after imputation for daylight saving

plot(VT.hour, ylab="Vermont hourly demand (in MW)", at="pretty")

# Transform dataset into daily peak demand data 
## maximal value for each column

df_max <- aggregate(df[,3:14],by=list(Date=df$Date),FUN=max)

## values of each columns for the maximal value of the demand ## would recommend using this for exp var

df_max.dem <-do.call(rbind,lapply(split(df,df$Date),function(chunk) chunk[which.max(chunk$Vermont),]))
rownames(df_max.dem)<-c()

df_max.dem.train <- do.call(rbind,lapply(split(df.train,df.train$Date),function(chunk) chunk[which.max(chunk$Vermont),]))
rownames(df_max.dem.train)<-c()

## mean of each column with the maximal value of the demand

df_mean <- merge(merge(aggregate(DA_VERMONT~Date,data=df,FUN=mean),
                  aggregate(Vermont~Date,data=df,FUN=max),by="Date"),
                  aggregate(df[,5:14],by=list(Date=df$Date),FUN=mean),by="Date")

# Read imputed data with daily peak demand 

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

VT.train <- read.data.day(2011:2017)
VT.valid <- read.data.day(2018)
VT.test <- read.data.day(2019)

# visualize the data of 2011-2019 through a plot 

plot(VT.day, ylab="Vermont daily peak demand (in MW)", at="pretty")

plot(VT.train, ylab="Vermont daily peak demand (in MW)", at="pretty")


# Trend and seasonality through acf and pacf
acf2(VT.train,main="Daily peak demand of electricity in Vermont 2011-2017")
lag7.VT.train<-diff(VT.train,7)
acf2(lag7.VT.train)
lag1.lag7.VT.train<-diff(lag7.VT.train,1)
acf2(lag1.lag7.VT.train)
plot(lag1.lag7.VT.train)

# plotting of relationship between variables to see linear relationship

plot(df_max.dem.train$DewPt,df_max.dem.train$DryBulb)
cor(df_max.dem.train$DewPt,df_max.dem.train$DryBulb)

# P.ref will be 75

plot(df_max.dem.train$LMP,df_max.dem.train$Vermont)
plot(df_max.dem.train$DA_LMP,df_max.dem.train$Vermont)
ggplot(df_max.dem.train, aes(x = LMP, y = Vermont)) + geom_point() + scale_x_continuous(name="Real Time LMP", limits=c(0, 120)) +
  scale_y_continuous(name="Vermont daily demand", limits=c(550, 1000))
ggplot(df_max.dem.train, aes(x = DA_LMP, y = Vermont)) + geom_point() + scale_x_continuous(name="Day-Ahead LMP", limits=c(0, 120)) +
  scale_y_continuous(name="Vermont daily demand", limits=c(550, 1000))

cor(df_max.dem.train$LMP,df_max.dem.train$Vermont)
cor(df_max.dem.train$DA_LMP,df_max.dem.train$Vermont)

# Dry bulb T.ref should be 60

plot(df_max.dem.train$DryBulb,df_max.dem.train$Vermont)
ggplot(df_max.dem.train, aes(x = DryBulb, y = Vermont)) + geom_point() + scale_x_continuous(name="Temp Dry Bulb", limits=c(30, 80)) +
  scale_y_continuous(name="Vermont daily demand", limits=c(500, 800))

# Dew point Tref should be 45

plot(df_max.dem.train$DewPt,df_max.dem.train$Vermont)
ggplot(df_max.dem.train, aes(x = DewPt, y = Vermont)) + geom_point() + scale_x_continuous(name="Temp Dew Point", limits=c(20, 60)) +
  scale_y_continuous(name="Vermont daily demand", limits=c(500, 900))

#HDD and CDD for the day of reference t

df_max.dem.train$HDD=60-df_max.dem.train$DryBulb
df_max.dem.train$CDD=df_max.dem.train$DryBulb-60
df_max.dem.train$HDD[df_max.dem.train$HDD<=0]=0
df_max.dem.train$CDD[df_max.dem.train$CDD<=0]=0

#Average wind speed and Precipitation

additional<-read.csv("2029335.csv",header=TRUE,sep=",")
additional=select(additional,-c("STATION","NAME","TAVG"))
additional=additional[!format(as.Date(additional$DATE),"%m-%d") == "02-29",]
additional[is.na(additional),]
additional.train <- additional[1:2555,]
df_max.dem.train=cbind(df_max.dem.train,AWND=additional.train$AWND)
df_max.dem.train=cbind(df_max.dem.train,PRCP=additional.train$PRCP)
cor(df_max.dem.train$AWND,df_max.dem.train$Vermont)
cor(df_max.dem.train$PRCP,df_max.dem.train$Vermont)
plot(df_max.dem.train$AWND,df_max.dem.train$Vermont)
plot(df_max.dem.train$PRCP,df_max.dem.train$Vermont)

# Weather type

weathertype0=select(additional.train,-c(1,2,3,4,5))
summary(weathertype0)
print(factanal(weathertype0,6),cutoff=.3)
tp_1=apply(weathertype0[,c(1,2)],1,max)
tp_2=apply(weathertype0[,c(4,6)],1,max)
tp_3=weathertype0$WT08
tp_4=weathertype0$WT09
tp_5=weathertype0$WT11
tp_6=apply(weathertype0[,c(10,11,13)],1,max)
tp_7=weathertype0$WT15
tp_8=weathertype0$WT17
tp_9=apply(weathertype0[,c(15,18)],1,max)
tp_10=weathertype0$WT21
wt=data.frame(tp_1,tp_2,tp_3,tp_4,tp_5,tp_6,tp_7,tp_8,tp_9,tp_10)

weathertype=cbind(Vermont=df_max.dem.train$Vermont,wt)
boxplot(Vermont~tp_1,
        data=weathertype,
        main="Boxplot for weather types",
        xlab="tp_1",
        ylab="Demand",
        col="orange",
        border="brown"
)


#Dummy variables for Weekdays

week=weekdays(as.Date(df_max.dem.train$Date))
df_max.dem.train=cbind(df_max.dem.train,week)
boxplot(Vermont ~ week,
        data=df_max.dem.train,
        names=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"),
        ylab="daily peak demand in Vermont from 2011 to 2018 (MW/h)")


#Dummy variables for holidays
library(timeDate)
library(chron)

hlist <- c("USChristmasDay","USGoodFriday","USIndependenceDay","USLaborDay",
           "USNewYearsDay","USThanksgivingDay") 


myholidays  <- dates(as.character(holiday(2011:2019,hlist)),format="Y-M-D")

df_max.dem.train$holicol= is.holiday(as.Date(df_max.dem.train$Date),myholidays)
df_max.dem.train$holicol=as.factor(df_max.dem.train$holicol)

boxplot(Vermont ~ holicol,
        data=df_max.dem.train,
        names=c("False","True"),
        ylab="daily peak demand in Vermont from 2011 to 2018 (MW/h)")


acf2(VT.day[-1,], main="vermont daily max demand")


#naive
VT.day.ts <- ts(VT.day$TS.1, start=c(1,1), frequency=7)

#Compute (1) naive/no-change/random walk/persistence forecast
naive1 <- naive(VT.day.ts, h=1)
bias1  <- mean(window(naive1$fitted, start=c(365,1),end=c(417,1)) - 
                 window(naive1$x, start=c(365,1),end=c(417,1)))

pbias1 <- mean((window(naive1$fitted, start=c(365,1),end=c(417,1)) -
                  window(naive1$x, start=c(365,1),end=c(417,1)))/
                 window(naive1$x, start=c(365,1),end=c(417,1)))*100
mape1  <- mean(abs((window(naive1$fitted, start=c(365,1),end=c(417,1)) -
                      window(naive1$x, start=c(365,1),end=c(417,1)))/
                     window(naive1$x, start=c(365,1),end=c(417,1))))*100

#seasonal naive / seasonal no-change
naiveS <- snaive(VT.day.ts, h=1)
bias2  <- mean(window(naiveS$fitted, start=c(365,1),end=c(417,1)) - 
                 window(naiveS$x, start=c(365,1),end=c(417,1)))

pbias2 <- mean((window(naiveS$fitted, start=c(365,1),end=c(417,1)) -
                  window(naiveS$x, start=c(365,1),end=c(417,1)))/
                 window(naiveS$x, start=c(365,1),end=c(417,1)))*100
mape2  <- mean(abs((window(naiveS$fitted, start=c(365,1),end=c(417,1)) -
                      window(naiveS$x, start=c(365,1),end=c(417,1)))/
                     window(naiveS$x, start=c(365,1),end=c(417,1))))*100

# rolling three-day mean
naive3 <- rollmean(VT.day.ts, 3, align="right")
#last   <- length(naive3)
bias3  <- mean(naive3[2556:2920] - window(VT.day.ts, start=c(365,1),end=c(417,1)))
pbias3 <- mean((naive3[2556:2920] - window(VT.day.ts, start=c(365,1),end=c(417,1)))/
                 window(VT.day.ts, start=c(365,1),end=c(417,1)))*100
mape3  <- mean(abs((naive3[2556:2920] -
                      window(VT.day.ts, start=c(365,1),end=c(417,1)))/
                     window(VT.day.ts, start=c(365,1),end=c(417,1))))*100



plot(window(naiveS$x, start=c(365,1), end=c(417,1)),
     ylab="Daily demand (TWh)")
lines(window(naive1$fitted, start=c(365,1), end=c(417,1)), col="blue")
lines(window(naiveS$fitted, start=c(365,1), end=c(417,1)), col="red")
lines(ts(naive3[2556:2920], start=c(365,1), freq=7), col="cyan")
legend("bottomleft", legend=c("obs","naive","snaive","3-day mean"),
       col=c("black","blue","red","cyan"), lty=1)


