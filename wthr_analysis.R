
##install packages
##these packages are for time series
install.packages("xts")
install.packages("tsbox")
##for future prediction
install.packages("forecast")
##get csv data (the data is taken from NOAA)
wthr_data <- read.csv("C:/R/wthr_data.csv")
View(wthr_data)

#list the available variables 
names(wthr_data)

#range of date
range(wthr_data$DATE)

# get library of packages to use
library(xts)
library(tsbox)

# xts creates a time series object and convert date of file to R date
wthr_history = xts(wthr_data[,c("TMAX","TMIN","PRCP")], order.by=as.Date(wthr_data$DATE))

# to remove duplicate data
hist <- wthr_history[ ! duplicated( index(wthr_history) ),  ]

# to view data set
hist

#adding NA values for missing dates
hist = ts_regular(hist)

#fill data by extending previous data
hist = na.fill(hist, "extend")

#clips off the starting and ending dates so the number of years covered is a multiple of four
hist = window(hist, start=as.Date("2001-01-01"), end=as.Date("2022-04-20"))
#view
hist


#Minimum and maximum daily temperatures in a region,from 2001 - 2022

plot(ts_ts(hist$TMAX), col="darkred", bty="n", las=1, fg=NA, 
     ylim=c(-20, 120), ylab="Temperature (F)")

#used to plot ts objects rather than xts 
lines(ts_ts(hist$TMIN), col="navy")

grid(nx=NA, ny=NULL, lty=1, col="gray")

legend("topright", fill=c("darkred", "navy"), cex=0.7,
       legend=c("TMAX", "TMIN"), bg="white")

#Historical daily precipitation in  from 2001 - 2022
barplot(hist$PRCP, border=NA, col="darkgreen", ylim=c(0, 2),
        space=0, bty="n", las=1, fg=NA, ylab="Daily Rainfall (inches)")

grid(nx=NA, ny=NULL, lty=1)
# summary of data:-basic descriptive statics for the time series field
summary(hist)

# it gives min and max of temp and prcp
hist[hist$TMIN == min(hist$TMIN)]
hist[hist$TMAX == max(hist$TMAX)]
hist[hist$PRCP == max(hist$PRCP)]

#it gives summary statics of month
hist$MONTH = format(index(hist), "%m")

#for max summary data
months = split(as.numeric(hist$TMAX), hist$MONTH)
sapply(months, summary)

#for min summary data
months = split(as.numeric(hist$TMIN), hist$MONTH)
sapply(months, summary)


############
#Time Series Decomposition to see what might happen in future
#Time series decomposition for daily maximum temperatures 
#the trend indicates an increase of around one degree (F) in daily
#maximum temperatures over the period
decomposition = stl(ts_ts(hist$TMAX), s.window=365, t.window=7001)

plot(decomposition)

summary(decomposition$time.series[,"trend"])
#############
#same for precipitation
# we also see an small trend in daily increase in precipitation 

decomposition = stl(ts_ts(hist$PRCP), s.window=365, t.window=7001)

plot(decomposition)

summary(decomposition$time.series[,"trend"])


####################
# we can aggregate data by month or year to make things more clear

##mean plot
monthly.tmax = period.apply(hist$TMAX, INDEX = seq(1, nrow(hist) - 1, by=30.4375), FUN = mean)

plot(ts(monthly.tmax), col="darkred",ylim=c(20, 100),  lwd=3, bty="n", las=1, fg=NA, ylab="TMAX (F)")

##sum plot
monthly.prcp = period.apply(hist$PRCP, INDEX = seq(1, nrow(hist) - 1, by=30.4375), FUN = sum)

plot(ts(monthly.prcp), col="darkgreen", 
     lwd=3, bty="n", las=1, fg=NA, ylab="Monthly Precipitation (inches)")


#############
## area plot 
###Area plots can be useful for showing extremes vs. averages with aggregated data
##Historical average, maximum, and minimum monthly temperatures
tmax.mean = period.apply(hist$TMAX, INDEX = seq(1, nrow(hist) - 1, 30.4375), FUN = mean)
tmax.max = period.apply(hist$TMAX, INDEX = seq(1, nrow(hist) - 1, 30.4375), FUN = max)
tmin.mean = period.apply(hist$TMIN, INDEX = seq(1, nrow(hist) - 1, 30.4375), FUN = mean)
tmin.min = period.apply(hist$TMIN, INDEX = seq(1, nrow(hist) - 1, 30.4375), FUN = min)

tmax.area = c(as.numeric(tmax.max), rev(as.numeric(tmax.mean)))
tavg.area = c(as.numeric(tmax.mean), rev(as.numeric(tmin.mean)))
tmin.area = c(as.numeric(tmin.mean), rev(as.numeric(tmin.min)))

indices = c(index(tmax.mean), rev(index(tmax.mean)))

plot(NA, xlim=range(indices), ylim=c(-20, 100), 
     lwd=3, bty="n", las=1, fg=NA, ylab="Monthly Temperatures (F)")

polygon(indices, tmax.area, border=NA, col="darkred")
polygon(indices, tavg.area, border=NA, col="lightgray")
polygon(indices, tmin.area, border=NA, col="navy")

grid(nx=NA, ny=NULL, lty=1)
#################
####To forecast future temperatures based on historical observations,
##we can use Holt-Winters model that considers past seasonal
##cycles, trends, and random variation. 
library(forecast)

training.data = period.apply(hist$TMAX, seq(1, nrow(hist) - 1, 30.4375), max,frequency=12)

model.tmax = hw(ts(training.data, frequency=12, start=2001), h=60)
###############
##The Holt-Winters model forecast of monthly high temperatures 
plot(model.tmax, lwd=3, bty="n", las=1, fg=NA)

grid(nx=NA, ny=NULL, lty=1)
#####The Holt-Winters model forecast of monthly high temperatures ,they give future reference
model.tmax = hw(ts(training.data,frequency=12,start=2001), h=720)

plot(model.tmax, lwd=3, bty="n", las=1, fg=NA)

grid(nx=NA, ny=NULL, lty=1)

######Heating and Cooling Degree Days
##it tells what are higher heating and cooling days

hist$TAVG = (hist$TMAX + hist$TMIN) / 2

hist$HDD = ifelse(hist$TAVG < 65, 65 - hist$TAVG, 0)

hist$CDD = ifelse(hist$TAVG > 65, hist$TAVG - 65, 0)
########Historical monthly heating and cooling degree days 
monthly.cdd = period.apply(hist$CDD, INDEX = seq(1, nrow(hist) - 1, 30.4375), FUN = sum)

monthly.hdd = period.apply(hist$HDD, INDEX = seq(1, nrow(hist) - 1, 30.4375), FUN = sum)

barplot(merge.xts(monthly.cdd, monthly.hdd), 
        col=c("navy", "darkred"), las=1, fg="white",
        space=0, border=NA)

grid(nx=NA, ny=NULL, lty=1)

legend(x="topright", fill=c("navy", "darkred"), legend=c("CDD", "HDD"), bg="white")
######Time series decomposition for cooling degree days
##shows a slight increasing trend 
decomposition = stl(ts_ts(hist$CDD), s.window=365, t.window=7001)

plot(decomposition)
######Time series decomposition for heating degree days
decomposition = stl(ts_ts(hist$HDD), s.window=365, t.window=7001)

plot(decomposition)

########Estimates from monthly temperature averages of monthly 
##heating and cooling degree days
hist$TAVG = (hist$TMAX + hist$TMIN) / 2

monthly.tavg = period.apply(hist$TAVG, INDEX = seq(1, nrow(hist) - 1, 30.4375), FUN = mean)

estimate.hdd = ifelse(monthly.tavg < 65, 65 - monthly.tavg, 0) * 30

estimate.cdd = ifelse(monthly.tavg > 65, monthly.tavg - 65, 0) * 30

barplot(merge.xts(estimate.cdd, estimate.hdd), 
        col=c("navy", "darkred"), las=1, fg="white",
        space=0, border=NA)

grid(nx=NA, ny=NULL, lty=1)

legend(x="topright", fill=c("navy", "darkred"), legend=c("CDD", "HDD"), bg="white")
#######Days per year of precipitation events exceeding one inch 
hist$FLOOD = ifelse(hist$PRCP > 1, 1, 0)

flooding = period.apply(hist$FLOOD, INDEX = seq(1, nrow(hist) - 1, 365.25), FUN = sum)

plot(ts(flooding), col="darkgreen", 
     lwd=3, bty="n", las=1, fg=NA, 
     ylab="Annual Days with High Precipitation")

grid(nx=NA, ny=NULL, lty=1)

summary(as.numeric(flooding))
#######Time series decomposition for high precipitation event days per year
decomposition = stl(ts_ts(hist$FLOOD), s.window=365, t.window=7001)

plot(decomposition)
 ###saving data
saveRDS(hist, "2000-2022-hist.rds")









