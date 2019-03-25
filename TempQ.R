df <- read.csv("SantaCruzPrecip_Daily.csv")
df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
set.seed(25)
df$Precip[is.na(df$Precip)] <- round(mean(df$Precip, na.rm = TRUE))
df$Tmax[is.na(df$Tmax)] <- round(mean(df$Tmax, na.rm = TRUE))
df$Tmin[is.na(df$Tmin)] <- round(mean(df$Tmin, na.rm = TRUE))
# start on 10/1/88, 1988 was a leap year
rain_ts <- ts(df$Precip, start=c(1988,275), frequency=365.25)
plot.ts(rain_ts)
# start on 10/1/88, 1988 was a leap year
df$Tmean <- (df$Tmax+df$Tmin)/2
df$Tmean[is.na(df$Tmean)] <- round(mean(df$Tmean, na.rm = TRUE))
temp_ts <- ts(df$Tmean, start=c(1988,275), frequency=365.25)
plot.ts(temp_ts)
library(xts)
newrain <- xts(rain_ts, order.by = as.Date(df$Date))
newtemp <- xts(temp_ts, order.by = as.Date(df$Date))
morain <- apply.quarterly(newrain,sum)
motemp <- apply.quarterly(newtemp,mean)
plot(morain)
plot(motemp)
rain.df <- data.frame(date=index(morain), coredata(morain), coredata(motemp))
rain.df$month <- format(as.Date(rain.df$date), "%m")
rain.df$year <- format(as.Date(rain.df$date), "%Y")
rain.df <- rain.df[,c(4,5,2,3)]
names(rain.df)[names(rain.df)=="month"] <- "quarter"
q <- function(x) as.integer(as.numeric(x)/4)+1
rain.df$quarter <- sapply(rain.df$quarter,q)
library(ggplot2)
ggplot(rain.df, aes(rain.df$quarter,rain.df$coredata.morain.), 
       value, group=factor(rain.df$year), 
       color=factor(rain.df$year)) +
#  geom_line() +
  geom_point() +
  labs(x="Quarter", color="Year") +
  theme_classic()
ggplot(rain.df, aes(rain.df$quarter,rain.df$coredata.motemp.), 
       value, group=factor(rain.df$year), 
       color=factor(rain.df$year)) +
#  geom_line() +
  geom_point() +
  labs(x="Quarter", color="Year") +
  theme_classic()
write.csv(rain.df, file="RainQ.csv", na="")
