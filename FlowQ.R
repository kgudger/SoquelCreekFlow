df <- read.csv("USGSGaugeSoquelCkSoquel.csv")
df$datetime <- as.Date(df$datetime, format = "%m/%d/%Y")
df$Mean.cfs[is.na(df$Mean.cfs)] <- round(mean(df$Mean.cfs, na.rm = TRUE))
set.seed(25)
# start on 10/1/88, 1988 was a leap year
flow_ts <- ts(df$Mean.cfs, start=c(1988,275), frequency=365.25)
flow_ts
class(flow_ts)
head(flow_ts) # as "matrix"
plot.ts(flow_ts)
fit <- stl(flow_ts, s.window="period")
plot(fit)
library(forecast)
library(zoo)
library(xts)
newflow <- xts(flow_ts, order.by = df$datetime)
quarterly <- apply.quarterly(newflow,mean)
plot(quarterly)
flow.df <- data.frame(date=index(quarterly), coredata(quarterly))
flow.df$month <- format(as.Date(flow.df$date), "%m")
flow.df$year <- format(as.Date(flow.df$date), "%Y")
flow.df <- flow.df[,c(3,4,2)]
names(flow.df)[names(flow.df)=="month"] <- "quarter"
q <- function(x) as.integer(as.numeric(x)/4)+1
flow.df$quarter <- sapply(flow.df$quarter,q)
library(ggplot2)
ggplot(flow.df, aes(flow.df$quarter,flow.df$coredata.quarterly.), 
       value, group=factor(flow.df$year), 
       color=factor(flow.df$year)) +
#  geom_line() +
  geom_point() +
  labs(x="Quarter", color="Year") +
  theme_classic()
write.csv(flow.df, file="FlowQ.csv", na="")
