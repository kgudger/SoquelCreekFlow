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
sadj <- seasadj(fit)  # de-seasonalize
plot(sadj, type="l")  # seasonal adjusted
seasonplot(sadj, 12, col=rainbow(12), year.labels=TRUE, main="Seasonal plot: Stream Flow") # seasonal frequency set as 12 for monthly data.
start(flow_ts)
end(flow_ts)
library(zoo)
library(xts)
newflow <- xts(flow_ts, order.by = df$datetime)
monthly <- apply.monthly(newflow,mean)
plot(monthly)
flow.df <- data.frame(date=index(monthly), coredata(monthly))
flow.df$month <- format(as.Date(flow.df$date), "%m")
flow.df$year <- format(as.Date(flow.df$date), "%Y")
flow.df <- flow.df[,c(3,4,2)]
library(ggplot2)
ggplot(flow.df, aes(flow.df$month,flow.df$coredata.monthly.), 
       value, group=factor(flow.df$year), 
       color=factor(flow.df$year)) +
  geom_line() +
  geom_point() +
  labs(x="Month", color="Year") +
  theme_classic()
write.csv(flow.df, file="Flow.csv", na="")
