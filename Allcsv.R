dfRain <- read.csv("/home/keith/R/x86_64-pc-linux-gnu-library/3.4/GSP/Rain.csv")
#dfRain$date <- as.Date(dfRain$date, format = "%Y-%m-%d")
dfFlow <- read.csv("/home/keith/R/x86_64-pc-linux-gnu-library/3.4/GSP/Flow.csv")
#dfFlow$date <- as.Date(dfFlow$date, format = "%Y-%m-%d")
df <- merge(dfFlow,dfRain, by=c("year","month"),all=TRUE)
df$coredata.monthly.[is.na(df$coredata.monthly.)] <- round(mean(df$coredata.monthly., na.rm = TRUE))
df$coredata.morain.[is.na(df$coredata.morain.)] <- round(mean(df$coredata.morain., na.rm = TRUE))
df$coredata.motemp.[is.na(df$coredata.motemp.)] <- round(mean(df$coredata.motemp., na.rm = TRUE))
names(df)[names(df)=="coredata.monthly."] <- "flow"
names(df)[names(df)=="coredata.morain."] <- "rain"
names(df)[names(df)=="coredata.motemp."] <- "temp"
newdf <- df[,c(1,2,4,6,7)]
write.csv(newdf[-c(1:24),], 
          file="/home/keith/R/x86_64-pc-linux-gnu-library/3.4/GSP/Allpre.csv", na="",
          row.names=FALSE)
