dfRain <- read.csv("Rain.csv")
dfFlow <- read.csv("Flow.csv")
#dfFlow$date <- as.Date(dfFlow$date, format = "%Y-%m-%d")
df <- merge(dfFlow,dfRain, by=c("year","month"),all=TRUE)
df$coredata.monthly.[is.na(df$coredata.monthly.)] <- round(mean(df$coredata.monthly., na.rm = TRUE))
df$coredata.morain.[is.na(df$coredata.morain.)] <- round(mean(df$coredata.morain., na.rm = TRUE))
df$coredata.motemp.[is.na(df$coredata.motemp.)] <- round(mean(df$coredata.motemp., na.rm = TRUE))
names(df)[names(df)=="coredata.monthly."] <- "flow"
names(df)[names(df)=="coredata.morain."] <- "rain"
names(df)[names(df)=="coredata.motemp."] <- "temp"
df <- subset(df, select = -c(X.x,X.y))
library(ggcorrplot)
t <- subset(df, year >= 1989)
p <- ggplot(t, aes(month, flow))
p + geom_line(aes(colour = factor(year)))
p <- ggplot(t, aes(month, rain))
p + geom_line(aes(colour = factor(year)))
p <- ggplot(t, aes(month, temp))
p + geom_line(aes(colour = factor(year)))
library(xts)
df <- transform(df, Date = as.Date(paste(year, month, 1, sep = "-")))
#df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
newdf <- as.xts(df, order.by=df$Date)
newdf <- merge(newdf, rain1=lag(newdf$rain,1),
                      rain2=lag(newdf$rain,2),
                      rain3=lag(newdf$rain,3),
                      rain4=lag(newdf$rain,4),
                      rain5=lag(newdf$rain,5),
                      rain6=lag(newdf$rain,6),
                      rain7=lag(newdf$rain,7),
                      rain8=lag(newdf$rain,8),
                      rain9=lag(newdf$rain,9),
                      rain10=lag(newdf$rain,10),
                      rain11=lag(newdf$rain,11),
                      rain12=lag(newdf$rain,12),
                      rain13=lag(newdf$rain,13),
                      rain14=lag(newdf$rain,14),
                      rain15=lag(newdf$rain,15),
                      rain16=lag(newdf$rain,16),
                      rain17=lag(newdf$rain,17),
                      rain18=lag(newdf$rain,18),
                      rain19=lag(newdf$rain,19),
                      rain20=lag(newdf$rain,20),
                      rain21=lag(newdf$rain,21),
                      rain22=lag(newdf$rain,22),
                      rain23=lag(newdf$rain,23),
                      rain24=lag(newdf$rain,24),
                      temp1=lag(newdf$temp,1),
                      temp2=lag(newdf$temp,2),
                      temp3=lag(newdf$temp,3),
                      temp4=lag(newdf$temp,4),
                      temp5=lag(newdf$temp,5),
                      temp6=lag(newdf$temp,6),
                      temp7=lag(newdf$temp,7),
                      temp8=lag(newdf$temp,8),
                      temp9=lag(newdf$temp,9),
                      temp10=lag(newdf$temp,10),
                      temp11=lag(newdf$temp,11),
                      temp12=lag(newdf$temp,12))
newdf.d <- data.frame(newdf)
newdf.d$Date <- NULL
write.csv(newdf.d, file="Allsum.csv", na="", row.names=FALSE)
Mcor <- as.data.frame(newdf)
Mcor$Date = NULL
Mcor <- Mcor[-c(1:24),]
# Mcor is still good data to plot
corDat <- Mcor[,c(3:6,30,36)]
corDat <- corDat[order(corDat$temp), , drop = FALSE]
ggplot(corDat, aes(x=temp, y=flow, group = 1)) + 
  ggtitle("Flow vs. Temperature Monthly") +
  geom_line(aes(y=flow)) +
  geom_smooth(method='lm',formula=y~x)
ggplot(corDat, aes(x=rain, y=flow, group = 1)) + 
  ggtitle("Flow vs Rain Monthly") +
  geom_line(aes(y=flow)) +
  geom_smooth(method='lm',formula=y~x)
ggplot(corDat, aes(x=temp.1, y=flow, group = 1)) + 
  ggtitle("Flow vs. Temperature - 1 month") +
  geom_line(aes(y=flow)) +
  geom_smooth(method='lm',formula=y~x)
ggplot(corDat, aes(x=rain.1, y=flow, group = 1)) + 
  ggtitle("Flow vs. Rain -1 Month") +
  geom_line(aes(y=flow)) +
  geom_smooth(method='lm',formula=y~x)
ggplot(corDat, aes(x=temp.7, y=flow, group = 1)) + 
  ggtitle("Flow vs. Temperature -7 months") +
  geom_line(aes(y=flow)) +
  geom_smooth(method='lm',formula=y~x)
Mcor <- sapply( Mcor, as.numeric )
Mcor <- round(cor(Mcor),2)
Mcor[lower.tri(Mcor, diag = TRUE)] <- NA          # lower tri and diag set to NA
sSet <- as.data.frame(Mcor['flow',])
colnames(sSet) <- c("flow")
sSet <- subset(sSet,abs(flow)>=0.5)
sSet <- sSet[order(sSet$flow), , drop = FALSE]
sSet$name <- factor(row.names(sSet), levels = row.names(sSet)[order(sSet$flow)])
ggplot(sSet, aes(x=name,y = flow)) +
    geom_bar(stat = 'identity') +
    labs(x='Variable', title="Correlation to Flow")
ggcorrplot(Mcor)
set.seed(1738)
newdf <- newdf[-c(1:24),]
sample = sample.int(n = nrow(newdf), size = floor(.8*nrow(newdf)), replace = F)
train = data.frame(newdf[sample, ]) #just the samples
test  = data.frame(newdf[-sample, ]) #everything but the samples
train <- subset(train, select = -c(Date))
#
train[,c(3:ncol(train))] <- sapply( train[,c(3:ncol(train))], as.numeric )
test <- subset(test, select = -c(Date))
testY <- sapply( test$flow, as.numeric)
test <- subset(test, select = -c(flow))
test[,c(3:ncol(test))] <- sapply(test[,c(3:ncol(test))], as.numeric)
library("gradDescent")
train <- train[,c(1:2,4:ncol(train),3)]
train.3 <- subset(train,as.integer(month)==8)
sgd.train <- ADAM(train[,c(3:ncol(train))], maxIter = 1000, seed = 1738)
sgd.train3 <- ADAM(train.3[,c(3:ncol(train.3))], maxIter = 1000, seed = 1738)
sgd.train
p_data <- prediction(sgd.train,test[,c(3:ncol(test))])
p_data.3 <- prediction(sgd.train3,test[,c(3:ncol(test))])
plot(p_data$V1,testY,
     xlab="predicted",ylab="actual",
     main="ADAM Linear Fit Monthly")
abline(a=0,b=1,col="blue")
text(40, 60, "perfect fit", col = "blue") 
abline(lm(testY ~ p_data$V1),col="green")
text(80, 30, "regression", col = "green") 
plot(p_data.3$V1,testY,
     xlab="predicted",ylab="actual",
     main="ADAM Linear Fit August only")
abline(a=0,b=1,col="blue")
text(10, 15, "perfect fit", col = "blue") 
abline(lm(testY ~ p_data$V1),col="green")
text(20, 15, "regression", col = "green") 
all <- data.frame(newdf[,c(3:ncol(newdf))])
all.date <- all$Date
all$Date <- NULL
all.Y <- data.frame(as.numeric(all$flow))
all3.Y <- subset(all.Y,as.integer(newdf.d$month)==8)
all$flow <- NULL
all.3 <- subset(all,as.integer(newdf.d$month)== 8)
all <- sapply(all, as.numeric)
all.3 <- sapply(all.3, as.numeric)
p_data <- prediction(sgd.train,all)
p_data.3 <- prediction(sgd.train3,all.3)
all.Z <- data.frame(as.Date(all.date),all.Y,p_data$V1)
names(all.Z)[names(all.Z)=="as.Date.all.date."] <- "date"
names(all.Z)[names(all.Z)=="as.numeric.all.flow."] <- "actual"
names(all.Z)[names(all.Z)=="p_data.V1"] <- "predicted"
ggplot(all.Z, aes(date,actual)) + 
  ggtitle("Actual Flow vs Predicted Monthly") +
  geom_line(aes(y=actual, colour="actual")) + 
  geom_line(aes(y=predicted, colour="predicted"))
all3.Z <- data.frame(all3.Y,p_data.3$V1)
all3.Z$dates <- subset(df$year,df$month==8)
names(all3.Z)[names(all3.Z)=="as.numeric.all.flow."] <- "actual"
names(all3.Z)[names(all3.Z)=="p_data.3.V1"] <- "predicted"
all3.Z$dates <- as.integer(all3.Z$dates)
ggplot(all3.Z, aes(x=dates, y=actual)) + 
  ggtitle("Actual Flow vs Predicted, August only") +
  geom_line(aes(y=actual, colour="actual")) + 
  geom_line(aes(y=predicted, colour="predicted"))
library('randomForest')
train.Y <- train[,c(ncol(train))]
train.X <- train[,c(1:ncol(train)-1)]
rf_model <- randomForest(train.X, y = train.Y ,
                         ntree = 500, importance = TRUE)
levels(test$year) <- levels(train.X$year)
y_pred = predict(rf_model , test)
plot(y_pred,testY,
     xlab="predicted",ylab="actual",
     main="Random Forest Fit")
abline(a=0,b=1,col="blue")
abline(lm(testY ~ y_pred),col="green")
text(150, 30, "regression", col = "green") 
rf_model$importance
varImpPlot(rf_model,main="Important variables for Flow",
           n.var=16,type=1)

