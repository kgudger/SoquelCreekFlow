dfRain <- read.csv("RainQ.csv")
dfFlow <- read.csv("FlowQ.csv")
df <- merge(dfFlow,dfRain, by=c("year","quarter"),all=TRUE)
df$coredata.quarterly.[is.na(df$coredata.quarterly.)] <- round(mean(df$coredata.quarterly., na.rm = TRUE))
df$coredata.morain.[is.na(df$coredata.morain.)] <- round(mean(df$coredata.morain., na.rm = TRUE))
df$coredata.motemp.[is.na(df$coredata.motemp.)] <- round(mean(df$coredata.motemp., na.rm = TRUE))
names(df)[names(df)=="coredata.quarterly."] <- "flow"
names(df)[names(df)=="coredata.morain."] <- "rain"
names(df)[names(df)=="coredata.motemp."] <- "temp"
df <- subset(df, select = -c(X.x,X.y))
library(ggcorrplot)
t <- subset(df, year >= 1989)
p <- ggplot(t, aes(quarter, flow))
p + geom_line(aes(colour = factor(year)))
p <- ggplot(t, aes(quarter, rain))
p + geom_line(aes(colour = factor(year)))
p <- ggplot(t, aes(quarter, temp))
p + geom_line(aes(colour = factor(year)))
library(xts)
df <- transform(df, Date = as.Date(paste(year, quarter, 1, sep = "-")))
newdf <- as.xts(df, order.by=df$Date)
newdf <- merge(newdf, rain1=lag(newdf$rain,1),
                      rain2=lag(newdf$rain,2),
                      rain3=lag(newdf$rain,3),
                      rain4=lag(newdf$rain,4),
                      rain5=lag(newdf$rain,5),
                      rain6=lag(newdf$rain,6),
                      rain7=lag(newdf$rain,7),
                      rain8=lag(newdf$rain,8),
                      temp1=lag(newdf$temp,1),
                      temp2=lag(newdf$temp,2),
                      temp3=lag(newdf$temp,3),
                      temp4=lag(newdf$temp,4))
newdf.d <- data.frame(newdf)
newdf.d$Date <- NULL
write.csv(newdf.d, file="AllsumQ.csv", na="", row.names=FALSE)
Mcor <- as.data.frame(newdf)
Mcor$Date = NULL
Mcor <- Mcor[-c(1:8),]
# Mcor is still good data to plot
corDat <- Mcor[,c(3:5,9,15,17)]
corDat <- corDat[order(corDat$flow), , drop = FALSE]
ggplot(corDat, aes(x=temp, y=flow, group = 1)) + 
  ggtitle("Flow vs. Temperature Quarterly") +
  geom_line(aes(y=flow)) +
  geom_smooth(method='lm',formula=y~x)
ggplot(corDat, aes(x=rain, y=flow, group = 1)) + 
  ggtitle("Flow vs. Rain Quarterly") +
  geom_line(aes(y=flow)) +
  geom_smooth(method='lm',formula=y~x)
ggplot(corDat, aes(x=rain.4, y=flow, group = 1)) + 
  ggtitle("Flow vs. Rain -4 Quarters") +
  geom_line(aes(y=flow)) +
  geom_smooth(method='lm',formula=y~x)
ggplot(corDat, aes(x=temp.2, y=flow, group = 1)) + 
  ggtitle("Flow vs. Temperature -2 Quarters") +
  geom_line(aes(y=flow)) +
  geom_smooth(method='lm',formula=y~x)
ggplot(corDat, aes(x=temp.4, y=flow, group = 1)) + 
  ggtitle("Flow vs. Temperature -4 Quarters") +
  geom_line(aes(y=flow)) +
  geom_smooth(method='lm',formula=y~x)

Mcor <- sapply(Mcor, as.numeric )
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
#
set.seed(1738)
newdf <- newdf[-c(1:8),]
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
#library("tidyverse")
train.3 <- subset(train,quarter== 3)
sgd.train <- ADAM(train[,c(3:ncol(train))], maxIter = 1000, seed = 1738)
sgd.train3 <- ADAM(train.3[,c(3:ncol(train.3))], maxIter = 1000, seed = 1738)
sgd.train
p_data <- prediction(sgd.train,test[,c(3:ncol(test))])
p_data.3 <- prediction(sgd.train3,test[,c(3:ncol(test))])
plot(p_data$V1,testY,
     xlab="predicted",ylab="actual",
     main="ADAM Linear Fit Quarterly")
abline(a=0,b=1,col="blue")
text(10, 15, "perfect fit", col = "blue") 
abline(lm(testY ~ p_data$V1),col="green")
text(20, 15, "regression", col = "green") 
plot(p_data.3$V1,testY,
     xlab="predicted",ylab="actual",
     main="ADAM Linear Fit Q3 only")
abline(a=0,b=1,col="blue")
text(10, 15, "perfect fit", col = "blue") 
abline(lm(testY ~ p_data$V1),col="green")
text(20, 15, "regression", col = "green") 
all <- newdf.d[-c(1:8),c(3:ncol(newdf.d))]
all$Date <- NULL
all.Y <- data.frame(as.numeric(all$flow))
all3.Y <- subset(all.Y,newdf.d$quarter==3)
all$flow <- NULL
all.3 <- subset(all,newdf.d$quarter== 3)
all <- sapply(all, as.numeric)
all.3 <- sapply(all.3, as.numeric)
p_data <- prediction(sgd.train,all)
p_data.3 <- prediction(sgd.train3,all.3)
all.Z <- data.frame(all.Y,p_data$V1)
all.Z$dates <- apply( dfFlow[ ,3:2] , 1 , paste , collapse = "" )
names(all.Z)[names(all.Z)=="as.numeric.all.flow."] <- "actual"
names(all.Z)[names(all.Z)=="p_data.V1"] <- "predicted"
all.Z$dates <- as.integer(all.Z$dates)
ggplot(all.Z, aes(x=dates, y=actual)) + 
  ggtitle("Actual Flow vs Predicted") +
  geom_line(aes(y=actual, colour="actual")) + 
  geom_line(aes(y=predicted, colour="predicted"))
all3.Z <- data.frame(all3.Y,p_data.3$V1)
all3.Z$dates <- subset(df$year,df$quarter==3)
names(all3.Z)[names(all3.Z)=="as.numeric.all.flow."] <- "actual"
names(all3.Z)[names(all3.Z)=="p_data.3.V1"] <- "predicted"
all3.Z$dates <- as.integer(all3.Z$dates)
ggplot(all3.Z, aes(x=dates, y=actual)) + 
  ggtitle("Actual Flow vs Predicted, Q3 only") +
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
text(45, 10, "regression", col = "green") 
rf_model$importance
varImpPlot(rf_model,main="Important variables for Quarterly Flow",
           n.var=16,type=1)

