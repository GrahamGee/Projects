#Housing Analysis
library(ggplot2)
set.seed(1234)

#All cleaning data
dat<-read.csv("~/Data Analytics/ECON 3856 Housing Project/canadian-us-residential-mortgage-arrears-foreclosure-rates-2002-2018-q1-en.csv",header=FALSE)
#Cleaning unecessary rows, for years with quarters, use Q1
dat<-as.data.frame(dat[c(-1,-4,-5,-6,-7,-16,-20,-21,-22,-25,-23:-44),c(-14:-16,-18:-20,-22:-24,-26:-28,-30:-32)])
#Get column names
rNames<-as.integer(substr(dat[1,],1,4))
#Assign column names
colnames(dat)<-rNames
#remove column names as entries
dat<-dat[-1,]

# Prepare data for Alberta
years <- rNames[-1]
albertaDat <- as.numeric(dat[3, 2:18]) 
quebecDat <- as.numeric(dat[8, 2:18]) 
ontarioDat <- as.numeric(dat[7, 2:18]) 
BCDat <- as.numeric(dat[5, 2:18]) 
SDat <- as.numeric(dat[9, 2:18]) 
MDat <- as.numeric(dat[6, 2:18])
ADat <- as.numeric(dat[4,2:18])
canadaDat<- as.numeric(dat[1,2:18])
usDat<-as.numeric(dat[10,2:18])


# Combine into a data frame for ggplot
provinceDF <- data.frame(Years = years,ArrearsRateA = albertaDat,ArrearsRateB = BCDat,ArrearsRateO = ontarioDat,ArrearsRateQ = quebecDat,ArrearsRateC = canadaDat,ArrearsRateM=MDat,ArrearsRateS=SDat,ArrearsRateAA=ADat,ArrearsRateUS=usDat)

#Plot of foreclosure rates in 4 provinces 
par(mfrow = c(2, 2))
plot(x=provinceDF$Years,y=provinceDF$ArrearsRateA,ylim=c(0,1.5),pch=16,cex=0.5,xlab="Years",ylab="Arrears rate(%)",main="Alberta")
lines(x=provinceDF$Years,y=provinceDF$ArrearsRateA)
plot(x=provinceDF$Years,y=provinceDF$ArrearsRateB,ylim=c(0,1.5),pch=16,cex=0.5,xlab="Years",ylab="Arrears rate(%)",main="British Columbia")
lines(x=provinceDF$Years,y=provinceDF$ArrearsRateB)
plot(x=provinceDF$Years,y=provinceDF$ArrearsRateO,ylim=c(0,1.5),pch=16,cex=0.5,xlab="Years",ylab="Arrears rate(%)",main="Ontario")
lines(x=provinceDF$Years,y=provinceDF$ArrearsRateO)
plot(x=provinceDF$Years,y=provinceDF$ArrearsRateQ,ylim=c(0,1.5),pch=16,cex=0.5,xlab="Years",ylab="Arrears rate(%)",main="Quebec")
lines(x=provinceDF$Years,y=provinceDF$ArrearsRateQ)
par(mfrow = c(1, 1))

plot(x=provinceDF$Years,y=provinceDF$ArrearsRateC,ylim=c(0,1.5),pch=16,cex=0.5,xlab="Years",ylab="Arrears rate(%)",main="Canada")
lines(x=provinceDF$Years,y=provinceDF$ArrearsRateC)

datAll<-provinceDF[1:6]
plot(x=datAll$Years,y=datAll$ArrearsRateC,type="l",ylim=c(0,1.5),xlab="Years",ylab="Arrears rate(%)",main="Canada vs. Four Provinces")
lines(datAll$Years,datAll$ArrearsRateC,col="red",lwd=2)
lines(datAll$Years,datAll$ArrearsRateQ,col="blue",lwd=2)
lines(datAll$Years,datAll$ArrearsRateO,col="green",lwd=2)
lines(datAll$Years,datAll$ArrearsRateA,col="purple",lwd=2)
lines(datAll$Years,datAll$ArrearsRateB,col="orange",lwd=2)

legend("topright", legend = c("Canada", "Quebec", "Ontario", "Alberta", "BC"),
       col = c("red", "blue", "green", "purple", "orange"), lwd = 2)

mod1<-lm(ArrearsRateC ~ ArrearsRateA + ArrearsRateB + ArrearsRateO + ArrearsRateQ, data=datAll)
summary(mod1)

dwtest(mod1)

plot1<-ggplot(data=provinceDF,aes(x=Years,y=ArrearsRateA),) +
  geom_point(color="blue") +
  geom_line(color = "blue") +
  scale_y_continuous(limits = c(0, 1.5)) +
  ggtitle("Arrears rate in Alberta in (%)") +
  xlab("Years") +
  ylab("Arrears rate")

plot(x=provinceDF$Years,y=provinceDF$ArrearsRateM,type="l",ylim=c(0,1.75),xlab="Years",ylab="Arrears rate(%)",main="Atlantic Provinces vs. Prarie Provinces")
lines(provinceDF$Years,provinceDF$ArrearsRateM,col="blue",lwd=2)
lines(provinceDF$Years,provinceDF$ArrearsRateS,col="green",lwd=2)
lines(provinceDF$Years,provinceDF$ArrearsRateB,col="cyan",lwd=2)
lines(provinceDF$Years,provinceDF$ArrearsRateA,col="purple",lwd=2)
lines(provinceDF$Years,provinceDF$ArrearsRateAA,col="red",lwd=2)


legend("topright", legend = c("Manitoba", "Saskatchewan", "BC", "Alberta", "Atlantic"),
       col = c("blue", "green", "cyan", "purple","red"), lwd = 2)
  
plot(x=provinceDF$Years,y=provinceDF$ArrearsRateUS,type="l",ylim=c(0,5),xlab="Years",ylab="Arrears rate(%)",main="Canada vs. United States")
lines(provinceDF$Years,provinceDF$ArrearsRateUS,col="blue",lwd=2)
lines(provinceDF$Years,provinceDF$ArrearsRateC,col="red",lwd=2)
legend("topright", legend = c("US", "CA"),
       col = c("blue", "red"), lwd = 2)

mod2 <- lm(ArrearsRateC ~ ArrearsRateUS,data=provinceDF)
summary(mod2)

mod3 <- lm(sqrt(ArrearsRateC) ~ ArrearsRateUS,data=provinceDF)
summary(mod3)
