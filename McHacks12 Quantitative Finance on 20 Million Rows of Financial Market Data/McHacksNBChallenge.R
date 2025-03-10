library(ggplot2)
library(tseries)
library(forecast)

#McHacks National Bank Challenge - Graham Gee & Hoang Nam Chu
set.seed(1234)
P15C0<-read.csv("~/Data Analytics/MCHacks/TrainingData/TrainingData/Period15/Period15/C/market_data_C_0.csv",header=FALSE)
P15C1<-read.csv("~/Data Analytics/MCHacks/TrainingData/TrainingData/Period15/Period15/C/market_data_C_1.csv",header=FALSE)
P15C2<-read.csv("~/Data Analytics/MCHacks/TrainingData/TrainingData/Period15/Period15/C/market_data_C_2.csv",header=FALSE)
P15C3<-read.csv("~/Data Analytics/MCHacks/TrainingData/TrainingData/Period15/Period15/C/market_data_C_3.csv",header=FALSE)
P15C4<-read.csv("~/Data Analytics/MCHacks/TrainingData/TrainingData/Period15/Period15/C/market_data_C_4.csv",header=FALSE)
P15C5<-read.csv("~/Data Analytics/MCHacks/TrainingData/TrainingData/Period15/Period15/C/market_data_C_5.csv",header=FALSE)
P15C6<-read.csv("~/Data Analytics/MCHacks/TrainingData/TrainingData/Period15/Period15/C/market_data_C_6.csv",header=FALSE)
P15C7<-read.csv("~/Data Analytics/MCHacks/TrainingData/TrainingData/Period15/Period15/C/market_data_C_7.csv",header=FALSE)

#Get names
P15C<-rbind(P15C0[-1,],P15C1,P15C2,P15C3,P15C4,P15C5,P15C6,P15C7)
P15C<-as.data.frame(P15C)
colnames(P15C)<-P15C0[1,]

#P15C$timestamp <- as.POSIXct(P15C$timestamp, format = "%Y-%m-%d %H:%M:%S")

###Saving this to ask Prof why this is bugged

# Plot BidPrice vs Timestamp
#ggplot(P15C, aes(x = 1:length(P15C[,1]), y = bidPrice)) +
#  geom_line(color = "blue") +
 # labs(
  #  title = "Bid Price Over Time",
   # x = "Timestamp",
    #y = "Bid Price"
#  ) +
 # theme_minimal()

#Length
#P15CL<-length(P15C[,1])
P15CL<-5000
par(mfrow=c(2,1))
#Plotting Bid vs. Ask Price
plot(x=1:P15CL,y=P15C$bidPrice[1:P15CL],type="l",xlab="Time",ylab="Bid & Ask Price",main="Bid Price(B) vs. Ask Price(R)",col="blue")
cat("Start Time: ",P15C$timestamp[1],". End time: ",P15C$timestamp[P15CL])
lines(1:P15CL, P15C$askPrice[1:P15CL], col = "red", lwd = 2)

#We will use PCA to find the leading indicator


#Plot Volume
#Plotting Bid vs. Ask Price
plot(x=1:P15CL,y=P15C$bidVolume[1:P15CL],type="l",xlab="Time",ylab="Bid & Ask Volume",col="blue")
lines(1:P15CL, P15C$askVolume[1:P15CL], col = "red", lwd = 2)


#Analysis with ARIMA model

#Is the data stationary i.e. constant mean and variance
#Null hypothesis states the data is not stationary
adf.test(P15C$bidPrice, alternative = "stationary", k = trunc((P15CL - 1)^(1/3)))
#We rejected the null hypotheses at p=0.01, so the data is stationary, so we are able to use this model

#ARIMA Model for bid price
P15C$bidPrice <- as.numeric(as.character(P15C$bidPrice))
P15C$askPrice <- as.numeric(as.character(P15C$askPrice))
#Need to subset for ARIMA as its very computationally heavy with large n
P15Cbid_subset <- tail(P15C$bidPrice, 25000)
ARIMA_P15C_bidPrice<-auto.arima(P15Cbid_subset,seasonal=FALSE)
summary(ARIMA_P15C_bidPrice)
par(mfrow=c(2,2))
plot(ARIMA_P15C_bidPrice)
forecast1Min<-forecast(ARIMA_P15C_bidPrice, h = 10000)

par(mfrow=c(1,1))
plot(forecast1Min)

###Implementing trading strategy using ARIMA
P15C$Spread<-P15C$askPrice-P15C$bidPrice

#Difference in prices from last one
bidPriceDiff<-diff(P15C$bidPrice,differences=1)
bidPriceDiff<-c(0,bidPriceDiff)
P15C<-cbind(P15C,bidPriceDiff)

askPriceDiff<-diff(P15C$askPrice,difference=1)
askPriceDiff<-c(0,askPriceDiff)
P15C<-cbind(P15C,askPriceDiff)

#Models for trading
tradeHorizon<-25000
bid_model<- auto.arima(P15C$bidPriceDiff[1:tradeHorizon], seasonal = FALSE)
ask_model <- auto.arima(P15C$askPriceDiff[1:tradeHorizon], seasonal = FALSE)
summary(bid_model)
summary(ask_model)

#Forecasts
bid_forecast<-forecast(bid_model, h = 10)
ask_forecast<-forecast(ask_model, h = 10)

# Extract point forecasts
bid_forecasted_prices<-bid_forecast$mean
ask_forecasted_prices<-ask_forecast$mean

#Get position
P15C$position <- ifelse(
  lag(P15C$bidPrice) > P15C$askPrice,  # Buy Signal
  1,
  ifelse(
    lag(P15C$askPrice) < P15C$bidPrice,  # Sell Signal
    -1,
    0  # Hold Signal
  )
)

#Simulate trade
P15C$PnL <- ifelse(P15C$position == 1, P15C$askPrice - lag(P15C$bidPrice),  # Buy at ask, sell at bid
  ifelse(P15C$position == -1, lag(P15C$askPrice) - P15C$bidPrice, 0)  # Sell at bid, buy at ask
)

###Trading Strategy based on Spread
threshold<-0.25
positionTradeSpread <- ifelse(P15C$Spread > threshold, 1, 0)  # Trade if spread exceeds threshold
P15C<-cbind(P15C,positionTradeSpread)

# Calculate PnL based on position
P15C$PnL <- ifelse(
  P15C$positionTradeSpread == 1,  # Long position
  P15C$askPrice - lag(P15C$bidPrice),
  ifelse(
    P15C$positionTradeSpread == -1,  # Short position
    lag(P15C$askPrice) - P15C$bidPrice,
    0  # No trade
  )
)

# Calculate cumulative PnL
P15C$cumulative_PnL <- cumsum(P15C$PnL)

par(mfrow=c(1,1))
# Plot cumulative PnL
plot(P15C$cumulative_PnL, type = "l", col = "blue", main = "Cumulative PnL from Spread Strategy",
     xlab = "Time Step", ylab = "Cumulative Profit/Loss")