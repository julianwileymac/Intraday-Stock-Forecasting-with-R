#################################################################
# Julian Wiley - Data Preprocessing                             #
#################################################################

library(arrow)
library(quantmod)
library(tseries)
library(forecast)
library(ggplot2)
library(highfrequency)
library(data.table)
library(TTR)
library(tidyr)
library(dplyr)

setwd("C:/Users/julia/Documents/College/Fall 2022/Dynamic Modeling and Forecasting in Big Data/Final Project/")
aapl = read_parquet('AAPL.parquet')

###############################################################
#                 Preprocessing                               #
###############################################################

#################################################
# Process Trade and Quote dataset               #
#################################################

# drop time offsets

offset_cols <- names(aapl) %in% c('OpenBarTimeOffset','FirstTradeTimeOffset',
                                  'HighBidTimeOffset','HighAskTimeOffset',
                                  'HighTradeTimeOffset','LowBidTimeOffset',
                                  'LowAskTimeOffset','LowTradeTimeOffset',
                                  'CloseBarTimeOffset','LastTradeTimeOffset')
aapl <- aapl[!offset_cols]


#################################################
# Generate Indicator dataset                    #
#################################################

# Since the price data is not stationary, we will use returns for forcasting

factors = aapl[,2] # Create empty dataframe for factors

# Calculate 1 min returns 
factors$ret1min <- (aapl$LastTradePrice / aapl$FirstTradePrice ) - 1
# Calculate 5 min returns
factors$ret5min <- (aapl$LastTradePrice / stats::lag(aapl$FirstTradePrice, k=-4)) -1
factors$ret5min <- (aapl$LastTradePrice / Lag(aapl$FirstTradePrice, k=4)) -1
# Calculate 10 min returns
# factors$ret10min <- (aapl$LastTradePrice / stats::lag(aapl$FirstTradePrice, k=-9)) -1
factors$ret10min <- (aapl$LastTradePrice / Lag(aapl$FirstTradePrice, k=9)) -1

# Calculate 15 min returns
# factors$ret15min <- (aapl$LastTradePrice / stats::lag(aapl$FirstTradePrice, k=-14)) -1
factors$ret15min <- (aapl$LastTradePrice / Lag(aapl$FirstTradePrice, k=14)) -1

# Calculate 20 min returns
# factors$ret20min <- (aapl$LastTradePrice / stats::lag(aapl$FirstTradePrice, k=-19)) -1
factors$ret20min <- (aapl$LastTradePrice / Lag(aapl$FirstTradePrice, k=19)) -1

# Calculate 25 min returns
# factors$ret25min <- (aapl$LastTradePrice / stats::lag(aapl$FirstTradePrice, k=-24)) -1
factors$ret25min <- (aapl$LastTradePrice / Lag(aapl$FirstTradePrice, k=24)) -1

# Calculate 30 min returns
# factors$ret30min <- (aapl$LastTradePrice / stats::lag(aapl$FirstTradePrice, k=-29)) -1
factors$ret30min <- (aapl$LastTradePrice / Lag(aapl$FirstTradePrice, k=29)) -1

# Calculate 60 min returns
# factors$ret60min <- (aapl$LastTradePrice / stats::lag(aapl$FirstTradePrice, k=-59)) -1
factors$ret60min <- (aapl$LastTradePrice / Lag(aapl$FirstTradePrice, k=59)) -1

# Calculate 120 min returns
# factors$ret120min <- (aapl$LastTradePrice / stats::lag(aapl$FirstTradePrice, k=-119)) -1
factors$ret120min <- (aapl$LastTradePrice / Lag(aapl$FirstTradePrice, k=119)) -1

# Calculate 180 min returns
# factors$ret180min <- (aapl$LastTradePrice / stats::lag(aapl$FirstTradePrice, k=-179)) -1
factors$ret180min <- (aapl$LastTradePrice / Lag(aapl$FirstTradePrice, k=179)) -1

colnames(factors) = c("DT","ret1min", "ret5min","ret10min","ret15min", "ret20min","ret25min","ret30min","ret60min","ret120min","ret180min")
str(factors)
########################
# Calculate Indicators #
########################

# Calculate EMA
factors$ema5 <- EMA(aapl$LastTradePrice, n=5, wilder=FALSE, ratio=NULL) # EMA 5
factors$ema10 <- EMA(aapl$LastTradePrice, n=10, wilder=FALSE, ratio=NULL) # EMA 10
factors$ema15 <- EMA(aapl$LastTradePrice, n=15, wilder=FALSE, ratio=NULL) # EMA 15
factors$ema20 <- EMA(aapl$LastTradePrice, n=20, wilder=FALSE, ratio=NULL) # EMA 20
factors$ema25 <- EMA(aapl$LastTradePrice, n=25, wilder=FALSE, ratio=NULL) # EMA 25
factors$ema30 <- EMA(aapl$LastTradePrice, n=30, wilder=FALSE, ratio=NULL) # EMA 30
factors$ema60 <- EMA(aapl$LastTradePrice, n=60, wilder=FALSE, ratio=NULL) # EMA 60
factors$ema120 <- EMA(aapl$LastTradePrice, n=120, wilder=FALSE, ratio=NULL) # EMA 120
factors$ema180 <- EMA(aapl$LastTradePrice, n=180, wilder=FALSE, ratio=NULL) # EMA 180

# Calculate MACD
factors$macd <- MACD(aapl$LastTradePrice, nFast=12, nSlow=26, nSig=9, maType="EMA", percent = TRUE)
# Calculate RSI
factors$rsi <- RSI(aapl$LastTradePrice, n=14, maType="EMA")
# Calculate MFI 
factors$mfi <- MFI(aapl[, c(11,20,37)], aapl$Volume, n=14)
# Calculate Volatility 
ohlc = as.data.table(aapl[,c(2, 11,20,29,37)], na.rm=TRUE)
colnames(ohlc) = c("Date", "Open", "High", "Low","Close")

factors$vol5min <- runVar(aapl$LastTradePrice, y=NULL, n=5, sample = TRUE, cumulative = FALSE)
factors$vol10min <- runVar(aapl$LastTradePrice, y=NULL, n=10, sample = TRUE, cumulative = FALSE)
factors$vol30min <- runVar(aapl$LastTradePrice, y=NULL, n=30, sample = TRUE, cumulative = FALSE)
factors$vol60min <- runVar(aapl$LastTradePrice, y=NULL, n=60, sample = TRUE, cumulative = FALSE)
factors$vol120min <- runVar(aapl$LastTradePrice, y=NULL, n=120, sample = TRUE, cumulative = FALSE)
factors$vol180min <- runVar(aapl$LastTradePrice, y=NULL, n=180, sample = TRUE, cumulative = FALSE)

# Calculate Stochastic Oscillator
factors$stoch <- stoch(aapl[,c(20,29,37)], nFastK = 14, nFastD = 3, nSlowD = 3, maType="EMA")

# Calculate Commodity Channel Index (CCI)
factors$cci <- CCI(aapl[,c(20,29,37)], n=20)

# Calculate Bollinger Bands

factors$bbands <- BBands(aapl[,c(20,29,37)], n=20)

# Chaikin Volatility
factors$chaikinvol <- chaikinVolatility(aapl[,c(20,29)],n=10)

# On Balance Volume
factors$obv <- OBV(aapl[,37], aapl$Volume)

# Calculate Rate of Change
factors$roc <- ROC(aapl[,37], n=1)

# Calculate Momentum
factors$mom <- momentum(aapl[,37])

# Signal to Noise Ratio
factors$snr <- SNR(aapl[,c(20,29,37)], n=10)

# Calculate Volume Weighted Average Price (VWAP)
factors$vwap <- VWAP(aapl$LastTradePrice, aapl$Volume, n=10)

# Future ret1min fore forecasting
factors <- factors %>%
  mutate(fwd1min = lead(ret1min, order_by=DT))

#############################################
# highfreq                                  #
#############################################

rvar<- rRVar(as.data.table(factors)[,c(1,2)], alignBy="minutes", alignPeriod = 1, makeReturns = FALSE)
rvar

factors2 = as.data.table(factors, na.rm=TRUE)
rvar <- rRVar(factors2, alignBy="minutes", alignPeriod=1, makeReturns=FALSE)
rvar
plot(rvar[,DT], rvar[, ret1min], xlab ="Date", ylab = "Realized Variance", type="l")


rskew <- rSkew(factors2, alignBy="minutes", alignPeriod=1, makeReturns=FALSE)
rskew

rkurt <- rKurt(factors2, alignBy="minutes", alignPeriod=1, makeReturns=FALSE)
rkurt

hf_data <- as.data.table(aapl)

colnames(hf_data)[2] <- c("DT")
colnames(hf_data)[37] <- c("PRICE")

rremedi <- ReMeDI(hf_data, kn = 2, lags=1:8) 
rremedi

qpv <- rQPVar(rData = hf_data, alignBy = "minutes", alignPeriod = 1, makeReturns=TRUE)
qpv 

sdrift <- spotDrift(data = hf_data, alignBy = "minutes", alignPeriod = 1)
sdrift

svol <- spotVol(data=hf_data, method="garch", model="sGARCH", alignBy = "minutes", alignPeriod = 1)
svol
# HAR model test
str(factors2)
hardata <- as.xts(factors2)
har <- HARmodel(data = hardata, periods = c(1,5,10,15,20,25,30,60,120,180), periodsJ = c(1,5,10,15,20,25,30,60,120,180), periodsQ=c(1), RVest = c("rCov"), type = "HARQ", h=1,
                transform = NULL, inputType = "RM")
summary(har)
plot(har)
predict(har)

########################################
# Deal with NaN and inf values         #
########################################

factors <- factors %>%
  fill(ret1min, .direction='up')

fact1 %>% filter(is.infinite(ret1min))

df <- fact1 %>%
  slice(180:240734) %>%
  # select(!c('roc','obv')) %>%
  mutate(across(ret1min:vwap, ~na_if(.,Inf)))%>%
  fill(everything(), .direction = "up") %>%
  mutate(fwd1min = lead(ret1min, order_by=DT))
#####################################
# Create Categorical Variables      #
#####################################


#########################
# Save Dataset          #
#########################
write_parquet(df, "aapl_factors.parquet")
