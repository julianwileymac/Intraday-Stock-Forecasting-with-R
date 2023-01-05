# Linear and Time Series Models

library(dplyr)
library(tseries)
library(quantmod)
library(ggplot2)
library(forecast)
library(corrplot)
library(arrow)
library(caret)
library(tidyr)
library(lubridate)

setwd("C:/Users/julia/Documents/College/Fall 2022/Dynamic Modeling and Forecasting in Big Data/Final Project/WileyJulianFinal")
aapl = read_parquet('AAPL.parquet')
df = read_parquet('aapl_factors.parquet')

str(df)
###############################################################
#                 Linear Models                               #
###############################################################

df <- df %>%
#  slice(180:240734) %>%
  select(!c('roc')) %>%
  mutate(across(ret1min:vwap, ~na_if(.,Inf)))%>%
  fill(everything(), .direction = "up")

# Basic Linear Regression

# Model 1 - use all variables
model1 <- lm(fwd1min ~., data=na.omit(df))
summary(model1)
# Adj R2 = 0.0002249

# Model 2  - use just returns

model2 <- lm(fwd1min ~ ret1min + ret5min + 
               ret10min + ret15min + ret20min + 
               ret25min + ret30min + ret60min + 
               ret120min + ret180min, data=df)

summary(model2)
# adj R2 = 0.000247

# Model 3 - EMAs
model3 <- lm(fwd1min ~ ema5 + ema10 + ema15 + 
               ema20 + ema25 + ema30 + ema60 + 
               ema120 + ema180, data=df)
summary(model3)
# Adj R2 = -0.00001986

# Model 4 - Volatility Estimators
model4 <- lm(fwd1min ~ vol5min + vol10min + vol30min + 
               vol60min + vol120min + vol180min, data=df)
summary(model4)
# Adj R2 = -0.00002208

# Model 5 - Only indicators

model5 <- lm(fwd1min ~ macd +rsi +mfi + bbands + chaikinvol+
               snr + vwap, data = df)
summary(model5)
# Adj R2 = 0.00004432

# Model 6 - datetime factors
model6 <- lm(fwd1min ~ . + factor(minute(DT)) + factor(day(DT)) + factor(hour(DT)), data=df)
summary(model6)
# Adj R2 = 0.0001923


# Full Lagged Model
df1 = ts(df)
df3 = cbind(df1, stats::lag(df1, k=-1), stats::lag(df1, k=-2), stats::lag(df1, k=-3),
            stats::lag(df1, k=-4), stats::lag(df1, k=-5), stats::lag(df1, k=-10),
            stats::lag(df1, k=-15), stats::lag(df1, k=-30), stats::lag(df1, k=-60),
            stats::lag(df1, k=-120), stats::lag(df1, k=-180))

lagged_model1 = lm(df1.fwd1min ~., data = df3)
summary(lagged_model1)


# Auto-Correlation Function
acf(df,lag.max=60, na.action = na.pass)
