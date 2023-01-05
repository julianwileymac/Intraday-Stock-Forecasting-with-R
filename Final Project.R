# Final Project - Julian wiley

# High Frequency Stock Return Prediction using Dynamic Models, 
# ARIMA 

library(highfreq)
library(dplyr)
library(tidyr)
library(tseries)
library(quantmod)
library(forecast)
library(arrow)
library(eDMA)
library(ggplot2)


setwd("C:/Users/julia/Documents/College/Fall 2022/Dynamic Modeling and Forecasting in Big Data/Final Project/")
aapl = read_parquet('AAPL.parquet')
fact = read_parquet('aapl_factors.parquet')

facts <- as.data.table(fact, na.rm=TRUE)
model1 <- DMA(fwd1min ~ ret1min + ret5min + ret10min, data=na.omit(df))

