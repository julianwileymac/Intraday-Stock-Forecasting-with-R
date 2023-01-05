# RNN - LSTM

library(tensorflow)
library(keras)
library(arrow)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(mice)
library(tseries) 
library(quantmod) 
library(forecast) 
library(vars)
library(lmtest)

setwd('C:/Users/julia/Documents/College/Fall 2022/Dynamic Modeling and Forecasting in Big Data/Final Project/')
aapl = read_parquet('AAPL.parquet')
fact = read_parquet('aapl_factors.parquet')

str(aapl)

pca = prcomp(aapl, scale=TRUE)

lstm_mod <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words + 1, output_dim = 32) %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")

lstm_mod