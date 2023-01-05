# Final Project - Dynamic Factor Models

library(dplyr)
library(quantmod)
library(tseries)
library(arrow)
library(tidyr)
library(strucchange)
library(vars)
library(forecast)
library(lubridate)
library(ggplot2)

setwd('C:/Users/julia/Documents/College/Fall 2022/Dynamic Modeling and Forecasting in Big Data/Final Project/WileyJulianFinal')
df = read_parquet('aapl_factors.parquet')


# DFM

df = df[,-32]

train = subset(df, date(DT) < as.Date("2022-10-02"))
test = subset(df, date(DT) >= as.Date("2022-10-02"))
test = na.omit(test)

df.hour = train %>%
  mutate(dt_aggregate = floor_date(DT, unit = "hour")) %>%
  group_by(dt_aggregate) %>%
  summarise(returns = sum(fwd1min, na.rm = TRUE))


df1 =na.omit(train[,-1])# Remove DateTime Column
df2 = prcomp(df1, scale=TRUE)
summary(df2)
# PC1: 53%, PC2:21%, PC3: 6.4%, PC4: 5.9%, PC5: 3.4%. Accumulated: 90%
df2$center
df2$scale
df2$rotation 
pc = df2$x
pca1 = cbind(train[,1], pc)

ggplot(pca1,aes(x=DT,y=PC1)) + geom_line() # 
ggplot(pca1,aes(x=DT,y=PC2)) + geom_line() # 
ggplot(pca1,aes(x=DT,y=PC3)) + geom_line() # 
ggplot(pca1,aes(x=DT,y=PC4)) + geom_line() # 
ggplot(pca1,aes(x=DT,y=PC5)) + geom_line() # 

pc.minute = ts(pca1[,-1])
pc.hour = pca1 %>%
  mutate(dt_aggregate = floor_date(DT, unit = "hour")) %>%
  group_by(dt_aggregate) %>%
  summarise(across(PC1:PC33, mean, .names = "mean_{.col}"))
  
data = data.frame(df.hour[,-1],pc.hour[,-1])

model = lm(returns~.,data=data)
summary(model)
# Adj R2 = 0.05035
# Adj R2 = 0.9979

# ARIMA
fact_arima = auto.arima(df1$fwd1min)
fcst01 = forecast(fact_arima, 7679)
fcst_arima = as.vector(fcst01$mean)
rmse_arima = sqrt(mean((fcst_arima-as.vector(ts(test[,34])))^2))
rmse_arima  # 0.0007242055

# Structural Time Series / State Space Model
ssm = StructTS(df1$fwd1min, type = "trend")
plot(forecast(ssm, h = 7679))
fcst03 = forecast(ssm, h = 7679)
fcst_ssm = as.vector(fcst03$mean)
fcst_ssm
rmse_ssm = sqrt(mean((fcst_ssm-as.vector(ts(test[,34])))^2))
rmse_ssm  # 0.0007241101

ssm1 = StructTS(df1$fwd1min, type = "level")
plot(forecast(ssm1, h = 7679))
fcst04 = forecast(ssm1, h = 7679)
fcst_ssm1 = as.vector(fcst04$mean)
fcst_ssm1
rmse_ssm1 = sqrt(mean((fcst_ssm1-as.vector(ts(test[,34])))^2))
rmse_ssm1  # 0.000724528

# DFM
var01 = VAR(pc.hour[,-1], p=60, type="both")  
summary(var01)
# multiple r2 = 0.9575 
# adj r2  = 0.9148

fcst02 = predict(var01, n.ahead = 7679, ci=0.95)
fcst02

pc.fcst = cbind(fcst02$fcst[[1]][,1], fcst02$fcst[[2]][,1],fcst02$fcst[[3]][,1],fcst02$fcst[[4]][,1],
                fcst02$fcst[[5]][,1],fcst02$fcst[[6]][,1], fcst02$fcst[[7]][,1],fcst02$fcst[[8]][,1],fcst02$fcst[[9]][,1],
                fcst02$fcst[[9]][,1],fcst02$fcst[[10]][,1], fcst02$fcst[[11]][,1],fcst02$fcst[[12]][,1],fcst02$fcst[[13]][,1],
                fcst02$fcst[[14]][,1],fcst02$fcst[[15]][,1], fcst02$fcst[[16]][,1],fcst02$fcst[[17]][,1],fcst02$fcst[[18]][,1],
                fcst02$fcst[[19]][,1],fcst02$fcst[[21]][,1], fcst02$fcst[[22]][,1],fcst02$fcst[[23]][,1],fcst02$fcst[[24]][,1],
                fcst02$fcst[[25]][,1],fcst02$fcst[[26]][,1], fcst02$fcst[[27]][,1],fcst02$fcst[[28]][,1],fcst02$fcst[[29]][,1],
                fcst02$fcst[[30]][,1],fcst02$fcst[[31]][,1], fcst02$fcst[[32]][,1], fcst02$fcst[[33]][,1])
pc.fcst

input=data.frame(pc.fcst)
colnames(input)=c("mean_PC1","mean_PC2","mean_PC3","mean_PC4","mean_PC5", "mean_PC6",
                  "mean_PC7","mean_PC8","mean_PC9","mean_PC10","mean_PC11",
                  "mean_PC12","mean_PC13","mean_PC14","mean_PC15","mean_PC16",
                  "mean_PC17","mean_PC18","mean_PC19","mean_PC20","mean_PC21",
                  "mean_PC22","mean_PC23","mean_PC24","mean_PC25","mean_PC26",
                  "mean_PC27","mean_PC28","mean_PC29","mean_PC30","mean_PC31",
                  "mean_PC32","mean_PC33")
fcst_dfm = predict(model, input)
fcst_dfm

rmse_dfm = sqrt(mean((fcst_dfm-as.vector(GDPC1[299:302,]))^2))
rmse_dfm

