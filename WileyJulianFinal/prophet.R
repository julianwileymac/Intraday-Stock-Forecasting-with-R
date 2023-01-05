# Univariate forecasting with Prophet

library(arrow)
library(dplyr)
library(tseries)
library(forecast)
library(quantmod)
library(ggplot2)
library(tidyr)
library(prophet)
library(lubridate)
library(plotly)

setwd('C:/Users/julia/Documents/College/Fall 2022/Dynamic Modeling and Forecasting in Big Data/Final Project/WileyJulianFinal')

aapl = read_parquet('AAPL.parquet')
df = read_parquet('aapl_factors.parquet')

####################################
# Check dataframes                 #
####################################

str(aapl)
summary(aapl)

str(df)
summary(df)


# train-test split

train = subset(df,DT <=as.Date("2022-10-02"))
test = subset(df,DT >as.Date("2022-10-02"))

train_aapl = subset(aapl, BarDateTime <= as.Date("2022-10-01"))
test_aapl = subset(aapl, BarDateTime > as.Date("2022-10-01"))

###################################
# prophet - close data            #
###################################

data = train_aapl[,c(1,37)]

colnames(data) <- c("ds","y")

m = prophet(data)
future = make_future_dataframe(m, periods = 9)
tail(future)

forecast = predict(m, future)

plot(m, forecast)
prophet_plot_components(m, forecast)

forecast1a <- subset(forecast1, ds > as.Date("2022-10-02"))

#####################################
# prophet - fwd1min                 #
#####################################

df1 = train[,c(1,34)]
colnames(df1) <- c("ds","y")

m2 = prophet(df1)

# R

future2 = make_future_dataframe(m2, freq=60, periods=7680)
tail(future2)

forecast2 = predict(m2, future2)

plot(m2, forecast2)
prophet_plot_components(m2, forecast2)

tail(forecast2[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

forecast2a <- subset(forecast2, ds > as.Date("2022-10-02"))

######################################
# RMSE                               #
######################################

rmse11 = sqrt(mean((forecast1a$yhat-aapl_test$LastTradePrice)^2))
rmse11


rmse12 = sqrt(mean((forecast2a$yhat-na.omit(test$fwd1min))^2))
rmse12 
# 0.0007595774



######################################
# Visualizations                     #
######################################



# Prophet - Close
data1 <- inner_join(forecast1a, aapl_test, by=c("ds"="DT"))

# data1 <- data.frame(forecast2a, subset(test, date(DT) <= as.Date("2022-10-06")))
fig <- plot_ly(data1, x = ~ds) 
fig <- fig %>% add_trace(y = ~yhat, name = 'yhat',mode = 'lines') 
fig <- fig %>% add_trace(y = ~fwd1min, name = 'fwd1min', mode = 'lines+markers') 
fig

# Prophet - fwd1min

ggplot() +
  geom_line(data=data2, aes(x=ds, y=yhat), color="blue") +  
  geom_line(data=data2, aes(x=ds, y=fwd1min), color="black") 


data2 <- inner_join(forecast2a, test, by=c("ds"="DT"))


diff_fig <- plot_ly(na.omit(data2), type = 'scatter', mode = 'lines')%>%
  #add_trace(x = ~quote_test$BarDateTime, y = ~quote_test$OpenBidPrice, name = 'GOOG')%>%
  add_trace(x = ~ds, y = ~yhat, name = 'yhat')%>%
  add_trace(x = ~ds, y = ~fwd1min, name = 'fwd1min')%>%
  layout(showlegend = T,xaxis = list(rangebreaks=
                                       list(
                                         list(bounds=list(16, 9),
                                              pattern="hour"),
                                         list(bounds=list('sat', 'mon'),
                                              pattern='day of week')
                                       ),#hide hours outside of 9am-5pm
                                     dtick=86400000.0/2,
                                     tickformat="%H:%M\n%b\n%Y"))

options(warn = -1)

diff_fig <- diff_fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)


diff_fig
