# Data Visualizations

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
library(lubridate)
library(plotly)
library(timetk)
library(processx)
library(orca)
library(corrplot)
library(DT)

setwd("C:/Users/julia/Documents/College/Fall 2022/Dynamic Modeling and Forecasting in Big Data/Final Project/")
aapl = read_parquet('AAPL.parquet')
fact = read_parquet('aapl_factors.parquet')

#####################################
# Trade and Quote Dataset           #
#####################################

hour(aapl$BarDateTime)

aapl_market <- aapl %>% 
  filter(hour(BarDateTime) >= 9 & hour(BarDateTime) <= 16)

aapl_premarket <- aapl %>% 
  filter(hour(BarDateTime) <= 9)

aapl_postmarket <- aapl %>%
  filter(hour(BarDateTime) > 16)


candle_fig <- aapl_market %>% plot_ly(x = ~BarDateTime, type="candlestick",
                               open = ~aapl_market$FirstTradePrice, close = ~aapl_market$LastTradePrice,
                               high = ~aapl_market$HighTradePrice, low = ~aapl_market$LowTradePrice) 
candle_fig <- candle_fig %>% layout(title = "Basic Candlestick Chart",showlegend = F,xaxis = list(rangebreaks=
                                                                                                    list(
                                                                                                      list(bounds=list(16, 9),
                                                                                                           pattern="hour"),
                                                                                                      list(bounds=list('sat', 'mon'),
                                                                                                           pattern='day of week')
                                                                                                    ),#hide hours outside of 9am-5pm
                                                                                                  dtick=86400000.0/2,
                                                                                                  tickformat="%H:%M\n%b\n%Y"))

candle_fig

orca(candle_fig, "aapl_trade_ohlc.svg")

bid_candle_fig <- aapl_market %>% plot_ly(x = ~BarDateTime, type="candlestick",
                                      open = ~aapl_market$OpenBidPrice, close = ~aapl_market$CloseBidPrice,
                                      high = ~aapl_market$HighBidPrice, low = ~aapl_market$LowBidPrice) 
bid_candle_fig <- candle_fig %>% layout(title = "Bid Candlestick Chart",showlegend = F,xaxis = list(rangebreaks=
                                                                                                    list(
                                                                                                      list(bounds=list(16, 9),
                                                                                                           pattern="hour"),
                                                                                                      list(bounds=list('sat', 'mon'),
                                                                                                           pattern='day of week')
                                                                                                    ),#hide hours outside of 9am-5pm
                                                                                                  dtick=86400000.0/2,
                                                                                                  tickformat="%H:%M\n%b\n%Y"))

bid_candle_fig

ask_candle_fig <- aapl_market %>% plot_ly(x = ~BarDateTime, type="candlestick",
                                          open = ~aapl_market$OpenAskPrice, close = ~aapl_market$CloseAskPrice,
                                          high = ~aapl_market$HighAskPrice, low = ~aapl_market$LowAskPrice) 
ask_candle_fig <- candle_fig %>% layout(title = "Ask Candlestick Chart",showlegend = F,xaxis = list(rangebreaks=
                                                                                                      list(
                                                                                                        list(bounds=list(16, 9),
                                                                                                             pattern="hour"),
                                                                                                        list(bounds=list('sat', 'mon'),
                                                                                                             pattern='day of week')
                                                                                                      ),#hide hours outside of 9am-5pm
                                                                                                    dtick=86400000.0/2,
                                                                                                    tickformat="%H:%M\n%b\n%Y"))

ask_candle_fig



# Combined charts
i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart

comb_fig <- aapl_market %>% plot_ly(x = ~BarDateTime, type="candlestick",
                      open = ~aapl_market$FirstTradePrice, close = ~aapl_market$LastTradePrice,
                      high = ~aapl_market$HighTradePrice, low = ~aapl_market$LowTradePrice)


comb_fig <- comb_fig %>% layout(yaxis = list(title = "Price"),xaxis = list(rangebreaks=
                                                                   list(
                                                                     list(bounds=list(16, 9),
                                                                          pattern="hour"),
                                                                     list(bounds=list('sat', 'mon'),
                                                                          pattern='day of week')
                                                                   ),#hide hours outside of 9am-5pm
                                                                 dtick=86400000.0/2,
                                                                 tickformat="%H:%M\n%b\n%Y"))

# plot volume bar chart
comb_fig2 <- aapl_market 
comb_fig2 <- comb_fig2 %>% plot_ly(x=~BarDateTime, y=~aapl_market$Volume, type='bar', name = "AAPL Volume") 
comb_fig2 <- comb_fig2 %>% layout(yaxis = list(title = "Volume"),xaxis = list(rangebreaks=
                                                                                list(
                                                                                  list(bounds=list(16, 9),
                                                                                       pattern="hour"),
                                                                                  list(bounds=list('sat', 'mon'),
                                                                                       pattern='day of week')
                                                                                ),#hide hours outside of 9am-5pm
                                                                              dtick=86400000.0/2,
                                                                              tickformat="%H:%M\n%b\n%Y"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=1,
                  label='1 YR',
                  step='year',
                  stepmode='backward'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward')
           ))

# subplot with shared x axis
comb_fig <- subplot(comb_fig, comb_fig2, heights = c(0.7,0.2), nrows=2,
               shareX = TRUE, titleY = TRUE)
comb_fig <- comb_fig %>% layout(title = paste("Apple",Sys.Date()),
                      xaxis = list(rangeselector = rs),
                      legend = list(orientation = 'h', x = 0.5, y = 1,
                                    xanchor = 'center', yref = 'paper',
                                    font = list(size = 10),
                                    bgcolor = 'transparent'))

comb_fig

quote_test <- aapl_market %>%
  select(c("BarDateTime","FirstTradePrice","OpenBidPrice","OpenAskPrice")) %>%
  filter(month(BarDateTime) == 6)

diff_fig <- plot_ly(quote_test, type = 'scatter', mode = 'lines')%>%
  #add_trace(x = ~quote_test$BarDateTime, y = ~quote_test$OpenBidPrice, name = 'GOOG')%>%
  add_trace(x = ~BarDateTime, y = ~FirstTradePrice, name = 'FirstTradePrice')%>%
  add_trace(x = ~BarDateTime, y = ~OpenBidPrice, name = 'OpenBidPrice')%>%
  add_trace(x = ~BarDateTime, y = ~OpenAskPrice, name = 'OpenAskPrice')%>%
  layout(showlegend = F,xaxis = list(rangebreaks=
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

###############################################
fig <- plot_ly(aapl_market, type = 'bar')%>%
  #add_trace(x = ~quote_test$BarDateTime, y = ~quote_test$OpenBidPrice, name = 'GOOG')%>%
  add_trace(x = ~BarDateTime, y = ~Volume, name = 'Volume')%>%
  add_trace(x = ~BarDateTime, y = ~FinraVolume, name = 'FinraVolume')%>%
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

fig <- fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)


fig
orca(fig, "volume_bar_chart.svg")
#####################


###############################################
# Factors Dataset                             #
###############################################

datatable(head(df))

# Corrplot
M = cor(na.omit(df[,-1]))
corrplot(M, method = 'shade') # colorful number

# Returns

ret_fig <- plot_ly(df, type = 'scatter', mode = 'lines')%>%
  #add_trace(x = ~quote_test$BarDateTime, y = ~quote_test$OpenBidPrice, name = 'GOOG')%>%
  add_trace(x = ~DT, y = ~ret1min, name = 'ret1min')%>%
  add_trace(x = ~DT, y = ~ret5min, name = 'ret5min')%>%
  add_trace(x = ~DT, y = ~ret10min, name = 'ret10min')%>%
  add_trace(x = ~DT, y = ~ret15min, name = 'ret15min')%>%
  add_trace(x = ~DT, y = ~ret20min, name = 'ret20min')%>%
  add_trace(x= ~DT, y=~ret25min, name='ret25min') %>%
  add_trace(x = ~DT, y = ~ret30min, name = 'ret30min')%>%
  add_trace(x = ~DT, y = ~ret60min, name = 'ret60min')%>%
  add_trace(x = ~DT, y = ~ret120min, name = 'ret120min')%>%
  add_trace(x = ~DT, y = ~ret180min, name = 'ret180min')%>%
  layout(title="AAPL returns",showlegend = T,xaxis = list(rangebreaks=
                                                            list(
                                                              list(bounds=list(16, 9),
                                                                   pattern="hour"),
                                                              list(bounds=list('sat', 'mon'),
                                                                   pattern='day of week')
                                                            ),#hide hours outside of 9am-5pm
                                                          dtick=86400000.0/2,
                                                          tickformat="%H:%M\n%b\n%Y"))

options(warn = -1)

ret_fig <- ret_fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 1,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 1,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width=1200)


ret_fig

# Volatility
vol_fig <- plot_ly(df, type = 'scatter', mode = 'lines')%>%
  #add_trace(x = ~quote_test$BarDateTime, y = ~quote_test$OpenBidPrice, name = 'GOOG')%>%
  add_trace(x = ~DT, y = ~vol5min, name = 'vol1min')%>%
  add_trace(x = ~DT, y = ~vol10min, name = 'vol5min')%>%
  add_trace(x = ~DT, y = ~vol30min, name = 'vol30min')%>%
  add_trace(x = ~DT, y = ~vol60min, name = 'vol60min')%>%
  add_trace(x = ~DT, y = ~vol120min, name = 'vol120min')%>%
  add_trace(x = ~DT, y = ~vol180min, name = 'vol180min')%>%
  layout(title="AAPL Volatility",showlegend = T,xaxis = list(rangebreaks=
                                                            list(
                                                              list(bounds=list(16, 9),
                                                                   pattern="hour"),
                                                              list(bounds=list('sat', 'mon'),
                                                                   pattern='day of week')
                                                            ),#hide hours outside of 9am-5pm
                                                          dtick=86400000.0/2,
                                                          tickformat="%H:%M\n%b\n%Y"))

options(warn = -1)

vol_fig <- vol_fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 1,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 1,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width=1200)


vol_fig
