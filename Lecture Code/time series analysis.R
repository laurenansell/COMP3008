library(tidyverse)
library(fpp2)

## Moving Average

oildata<-window(oil,start=1996)

autoplot(oildata)+ylab("Oil (millions of tonnes)")+xlab("Year")

ma(oildata,5)

autoplot(oildata,series = "Data")+
  autolayer(ma(oildata,3),series = "5-MA")

## ARIMA

ggtsdisplay(oil)

Box.test(goog200,lag = 10,type = "Ljung-Box")


Box.test(diff(goog200),lag = 10,type = "Ljung-Box")

ARIMA_fit<-auto.arima(uschange[,"Consumption"])

ARIMA_fit

ARIMA_fit %>% forecast(h=20) %>% autoplot(include=80)

ses_model<-ses(oildata,h=3)
ses_model

round(accuracy(ses_model),2)

autoplot(ses_model)+autolayer(fitted(ses_model))

checkresiduals(ARIMA_fit)
