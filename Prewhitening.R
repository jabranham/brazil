#Prewhitening

library(forecast)
source("run-first.R")
preW.R <- auto.arima(ts(w.list$Proportion.Rousseff), D=0, seasonal=F, ic="bic")
res.R <- residuals(preW.R)
preW.N <- auto.arima(ts(w.list$Proportion.Neves), D=0, seasonal=F, ic="bic")
res.N <- residuals(preW.N)
preW.psb <- auto.arima(ts(w.list$Proportion.PSB), D=0, seasonal=F, ic="bic")
res.psb <- residuals(preW.psb)
preW.dk <- auto.arima(ts(w.list$Proportion.DK), D=0, seasonal=F, ic="bic")
res.dk <- residuals(preW.dk)
preW.O <- auto.arima(ts(w.list$Proportion.Other), D=0, seasonal=F, ic="bic")
res.O <- residuals(preW.O)