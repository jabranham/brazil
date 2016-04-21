### Need to run "Run me first" and "determing optimal lag..." first
source("run-first.R")
##This script checks each time series for stationarity

library(urca)

summary(ur.za(w.list$Proportion.Rousseff))

## ACFs
acf(w.list$Proportion.Rousseff)
acf(w.list$Proportion.Neves)
acf(w.list$Proportion.PSB)
acf(w.list$Proportion.DK)
acf(w.list$Proportion.Other)

#kpss test
summary(ur.kpss(w.list$Proportion.Rousseff, type = "mu", lags="long"))
summary(ur.kpss(w.list$Proportion.Neves, type = "mu", lags="long"))
summary(ur.kpss(w.list$Proportion.PSB, type = "mu", lags="long"))
summary(ur.kpss(w.list$Proportion.DK, type = "mu", lags="long"))
summary(ur.kpss(w.list$Proportion.Other, type = "mu", lags="long"))

summary(ur.kpss(w.recall$Proportion.Rousseff, type = "mu", lags="long"))
summary(ur.kpss(w.recall$Proportion.Neves, type = "mu", lags="long"))
summary(ur.kpss(w.recall$Proportion.PSB, type = "mu", lags="long"))
summary(ur.kpss(w.recall$Proportion.DK, type = "mu", lags="long"))
summary(ur.kpss(w.recall$Proportion.Other, type = "mu", lags="long"))

#Schwert value
schwert <- function (x) {floor(12*((length(x)/100))^.25)}
schwert.list.Rousseff <- schwert(w.list$Proportion.Rousseff)
schwert.list.Neves <- schwert(w.list$Proportion.Neves)
schwert.list.PSB <- schwert(w.list$Proportion.PSB)
schwert.list.DK <- schwert(w.list$Proportion.DK)
schwert.list.Other <- schwert(w.list$Proportion.Other)
schwert.recall.Rousseff <- schwert(w.recall$Proportion.Rousseff)
schwert.recall.Neves <- schwert(w.recall$Proportion.Neves)
schwert.recall.PSB <- schwert(w.recall$Proportion.PSB)
schwert.recall.DK <- schwert(w.recall$Proportion.DK)
schwert.recall.Other <- schwert(w.recall$Proportion.Other)


#DF test
for (i in 1:9) {
  print(ur.df(w.list$Proportion.Rousseff, type=c("none"), lags=i) )
}

#alternative package DF test
library(tseries)
for (i in 1:9) {
  print(adf.test(w.list$Proportion.Rousseff, k=i))
}
for (i in 1:9) {
  print(adf.test(w.list$Proportion.Neves, k=i))
}
for (i in 1:9) {
  print(adf.test(w.list$Proportion.PSB, k=i))
}
adf.test(w.list$Proportion.Rousseff, k=9)


#ERS test
summary(ur.ers(w.list$Proportion.Rousseff, type=c("DF-GLS"), model=c("constant"), lag.max=9))
summary(ur.ers(w.list$Proportion.Neves, type=c("DF-GLS"), model=c("constant"), lag.max=9))
summary(ur.ers(w.list$Proportion.PSB, type=c("DF-GLS"), model=c("constant"), lag.max=9))
summary(ur.ers(w.list$Proportion.DK, type=c("DF-GLS"), model=c("constant"), lag.max=9))
summary(ur.ers(w.list$Proportion.Other, type=c("DF-GLS"), model=c("constant"), lag.max=9))

#Phillips-Perron Test
PP.test(w.list$Proportion.Rousseff, lshort=F)

