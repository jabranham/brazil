## This script produces the individual and cumulative effect graphs 
## Unnecessary parts have been commented out - run all
rm(list=ls())
source("run-first.R")
## uncomment these lines to produce results from prewhitened data
# source("Prewhitening.R")
# w.list$Proportion.Rousseff <- res.R
# w.list$Proportion.Neves <- res.N
# w.list$Proportion.PSB <- res.psb
# w.list$Proportion.DK <- res.dk
# w.list$Proportion.Other <- res.O

library(dlnm)
library(ggplot2)

cb.campos <- crossbasis(w.list$campos.dead, lag=7, argvar=list("lin"), arglag=list(fun="poly", degree=3))
model1 <- glm(w.list$Proportion.PSB ~ cb.campos + w.list$cpi + w.list$unemployment)
pred.campos <- crosspred(cb.campos, model1, at=1, lag = 7, from=0, to=7, bylag=.1, cumul=TRUE)
campos.fit <- NA
campos.fit.se <- NA
campos.cumfit <- NA
campos.cumfit.se <- NA
campos.fit <- as.vector(pred.campos$matfit[1,])
campos.fit.se <- as.vector(pred.campos$matse[1,])
campos.cumfit <- as.vector(pred.campos$cumfit[1,])
campos.cumfit.se <- as.vector(pred.campos$cumse[1])

cb.rousseff <- crossbasis(w.list$campos.dead, lag=7, argvar=list("lin"), arglag=list(fun="poly", degree=3))
model2 <- glm(w.list$Proportion.Rousseff ~ cb.rousseff + w.list$cpi + w.list$unemployment)
pred.rousseff <- crosspred(cb.rousseff, model2, at=1, lag = 7, from = 0, to = 7, bylag=0.1, cumul=TRUE)
rousseff.fit <- NA
rousseff.fit.se <- NA
rousseff.cumfit <- NA
rousseff.cumfit.se <- NA
rousseff.fit <- as.vector(pred.rousseff$matfit[1,])
rousseff.fit.se <- as.vector(pred.rousseff$matse[1,])
rousseff.cumfit <- as.vector(pred.rousseff$cumfit[1,])
rousseff.cumfit.se <- as.vector(pred.rousseff$cumse[1])

cb.neves <- crossbasis(w.list$campos.dead, lag=7, argvar=list("lin"), arglag=list(fun="poly", degree=3))
model3 <- glm(w.list$Proportion.Neves ~ cb.neves + w.list$cpi + w.list$unemployment)
pred.neves <- crosspred(cb.neves, model3, at=1, lag = 7, from = 0, to = 7, bylag=0.1, cumul=TRUE)
neves.fit <- NA
neves.fit.se <- NA
neves.cumfit <- NA
neves.cumfit.se <- NA
neves.fit <- as.vector(pred.neves$matfit[1,])
neves.fit.se <- as.vector(pred.neves$matse[1,])
neves.cumfit <- as.vector(pred.neves$cumfit[1,])
neves.cumfit.se <- as.vector(pred.neves$cumse[1])


# cb.campos.recall <- crossbasis(w.recall$campos.dead, lag=7, argvar=list("lin"), arglag=list(fun="poly", degree=3))
# model4 <- glm(w.recall$Proportion.PSB ~ cb.campos.recall + w.recall$cpi + w.recall$unemployment)
# pred.campos.recall <- crosspred(cb.campos.recall, model4, at=1, lag = 7, from = 0, to = 7, bylag=0.1, cumul=TRUE)
# campos.recall.values <- NA
# campos.recall.values <- as.vector(pred.campos.recall$cumfit[1,])
# 
# cb.rousseff.recall <- crossbasis(w.recall$campos.dead, lag=7, argvar=list("lin"), arglag=list(fun="poly", degree=3))
# model5 <- glm(w.recall$Proportion.Rousseff ~ cb.rousseff.recall + w.recall$cpi + w.recall$unemployment)
# pred.rousseff.recall <- crosspred(cb.rousseff.recall, model5, at=1, lag = 7, from = 0, to = 7, bylag=0.1, cumul=TRUE)
# rousseff.recall.values <- NA
# rousseff.recall.values <- as.vector(pred.rousseff.recall$cumfit[1,])
# 
# cb.neves.recall <- crossbasis(w.recall$campos.dead, lag=7, argvar=list("lin"), arglag=list(fun="poly", degree=3))
# model6 <- glm(w.recall$Proportion.Neves ~ cb.neves.recall + w.recall$cpi + w.recall$unemployment)
# pred.neves.recall <- crosspred(cb.neves.recall, model6, at=1, lag = 7, from = 0, to = 7, bylag=0.1, cumul=TRUE)
# neves.recall.values <- NA
# neves.recall.values <- as.vector(pred.neves.recall$cumfit[1,])

#time.vector <- seq(0, 7, 0.1)
time.vector.fit <- c(0:70)
time.vector.cumfit <- c(0:7)
plots.fit <- cbind(time.vector.fit, campos.fit, campos.fit.se,
                   rousseff.fit, rousseff.fit.se, neves.fit, neves.fit.se)
plots.fit <- as.data.frame(plots.fit)
plots.cumfit <- cbind(time.vector.cumfit, campos.cumfit,
                      campos.cumfit.se, rousseff.cumfit,
                      rousseff.cumfit.se, neves.cumfit,
                      neves.cumfit.se)
plots.cumfit <- as.data.frame(plots.cumfit)

plots.fit$weeks <- seq(from=70, to=0, by=-1)
plots.cumfit$weeks <- seq(from=7, to=0, by=-1)

#Plot of effect by candidate
#campos
# ggplot(data=plots.fit, aes(x=weeks/10)) +
#   theme(legend.position="none") +
#   scale_x_reverse(name="") +
#   scale_y_continuous(name="")+
#   geom_line(aes(y=campos.fit)) +
#   geom_ribbon(aes(ymin=campos.fit-campos.fit.se, ymax=campos.fit+campos.fit.se), alpha=.3)
# #rousseff
# ggplot(data=plots.fit, aes(x=weeks/10)) +
#   theme(legend.position="none") +
#   scale_x_reverse(name="") +
#   scale_y_continuous(name="")+
#   geom_line(aes(y=rousseff.fit)) +
#   geom_ribbon(aes(ymin=rousseff.fit-rousseff.fit.se, ymax=rousseff.fit+rousseff.fit.se), alpha=.3)
# #neves
# ggplot(data=plots.fit, aes(x=weeks/10)) +
#   theme(legend.position="none") +
#   scale_x_reverse(name="") +
#   scale_y_continuous(name="")+
#   geom_line(aes(y=neves.fit)) +
#   geom_ribbon(aes(ymin=neves.fit-campos.fit.se, ymax=neves.fit+neves.fit.se), alpha=.3)
# 
# #Plot of cumulative effect by candidate
# #campos
ggplot(data=plots.cumfit, aes(x=weeks)) +
  theme(legend.position="none") +
  scale_x_reverse(name="") +
  scale_y_continuous(name="")+
  geom_line(aes(y=campos.cumfit)) +
  geom_ribbon(aes(ymin=campos.cumfit-2 * campos.cumfit.se,
                  ymax=campos.cumfit + 2 * campos.cumfit.se), alpha=.3)

# #rousseff
ggplot(data=plots.cumfit, aes(x=weeks)) +
  theme(legend.position="none") +
  scale_x_reverse(name="") +
  scale_y_continuous(name="")+
  geom_line(aes(y=rousseff.cumfit)) +
  geom_ribbon(aes(ymin=rousseff.cumfit - 2 * rousseff.cumfit.se,
                  ymax=rousseff.cumfit + 2 * rousseff.cumfit.se), alpha=.3)

# #neves
ggplot(data=plots.cumfit, aes(x=weeks)) +
  theme(legend.position="none") +
  scale_x_reverse(name="") +
  scale_y_continuous(name="")+
  geom_line(aes(y=neves.cumfit)) +
  geom_ribbon(aes(ymin=neves.cumfit - 2 * neves.cumfit.se,
                  ymax=neves.cumfit + 2 * neves.cumfit.se), alpha=.3)




#Combined plots cumulative effect
ggplot(data=plots.cumfit, aes(x=weeks)) +
  scale_x_reverse(name="Weeks before election")+
  scale_y_continuous(name="Change in Support") +
  scale_linetype(name="Party") +
  geom_line(aes(y=campos.cumfit, linetype="PSB")) + 
  geom_line(aes(y=rousseff.cumfit, linetype="PT")) + 
  geom_line(aes(y=neves.cumfit, linetype="PSDB")) +
  theme_minimal()

ggsave("c-effect.eps", scale = 0.65)

#Combined plots effect
ggplot(data=plots.fit, aes(x=weeks/10)) +
  scale_linetype(name="Party") +
  scale_x_reverse(name="Weeks before election") +
  scale_y_continuous(name="Change in Support")+
  geom_line(aes(y=campos.fit, linetype="PSB")) +
  geom_line(aes(y=rousseff.fit, linetype="PT")) +
  geom_line(aes(y=neves.fit, linetype="PSDB")) +
  theme_minimal()

ggsave("effect.eps", scale = 0.65)
