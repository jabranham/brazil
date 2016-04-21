## Finding optimal lag length
rm(list=ls())
source("run-first.R")

min.AIC <- 999999999999999
min.BIC <- 999999999999999

## Rousseff optimal lag find
for (i in 0:max.lag) {
  formula <- as.formula(paste("Proportion.Rousseff ~ campos.dead +", paste(names[1:i], collapse = " + ")))
  model <- lm(formula, data=subset(w.list, is.na(w.list$A7)==FALSE ))
  model.AIC <- AIC(model)
  model.BIC <- BIC(model)
  if (model.AIC < min.AIC){
    min.AIC <- model.AIC
    AIC.optimal.lag <- i
  }
  if (model.BIC < min.BIC) {
    min.BIC <- model.BIC
    BIC.optimal.lag <- i
  }
}
AIC.optimal.lag #6
BIC.optimal.lag #5

##Neves optimal lag find
min.AIC <- 999999999999999
min.BIC <- 999999999999999
for (i in 0:max.lag) {
  formula <- as.formula(paste("Proportion.Neves ~ campos.dead + ", paste(names[1:i], collapse = " + ")))
  model <- lm(formula, data=subset(w.list, is.na(w.list$A7)==FALSE ))
  print(summary(model))
  model.AIC <- AIC(model)
  model.BIC <- BIC(model)
  if (model.AIC < min.AIC){
    min.AIC <- model.AIC
    AIC.optimal.lag <- i
  }
  if (model.BIC < min.BIC) {
    min.BIC <- model.BIC
    BIC.optimal.lag <- i
  }
}
BIC.optimal.lag #0
AIC.optimal.lag #0

##PSB optimal lag find
min.AIC <- 999999999999999
min.BIC <- 999999999999999
for (i in 0:max.lag) {
  formula <- as.formula(paste("Proportion.PSB ~ campos.dead + ", paste(names[1:i], collapse = " + ")))
  model <- lm(formula, data=subset(w.list, is.na(w.list$A7)==FALSE ))
  model.AIC <- AIC(model)
  model.BIC <- BIC(model)
  if (model.AIC < min.AIC){
    min.AIC <- model.AIC
    AIC.optimal.lag <- i
  }
  if (model.BIC < min.BIC) {
    min.BIC <- model.BIC
    BIC.optimal.lag <- i
  }
}
AIC.optimal.lag #4
BIC.optimal.lag #0

##Other optimal lag find
min.AIC <- 999999999999999
min.BIC <- 999999999999999
for (i in 0:max.lag) {
  formula <- as.formula(paste("Proportion.Other ~ campos.dead + ", paste(names[1:i], collapse = " + ")))
  model <- lm(formula, data=subset(w.list, is.na(w.list$A7)==FALSE ))
  model.AIC <- AIC(model)
  model.BIC <- BIC(model)
  if (model.AIC < min.AIC){
    min.AIC <- model.AIC
    AIC.optimal.lag <- i
  }
  if (model.BIC < min.BIC) {
    min.BIC <- model.BIC
    BIC.optimal.lag <- i
  }
}
AIC.optimal.lag #0
BIC.optimal.lag #0

#DK optimal lag find
min.AIC <- 999999999999999
min.BIC <- 999999999999999
for (i in 0:max.lag) {
  formula <- as.formula(paste("Proportion.DK ~ campos.dead + ", paste(names[1:i], collapse = " + ")))
  model <- lm(formula, data=subset(w.list, is.na(w.list$A7)==FALSE ))
  model.AIC <- AIC(model)
  model.BIC <- BIC(model)
  if (model.AIC < min.AIC){
    min.AIC <- model.AIC
    AIC.optimal.lag <- i
  }
  if (model.BIC < min.BIC) {
    min.BIC <- model.BIC
    BIC.optimal.lag <- i
  }
}
AIC.optimal.lag #0
BIC.optimal.lag #0

#None optimal lag find
min.AIC <- 999999999999999
min.BIC <- 999999999999999
for (i in 0:max.lag) {
  formula <- as.formula(paste("Proportion.None ~ campos.dead + ", paste(names[1:i], collapse = " + ")))
  model <- lm(formula, data=subset(w.list, is.na(w.list$A7)==FALSE ))
  model.AIC <- AIC(model)
  model.BIC <- BIC(model)
  if (model.AIC < min.AIC){
    min.AIC <- model.AIC
    AIC.optimal.lag <- i
  }
  if (model.BIC < min.BIC) {
    min.BIC <- model.BIC
    BIC.optimal.lag <- i
  }
}
AIC.optimal.lag #0
BIC.optimal.lag #0

##Recall question


##Rousseff optimal lag find
min.AIC <- 999999999999999
min.BIC <- 999999999999999
for (i in 0:max.lag) {
  formula <- as.formula(paste("Proportion.Rousseff ~ campos.dead + ", paste(names[1:i], collapse = " + ")))
  model <- lm(formula, data=subset(w.recall, is.na(w.list$A7)==FALSE ))
  model.AIC <- AIC(model)
  model.BIC <- BIC(model)
  if (model.AIC < min.AIC){
    min.AIC <- model.AIC
    AIC.optimal.lag <- i
  }
  if (model.BIC < min.BIC) {
    min.BIC <- model.BIC
    BIC.optimal.lag <- i
  }
}
AIC.optimal.lag #3
BIC.optimal.lag #3

##Neves optimal lag find
min.AIC <- 999999999999999
min.BIC <- 999999999999999
for (i in 0:max.lag) {
  formula <- as.formula(paste("Proportion.Neves ~ campos.dead + ", paste(names[1:i], collapse = " + ")))
  model <- lm(formula, data=subset(w.recall, is.na(w.list$A7)==FALSE ))
  model.AIC <- AIC(model)
  model.BIC <- BIC(model)
  if (model.AIC < min.AIC){
    min.AIC <- model.AIC
    AIC.optimal.lag <- i
  }
  if (model.BIC < min.BIC) {
    min.BIC <- model.BIC
    BIC.optimal.lag <- i
  }
}
AIC.optimal.lag #3
BIC.optimal.lag #0

##PSB optimal lag find
min.AIC <- 999999999999999
min.BIC <- 999999999999999
for (i in 0:max.lag) {
  formula <- as.formula(paste("Proportion.PSB ~ campos.dead + ", paste(names[1:i], collapse = " + ")))
  model <- lm(formula, data=subset(w.recall, is.na(w.list$A7)==FALSE ))
  model.AIC <- AIC(model)
  model.BIC <- BIC(model)
  if (model.AIC < min.AIC){
    min.AIC <- model.AIC
    AIC.optimal.lag <- i
  }
  if (model.BIC < min.BIC) {
    min.BIC <- model.BIC
    BIC.optimal.lag <- i
  }
}
AIC.optimal.lag #0
BIC.optimal.lag #0

##Other optimal lag find
min.AIC <- 999999999999999
min.BIC <- 999999999999999
for (i in 0:max.lag) {
  formula <- as.formula(paste("Proportion.Other ~ campos.dead + ", paste(names[1:i], collapse = " + ")))
  model <- lm(formula, data=subset(w.recall, is.na(w.list$A7)==FALSE ))
  model.AIC <- AIC(model)
  model.BIC <- BIC(model)
  if (model.AIC < min.AIC){
    min.AIC <- model.AIC
    AIC.optimal.lag <- i
  }
  if (model.BIC < min.BIC) {
    min.BIC <- model.BIC
    BIC.optimal.lag <- i
  }
}
AIC.optimal.lag #0
BIC.optimal.lag #0

##DK optimal lag find
min.AIC <- 999999999999999
min.BIC <- 999999999999999
for (i in 0:max.lag) {
  formula <- as.formula(paste("Proportion.DK ~ campos.dead + ", paste(names[1:i], collapse = " + ")))
  model <- lm(formula, data=subset(w.recall, is.na(w.list$A7)==FALSE ))
  model.AIC <- AIC(model)
  model.BIC <- BIC(model)
  if (model.AIC < min.AIC){
    min.AIC <- model.AIC
    AIC.optimal.lag <- i
  }
  if (model.BIC < min.BIC) {
    min.BIC <- model.BIC
    BIC.optimal.lag <- i
  }
}
AIC.optimal.lag #0
BIC.optimal.lag #0

##Other optimal lag find
min.AIC <- 999999999999999
min.BIC <- 999999999999999
for (i in 0:max.lag) {
  formula <- as.formula(paste("Proportion.Other ~ campos.dead + ", paste(names[1:i], collapse = " + ")))
  model <- lm(formula, data=subset(w.recall, is.na(w.list$A7)==FALSE ))
  model.AIC <- AIC(model)
  model.BIC <- BIC(model)
  if (model.AIC < min.AIC){
    min.AIC <- model.AIC
    AIC.optimal.lag <- i
  }
  if (model.BIC < min.BIC) {
    min.BIC <- model.BIC
    BIC.optimal.lag <- i
  }
}
AIC.optimal.lag #0
BIC.optimal.lag #0


