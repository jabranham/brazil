#Run me first!
#J. Alexander Branham && Brendan Apfeld

##Import data from 1st round, list question
round1list <- read.csv("data/Round1ListQ.csv")
round1list$Date <- as.POSIXct(as.character(round1list$Date),
                              format = "%m/%d/%Y")
round1list$elect.day <- as.character("10/05/2014")
round1list$elect.day <- as.POSIXct(round1list$elect.day,
                                   format = "%m/%d/%Y")
round1list$days.to.elect <- round(round1list$elect.day - round1list$Date)

#Sort by days to election
round1list <- round1list[order(round1list$days.to.elect), ]

##Import data from 1st round, recall question
round1recall <- read.csv("data/Round1RecallQ.csv")
round1recall$Date <- as.POSIXct(as.character(round1recall$Date),
                                format="%m/%d/%Y")
round1recall$elect.day <- as.character("10/05/2014")
round1recall$elect.day <- as.POSIXct(round1recall$elect.day,
                                     format = "%m/%d/%Y")
round1recall$days.to.elect <- round(round1recall$elect.day - round1recall$Date)

#Sort by days to election
round1recall <- round1recall[order(round1recall$days.to.elect),]

##Import data from 2nd round, Rouseff-Neves Scenario
round2RN <- read.csv("data/Round2RN.csv")
round2RN$Date <- as.POSIXct(as.character(round2RN$Date),
                            format="%m/%d/%Y")
round2RN$elect.day <- as.character("10/05/2014")
round2RN$elect.day <- as.POSIXct(round2RN$elect.day,
                                 format = "%m/%d/%Y")
round2RN$days.to.elect <- round(round2RN$elect.day - round2RN$Date)

#Sort by days to election
round2RN <- round2RN[order(round2RN$days.to.elect),]

##Import data from 2nd round, Rouseff-Silva Scenario
round2RS <- read.csv("data/Round2RS.csv")
round2RS$Date <- as.POSIXct(as.character(round2RS$Date),
                            format="%m/%d/%Y")
round2RS$elect.day <- as.character("10/05/2014")
round2RS$elect.day <- as.POSIXct(round2RS$elect.day,
                                 format = "%m/%d/%Y")
round2RS$days.to.elect <- round(round2RS$elect.day - round2RS$Date)

#Sort by days to election
round2RS <- round2RS[order(round2RS$days.to.elect),]

#Function to add election results to data frame
brazilelectresults <- function(dat){
    dat$r1results.Rousseff <- 41.59
    dat$r1results.Neves <- 33.55
    dat$r1results.Silva <- 21.32
    dat$r1results.Other <- 1.55 + .75 + .61 + .43 + .09 + .06 + .05 + .01
    dat$r1results.Blank <- 5.8 + 3.84
    dat$r2results.Rousseff <- 51.64
    dat$r2results.Neves <- 48.36
    dat$r2results.Blank <- 4.63 + 1.71
    return(dat)
}

## There are two polls on 1 daystoelect and 33 daystoelect - take the average
library(dplyr)
r1list <- round1list %>%
    group_by(days.to.elect) %>%
    summarise(House = paste(House, collapse = " + "),
              Proportion.Rousseff = mean(Proportion.Rousseff),
              Proportion.Neves = mean(Proportion.Neves),
              Proportion.Campos = mean(Proportion.Campos),
              Proportion.Silva = mean(Proportion.Silva),
              Proportion.Other = mean(Proportion.Other),
              Proportion.None = mean(Proportion.None),
              Proportion.DK = mean(Proportion.DK),
              cpi = mean(cpi),
              unemployment = mean(unemployment))

r1recall <- round1recall %>%
    group_by(days.to.elect) %>%
    summarise(House = paste(House, collapse = " + "),
              Proportion.Rousseff = mean(Proportion.Rousseff),
              Proportion.Neves = mean(Proportion.Neves),
              Proportion.Campos = mean(Proportion.Campos),
              Proportion.Silva = mean(Proportion.Silva),
              Proportion.Other = mean(Proportion.Other),
              Proportion.None = mean(Proportion.None),
              Proportion.DK = mean(Proportion.DK),
              cpi = mean(cpi),
              unemployment = mean(unemployment))

r2RN <- round2RN %>%
    group_by(days.to.elect) %>%
    summarise(House = paste(House, collapse = " + "),
              Proportion.Rousseff = mean(Proportion.Rousseff),
              Proportion.Neves = mean(Proportion.Neves),
              Proportion.None = mean(Proportion.None),
              Proportion.DK = mean(Proportion.DK),
              cpi = mean(cpi),
              unemployment = mean(unemployment))

r2RS <- round2RS %>%
  group_by(days.to.elect) %>%
    summarise(House = paste(House, collapse = " + "),
              Proportion.Rousseff = mean(Proportion.Rousseff),
              Proportion.Campos = mean(Proportion.Campos),
              Proportion.Silva = mean(Proportion.Silva),
              Proportion.None = mean(Proportion.None),
              Proportion.DK = mean(Proportion.DK),
              cpi = mean(cpi),
              unemployment = mean(unemployment))

## r1list.estimates object
days.to.elect <- seq(from = 1, to=235)
r1list.estimates <- data.frame(days.to.elect)
r1list.estimates <- merge(r1list.estimates, r1list,
                          by="days.to.elect", all = TRUE)

## r1recall.estimates object
r1recall.estimates <- data.frame(days.to.elect)
r1recall.estimates <- merge(r1recall.estimates, r1recall,
                            by="days.to.elect", all = TRUE)

## r2RN.estimates object
r2RN.estimates <- data.frame(days.to.elect)
r2RN.estimates <- merge(r2RN.estimates, r2RN,
                        by="days.to.elect", all = TRUE)

## r2RS.estimates object
r2RS.estimates <- data.frame(days.to.elect)
r2RS.estimates <- merge(r2RS.estimates, r2RS,
                        by="days.to.elect", all = TRUE)
rm(days.to.elect)

#Combine Silva and Campos into support for PSB
r1list.estimates$Proportion.PSB <- NA
r1list.estimates$Proportion.PSB[is.na(r1list.estimates$Proportion.Campos)== FALSE] <- r1list.estimates$Proportion.Campos[is.na(r1list.estimates$Proportion.Campos)== FALSE]
r1list.estimates$Proportion.PSB[is.na(r1list.estimates$Proportion.Campos)== TRUE] <- r1list.estimates$Proportion.Silva[is.na(r1list.estimates$Proportion.Campos)== TRUE]

r1recall.estimates$Proportion.PSB <- NA
r1recall.estimates$Proportion.PSB[is.na(r1recall.estimates$Proportion.Campos)== FALSE] <- r1recall.estimates$Proportion.Campos[is.na(r1recall.estimates$Proportion.Campos)== FALSE]
r1recall.estimates$Proportion.PSB[is.na(r1recall.estimates$Proportion.Campos)== TRUE] <- r1recall.estimates$Proportion.Silva[is.na(r1recall.estimates$Proportion.Campos)== TRUE]

r2RS.estimates$Proportion.PSB <- NA
r2RS.estimates$Proportion.PSB[is.na(r2RS.estimates$Proportion.Campos)== FALSE] <- r2RS.estimates$Proportion.Campos[is.na(r2RS.estimates$Proportion.Campos)== FALSE]
r2RS.estimates$Proportion.PSB[is.na(r2RS.estimates$Proportion.Campos)== TRUE] <- r2RS.estimates$Proportion.Silva[is.na(r2RS.estimates$Proportion.Campos)== TRUE]

#Now, create a Vhat for each day that doesn't already have data

vhatestimate <- function(dat) {
    set.seed(124)
    N <- length(dat)
    na.pos <- which(is.na(dat))
    if (length(na.pos) %in% c(0, N)){
        return(dat)
    }
    non.na.pos <- which(!is.na(dat))
    intervals <- findInterval(na.pos, non.na.pos, all.inside = TRUE)
    left.pos <- non.na.pos[pmax(1, intervals)]
    right.pos <- non.na.pos[pmin(N, intervals + 1)]
    left.dist <- na.pos - left.pos
    right.dist <- right.pos - na.pos
    sdev <- sd(dat, na.rm=TRUE)
    error <- rnorm(N, 0, sdev)
    dat[na.pos] <- (left.dist * dat[right.pos] + right.dist * dat[left.pos])/(left.dist + right.dist) + error[na.pos]
    return(dat)
}

varnames <- c("Proportion.Rousseff", "Proportion.Neves", "Proportion.PSB",
              "Proportion.Other", "Proportion.None", "Proportion.DK")

r1list.estimates[, varnames] <- apply(r1list.estimates[, varnames], 2, vhatestimate)
## r1list.estimates[, varnames] <- apply(r1recall.estimates[, varnames], 2, na.approx)
## r1list.estimates[, varnames] <- apply(r1recall.estimates[, varnames], 2, na.spline)
## r1list.estimates[, varnames] <- apply(r1recall.estimates[, varnames], 2, na.locf)

r1recall.estimates[, varnames] <- apply(r1recall.estimates[, varnames], 2, vhatestimate)


varnames <- c("Proportion.Rousseff", "Proportion.Neves", "Proportion.None", "Proportion.DK")
r2RN.estimates[, varnames] <- apply(r2RN.estimates[, varnames], 2, vhatestimate)

varnames <- c("Proportion.Rousseff", "Proportion.PSB", "Proportion.None", "Proportion.DK")
r2RS.estimates[, varnames] <- apply(r2RS.estimates[, varnames], 2, vhatestimate)

#Add campos living variable
r1list.estimates$campos.living <- ifelse(r1list.estimates$days.to.elect > 52, 1, 0)
r1recall.estimates$campos.living <- ifelse(r1recall.estimates$days.to.elect > 52, 1, 0)
r2RS.estimates$campos.living <- ifelse(r2RS.estimates$days.to.elect > 52, 1, 0)
r2RN.estimates$campos.living <- ifelse(r2RN.estimates$days.to.elect > 52, 1, 0)


#Drop day 337 observation
r1list.estimates <- subset(r1list.estimates, r1list.estimates$days.to.elect <=235)
r1recall.estimates <- subset(r1recall.estimates, r1recall.estimates$days.to.elect <=235)
r2RN.estimates <- subset(r2RN.estimates, r2RN.estimates$days.to.elect <=235)
r2RS.estimates <- subset(r2RS.estimates, r2RS.estimates$days.to.elect <=235)

#Put in a date variable
r1list.estimates$days.since.R.date <- 16349
r1list.estimates$date <- r1list.estimates$days.since.R.date - r1list.estimates$days.to.elect
r1list.estimates$date <- as.Date(r1list.estimates$date, origin = "1970-01-01")
r1recall.estimates$days.since.R.date <- 16349
r1recall.estimates$date <- r1recall.estimates$days.since.R.date - r1recall.estimates$days.to.elect
r1recall.estimates$date <- as.Date(r1recall.estimates$date, origin = "1970-01-01")
r2RN.estimates$days.since.R.date <- 16349
r2RN.estimates$date <- r2RN.estimates$days.since.R.date - r2RN.estimates$days.to.elect
r2RN.estimates$date <- as.Date(r2RN.estimates$date, origin = "1970-01-01")
r2RS.estimates$days.since.R.date <- 16349
r2RS.estimates$date <- r2RS.estimates$days.since.R.date - r2RS.estimates$days.to.elect
r2RS.estimates$date <- as.Date(r2RS.estimates$date, origin = "1970-01-01")


##Create cpi and unemployment
library(zoo)
# put in unemploy and cpi for r2RN first observation
r2RN.estimates$cpi[1] <- 0.43
r2RN.estimates$unemployment[1] <- 4.7
r1list.estimates$cpi <- na.approx(r1list.estimates$cpi)
r1list.estimates$unemployment <- na.approx(r1list.estimates$unemployment)
r1recall.estimates$cpi <- na.approx(r1recall.estimates$cpi)
r1recall.estimates$unemployment <- na.approx(r1recall.estimates$unemployment)
r2RN.estimates$cpi <- na.approx(r2RN.estimates$cpi)
r2RN.estimates$unemployment <- na.approx(r2RN.estimates$unemployment)
r2RS.estimates$cpi <- na.approx(r2RS.estimates$cpi)
r2RS.estimates$unemployment <- na.approx(r2RS.estimates$unemployment)


r1list.estimates <- r1list.estimates[order(r1list.estimates$date),]
r1list.estimates$campos.dead <- NA
r1list.estimates$campos.dead[r1list.estimates$campos.living == 0] <- 1
r1list.estimates$campos.dead[r1list.estimates$campos.living == 1] <- 0

##Aggregate to Weekly 
library(xts)
list <- r1list.estimates
z.list <- zoo(list, order.by=list$date)
weekly.list <- apply.weekly(z.list, mean, na.rm=TRUE)
tscampos <- ts(weekly.list$campos.dead )
lag.death <- data.frame(tscampos)
max.lag <- 7
for (i in 1:max.lag){
    lag.death[i] <- lag(tscampos, i)
}
names <- paste(rep("A", max.lag), seq(1:max.lag), sep="")
colnames(lag.death) <- names

weekly.list <- as.data.frame(weekly.list)
w.list <- cbind(weekly.list, lag.death)

r1recall.estimates <- r1recall.estimates[order(r1recall.estimates$date),]
r1recall.estimates$campos.dead <- NA
r1recall.estimates$campos.dead[r1recall.estimates$campos.living == 0] <- 1
r1recall.estimates$campos.dead[r1recall.estimates$campos.living == 1] <- 0
recall <- r1recall.estimates
z.recall <- zoo(recall, order.by=recall$date)
weekly.recall <- apply.weekly(z.recall, mean, na.rm=TRUE)
tscampos <- ts(weekly.recall$campos.dead )
lag.death <- data.frame(tscampos)
max.lag <- 7
for (i in 1:max.lag){
    lag.death[i] <- lag(tscampos, i)
}
names <- paste(rep("A", max.lag), seq(1:max.lag), sep="")
colnames(lag.death) <- names

weekly.recall <- as.data.frame(weekly.recall)
w.recall <- cbind(weekly.recall, lag.death)

w.list$week <- seq(from=33, to=0, by=-1)
w.recall$week <- seq(from=33, to=0, by=-1)

rm(lag.death, list, r1list, r1list.estimates, r1recall,
   r1recall.estimates, r2RN, r2RN.estimates, r2RS,
   r2RS.estimates, recall, round1list, round1recall,
   round2RN, round2RS, weekly.list, weekly.recall,
   i, max.lag, names, tscampos, z.list, z.recall)

w.recall$Proportion.PSB <- ifelse(w.recall$Proportion.PSB < 0,
                                  abs(w.recall$Proportion.PSB),
                                  w.recall$Proportion.PSB)
