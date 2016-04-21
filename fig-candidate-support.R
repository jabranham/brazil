## after running Run Me First and Determing optimal lag... (to aggregate to weekly)
#This script will produce a plot with candidate's estimated support by week across the electoral cycle
rm(list=ls())
source("run-first.R")

w.list$week <- seq(from=33, to=0, by = -1)
w.recall$week <- seq(from=33, to=0, by=-1)
library(ggplot2)

ggplot(data=w.list, aes(week)) +
  scale_x_reverse() +
  scale_linetype_discrete(breaks=c("PT", "PSB", "PSDB"), name="Party") +
  labs(x = "Weeks Before Election", y = "Percent Support") +
  geom_line(aes(y=Proportion.Rousseff, linetype="PT")) +
  geom_line(aes(y=Proportion.PSB, linetype="PSB")) +
  geom_line(aes(y=Proportion.Neves, linetype="PSDB")) +
  geom_vline(xintercept=7.42) +
  theme_minimal()

ggsave("Figure_Support_Candidate_List.eps", scale=0.65)

ggplot(data=w.recall, aes(week)) +
  scale_x_reverse() +
  scale_linetype_discrete(breaks=c("PT", "PSB", "PSDB"), name="Party") +
  labs(x = "Weeks Before Election", y = "Percent Support") +
  geom_line(aes(y=Proportion.Rousseff, linetype="PT")) +
  geom_line(aes(y=Proportion.PSB, linetype="PSB")) +
  geom_line(aes(y=Proportion.Neves, linetype="PSDB")) +
  geom_vline(xintercept=7.42) +
  theme_minimal()

ggsave("Figure_Candidate_Support_Recall.eps", scale = 0.65)
