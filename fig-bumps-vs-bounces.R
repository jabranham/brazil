##This will create a graph for bounces and bumps
library(ggplot2)

bounce <- function(x){
  ifelse(x < 9.9, -(x-4)^2 +2*x + 15, 0)
}

bump <- function(x){
  ifelse(x < 5, -(x-4)^2 +2*x + 14.75, 23.75)
}

ggplot(data.frame(Time = c(0,14)), aes(x=Time)) + 
  stat_function(fun=bump, aes(linetype="Bump")) + 
  stat_function(fun=bounce, aes(linetype="Bounce")) + 
  labs(y="Cumulative Change in Support", x="Time") +
  scale_linetype_discrete(breaks=c("Bounce", "Bump"),
                          name="") + 
  theme_minimal()

ggsave("Figure_bumps_bounces.eps", scale = 0.65)
