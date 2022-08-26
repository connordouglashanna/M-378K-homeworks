library(tidyverse)
library(ggplot2)

#1.4 & 1.15 & 1.17

stonks <- 
  c(11.88, 7.99, 7.15, 7.13, 6.27, 6.07, 5.98, 5.91, 5.49, 5.26, 5.07, 4.94,4.81, 4.79,
    4.55, 4.43, 4.40, 4.05, 3.94, 3.93, 3.78, 3.69, 3.62, 3.48, 3.44, 3.36, 3.26, 3.20,
    3.11, 3.03, 2.99, 2.89, 2.88, 2.74, 2.74, 2.69, 2.68, 2.63, 2.62, 2.61)

#1,15 a

mstonk <- mean(stonks)

vstonk <- var(stonks)

sdstonk <- sd(stonks)

#1.15 b

kstonk <-
  function(x){
    lower <- (mstonk - x * sdstonk)
    upper <- (mstonk + x * sdstonk)
    c(upper, lower)
  }

kstonk(1)
kstonk(2)
kstonk(3)

nstonk <- 
  function(x){
    lower <- I(mstonk - x * sdstonk)
    upper <- I(mstonk + x * sdstonk)
    stonks_df <- data.frame(stonks)
    vals <- filter(stonks_df, stonks > lower, stonks < upper)
    count(vals)
  }

nstonk(1)
nstonk(2)
nstonk(3)

#1.17

range <- max(stonks) - min(stonks)
appx <- range/4
appx

#3.115

dhyper(0:2, 2, 6, 3)

#3.116

sim <- data.frame(sim = rhyper(100, 2, 6, 3)) 

sim %>%
ggplot(aes(sim)) + 
  geom_histogram(bins = 3)

#3.190

calc <- data.frame(prob = dhyper(0:2, 2, 6, 3), y = 0:2)
calc_mean <- sum(calc$prob * calc$y)
calc_mean

exp_mean <- mean(sim$sim)
exp_mean

#3.191

p <- .25
calc_var <- 3*p*(1-p)*(5/7)
#the above are formulas from the dhyper documentation
calc_var

exp_var <- var(sim$sim)
exp_var

#4.18

integrate()
