library(tidyverse)

x<-rnorm(100000)

hist(x)

random_sample<-sample(100000,200, replace = FALSE)

NAs<-c(rep(NA,20))

x_MAR<-c(x[-random_sample],NAs)

hist(x_MAR)

x_disort<-order(x) %>% head(50000)

hist(x_disort)
