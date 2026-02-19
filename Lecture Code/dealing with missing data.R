library(tidyverse)

covid_data<-read.csv("covid_wave1.csv")

table(covid_data)

table(covid_data$Drinking_frequency)


table(covid_data$Number_of_drinks)

## The replace_na function will just swap out the NA for your desired value.

covid_data$Drinking_frequency<-replace_na(covid_data$Drinking_frequency,1)

covid_data$Number_of_drinks<-replace_na(covid_data$Number_of_drinks,5)
