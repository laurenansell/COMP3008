## Read in the required libraries
library(tidyverse)

## Read in the data
twitter_data<-read.csv("../endgame_tweets.csv")
## The dataset is from Kaggle and can be downloaded from here: 
## https://www.kaggle.com/datasets/kavita5/twitter-dataset-avengersendgame

## Look at the variable names
names(twitter_data)

## Look at the structure of the data
str(twitter_data)

