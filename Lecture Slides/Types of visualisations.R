library(tidyverse)
library(tidytext)
library(textdata)
library(wordcloud)
library(wordcloud2)
library(ISLR)
library(plot3D)

## Barchart

arrest<-gather(USArrests, "Crime", "Value", c(1,2,4)) %>% group_by(Crime) %>% 
  summarise(Total=sum(Value))

ggplot(arrest,aes(x=Crime,y=Total))+geom_bar(stat="identity",fill="red")+
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size = 14))

## Histogram

##  Simulate data:
set.seed(2)
x <- rnorm(1000)
y <- rnorm(1000)

##  Create cuts:
x_c <- cut(x, 20)
y_c <- cut(y, 20)

##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)

##  Plot as a 3D histogram:
hist3D(z=z, border="black")


## Scatterplot

## Line graph

## Heatmap

image2D(z=z, border="black")

## Boxplots

## Word cloud

wordcloud_good_data<-read.delim("./Lecture Slides/book_1.txt",header = FALSE)

wordcloud_good_data <- wordcloud_good_data %>%
  select(V1) %>% 
  unnest_tokens(word, V1)

wordcloud_good_data %>%
  count(word, sort = TRUE) %>%
  reshape2::acast(word, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("blue", "red","gold","green","purple","hotpink","seagreen"),
                   max.words = 100)


wordcloud_bad_data

## Network

## Map

## TreeMap

## Radar

## Table