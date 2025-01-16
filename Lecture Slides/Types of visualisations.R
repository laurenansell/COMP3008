library(tidyverse)
library(tidytext)
library(textdata)

## Barchart


## Histogram

## Scatterplot

## Line grapg

## Heatmap

## Boxplots

## Word cloud

wordcloud_good_data<-read.delim("./Lecture Slides/book_1.txt",header = FALSE)

wordcloud_good_data <- wordcloud_good_data %>%
  select(V1) %>% 
  unnest_tokens(word, V1)

wordcloud_bad_data

## Network

## Map

## TreeMap

## Radar

## Table