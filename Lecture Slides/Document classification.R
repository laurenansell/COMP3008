######## Code used in the slides for the document classification lecture

library(tidyverse)
library(ggthemes)
library(tidytext)
library(textdata)
library(igraph)
library(ggraph)
library(tm) # text mining
library(caret) # classification
library(keras) # deep learning
library(tensorflow) # deep learning
library(rnn) ## recurrent neural network

## Data

text_1<-read.delim("book_1.txt",header = FALSE)
text_2<-read.delim("book_2.txt",header = FALSE)
text_3<-read.delim("book_3.txt",header = FALSE)
text_4<-read.delim("book_4.txt",header = FALSE)
text_5<-read.delim("book_5.txt",header = FALSE)
text_6<-read.delim("book_6.txt",header = FALSE)

app_data<-read.csv("zipfs_law_data.csv")


## Bag of words 

text_1_clean <- text_1 %>%
  select(V1) %>% 
  unnest_tokens(word, V1)

## plot the top 10 words
text_1_clean %>%
  count(word, sort = TRUE) %>% 
  head(10) %>% # extract top 10 words
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "pink", color = "red") +
  coord_flip() +
  labs(x = "Unique Words",
       y = "Frequency",
       title = "Count of unique words found") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))
#
text_6_clean <- text_6 %>%
  select(V1) %>% 
  unnest_tokens(word, V1)

## plot the top 10 words
text_6_clean %>%
  count(word, sort = TRUE) %>% 
  head(10) %>% # extract top 10 words
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "pink", color = "red") +
  coord_flip() +
  labs(x = "Unique Words",
       y = "Frequency",
       title = "Count of unique words found") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))