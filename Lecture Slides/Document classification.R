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

## First without removing stop words

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


## Now removing stop words

data("stop_words")

text_1_clean <- text_1_clean %>%
  anti_join(stop_words)

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

text_6_clean <- text_6_clean %>%
  anti_join(stop_words)

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

## Two texts by the same author


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

text_3_clean <- text_3 %>%
  select(V1) %>% 
  unnest_tokens(word, V1)

text_3_clean <- text_3_clean %>%
  anti_join(stop_words)

## plot the top 10 words
text_3_clean %>%
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

## Two texts by a different author

text_4_clean <- text_4 %>%
  select(V1) %>% 
  unnest_tokens(word, V1)

text_4_clean <- text_4_clean %>%
  anti_join(stop_words)

## plot the top 10 words
text_4_clean %>%
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

text_5_clean <- text_5 %>%
  select(V1) %>% 
  unnest_tokens(word, V1)

text_5_clean <- text_5_clean %>%
  anti_join(stop_words)

## plot the top 10 words
text_5_clean %>%
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

## Visualise the relationship between words

## Author 1 

data_paired_words <- text_1 %>%
  dplyr::select(V1) %>%
  unnest_tokens(paired_words, V1, token = "ngrams", n = 2)


data_separated_words <- data_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

data_filtered <- data_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- data_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts<-bigram_counts %>% na.omit()

bigram_graph <- bigram_counts %>%
  filter(n > 2) %>%
  graph_from_data_frame()
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1,size=4)+
  theme_graph(base_size=10)+
  ggtitle("Network of words")

## Author 2

data_paired_words_6 <- text_6 %>%
  dplyr::select(V1) %>%
  unnest_tokens(paired_words, V1, token = "ngrams", n = 2)


data_separated_words_6 <- data_paired_words_6 %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

data_filtered_6 <- data_separated_words_6 %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts_6 <- data_filtered_6 %>% 
  count(word1, word2, sort = TRUE)

bigram_counts_6<-bigram_counts_6 %>% na.omit()

bigram_graph_6 <- bigram_counts_6 %>%
  filter(n > 2) %>%
  graph_from_data_frame()
ggraph(bigram_graph_6, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1,size=4)+
  theme_graph(base_size=10)+
  ggtitle("Network of words")