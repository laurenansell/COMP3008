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
library(ggpubr)


## Data

text_1<-read.delim("book_1.txt",header = FALSE)
text_2<-read.delim("book_2.txt",header = FALSE)
text_3<-read.delim("book_3.txt",header = FALSE)
text_4<-read.delim("book_4.txt",header = FALSE)
text_5<-read.delim("book_5.txt",header = FALSE)
text_6<-read.delim("book_6.txt",header = FALSE)

all_texts<-read.csv("./Lecture Slides/zipfs_law_data.csv")


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

## N-grams

## bigram example

bigram_counts<-bigram_counts %>% na.omit() %>% mutate(word=paste(word1,word2,sep=" "))

bigram_counts %>% head(10) %>% 
  ggplot( aes(x = word, y = n)) +
  geom_col(fill = "cyan", color = "blue") +
  coord_flip() +
  labs(x = "Unique Bigrams",
       y = "Frequency",
       title = "Count of unique bigrams found") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))

bigram_counts_6<-bigram_counts_6 %>% na.omit() %>% mutate(word=paste(word1,word2,sep=" "))

bigram_counts_6 %>% head(10) %>% 
  ggplot( aes(x = word, y = n)) +
  geom_col(fill = "cyan", color = "blue") +
  coord_flip() +
  labs(x = "Unique Bigrams",
       y = "Frequency",
       title = "Count of unique bigrams found") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))

## 4-gram example

data_n_words <- text_1 %>%
  dplyr::select(V1) %>%
  unnest_tokens(n_words, V1, token = "ngrams", n = 4)


data_separated_words <- data_n_words %>%
  separate(n_words, c("word1", "word2","word3","word4"), sep = " ")

data_filtered <- data_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word4 %in% stop_words$word)

ngram_counts <- data_filtered %>% 
  count(word1, word2, word3, word4, sort = TRUE)

ngram_counts<-ngram_counts %>% na.omit() %>% mutate(word=paste(word1,word2,word3,word4,
                                                               sep=" "))

ngram_counts %>% head(10) %>% 
  ggplot( aes(x = word, y = n)) +
  geom_col(fill = "cyan", color = "blue") +
  coord_flip() +
  labs(x = "Unique ngrams (n=4)",
       y = "Frequency",
       title = "Count of unique ngrams found") + 
  theme(axis.text = element_text(size = 12, color = "black"), 
        axis.title = element_text(size = 12, color = "black"),
        title = element_text(size = 14))

data_n_words_6 <- text_6 %>%
  dplyr::select(V1) %>%
  unnest_tokens(n_words, V1, token = "ngrams", n = 4)


data_separated_words_6 <- data_n_words_6 %>%
  separate(n_words, c("word1", "word2","word3","word4"), sep = " ")

data_filtered_6 <- data_separated_words_6 %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word4 %in% stop_words$word)

ngram_counts_6 <- data_filtered_6 %>% 
  count(word1, word2, word3, word4, sort = TRUE)

ngram_counts_6<-ngram_counts_6 %>% na.omit() %>% mutate(word=paste(word1,word2,word3,word4,sep=" "))

ngram_counts_6 %>% head(10) %>% 
  ggplot( aes(x = word, y = n)) +
  geom_col(fill = "cyan", color = "blue") +
  coord_flip() +
  labs(x = "Unique ngrams (n=4)",
       y = "Frequency",
       title = "Count of unique ngrams found") + 
  theme(axis.text = element_text(size = 12, color = "black"), 
        axis.title = element_text(size = 12, color = "black"),
        title = element_text(size = 14))



## Term frequency - inverse document frequency (TF-IDF)

all_text_clean <- all_texts %>%
  select(V1,text) %>% 
  unnest_tokens(word, V1)

all_text_clean_no <- all_text_clean %>%
  anti_join(stop_words)

tf_idf_df <- all_text_clean_no %>%
  count(text, word) %>%
  bind_tf_idf(word, text, n) %>%
  arrange(desc(tf_idf)) 

tf_idf_df %>%
  group_by(text) %>%
  top_n(10, tf_idf) %>% ggplot(aes(x=word, y=tf_idf,fill=text))+
  geom_bar(stat = "identity")+facet_wrap(.~text, scales = "free")+
  coord_flip()+theme(legend.position = "none")

## Zipf's Law

all_text_clean <- all_texts %>%
  select(V1,text) %>% 
  unnest_tokens(word, V1)

zipfs_law<-all_text_clean %>%
  count(text,word) %>%
  bind_tf_idf(word,text,n)

zipfs_law<-zipfs_law %>%  arrange(desc(tf)) %>% group_by(text) %>% 
  mutate(rank=row_number())

## Plot all the data together

ggplot(zipfs_law,aes(x=log(rank),y=log(tf),col=text))+geom_line(linewidth=1)+
  geom_smooth(method = "lm",se=FALSE, linetype=2, linewidth=0.5)+
  stat_regline_equation(label.x =4.5)

## Or separately

zipfs_law_text_3<-zipfs_law %>% filter(text == "book 3")

ggplot(zipfs_law_text_3,aes(x=log(rank),y=log(tf)))+geom_line(linewidth=1)+
  geom_smooth(method = "lm",se=FALSE, linetype=2, linewidth=0.5)+
  stat_regline_equation(label.x =4.5)

## or combinations

zipfs_law_texts_1_5<-zipfs_law %>% filter(text %in% c("book 1", "book 5"))

ggplot(zipfs_law_texts_1_5,aes(x=log(rank),y=log(tf),col=text))+geom_line(linewidth=1)+
  geom_smooth(method = "lm",se=FALSE, linetype=2, linewidth=0.5)+
  stat_regline_equation(label.x =4.5)

## Neural networks


## rnn package (this will take a long time to run)

text.1 = as.matrix(text_2)

text_4$V1<-gsub('[[:punct:] ]+',' ',text_4$V1)

text.2 = as.matrix(text_4)

matrix.text.1 = matrix(nrow = 0, ncol = 8)

for (i in 1:NROW(text.1)){
  matrix.1 = int2bin(utf8ToInt(text.1[i]))
  matrix.text.1 = rbind(matrix.1, matrix.text.1)
}



matrix.text.2 = matrix(nrow = 0, ncol = 8)

for (i in 1:NROW(text.2)){
  matrix.2 = int2bin(utf8ToInt(text.2[i]))
  matrix.text.2 = rbind(matrix.2, matrix.text.2)
}

X = rbind(matrix.text.1, matrix.text.2)

y = int2bin(rep(c(2,4), times = c(nrow(matrix.text.1), nrow(matrix.text.2))))

model.rnn = trainr(Y = y, X = X, learningrate = 1, numepochs = 1, hidden_dim = 1)


## keras and tensorflow (faster than above, need to have tensorflow installed)

max_words <- 10000 
max_len <- 100 

# Load the IMDB data 
imdb <- dataset_imdb(num_words = max_words) 

# Split the data into training and test sets 
x_train <- imdb$train$x 
y_train <- imdb$train$y 
x_test <- imdb$test$x 
y_test <- imdb$test$y 

# Pad the sequences to have a fixed length 
x_train <- pad_sequences(x_train, maxlen = max_len) 
x_test <- pad_sequences(x_test, maxlen = max_len)


model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_words, 
                  output_dim = 32) %>% 
  layer_lstm(units = 32) %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile( 
  optimizer = "adam", 
  loss = "binary_crossentropy", 
  metrics = c("accuracy") 
)

history <- model %>% fit( 
  x_train, y_train, 
  epochs = 10, 
  batch_size = 32, 
  validation_split = 0.2 
)


scores <- model %>% evaluate(x_test, y_test, 
                             verbose = 0) 
print(paste("Test accuracy:", scores[[2]]))
