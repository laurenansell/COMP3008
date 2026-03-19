library(tidyverse)
library(ggthemes)
library(tidytext)
library(textdata)
library(igraph)
library(ggraph)
library(wordcloud)
library(wordcloud2)
library(ggpubr)
library(topicmodels)
library(tm)
library(mdsr) 
library(FactoMineR)
library(data.table)
library(Matrix)

text_1<-read.delim("./Lecture Slides/book_1.txt",header = FALSE)
text_2<-read.delim("./Lecture Slides/book_2.txt",header = FALSE)
text_3<-read.delim("./Lecture Slides/book_3.txt",header = FALSE)
text_4<-read.delim("./Lecture Slides/book_4.txt",header = FALSE)
text_5<-read.delim("./Lecture Slides/book_5.txt",header = FALSE)
text_6<-read.delim("./Lecture Slides/book_6.txt",header = FALSE)

## Bag-of-words


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


## N-grams

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

## tf-idf

all_data<-read.csv("./Lecture Slides/zipfs_law_data.csv")


all_text_clean <- all_data %>%
  select(V1,text) %>% 
  unnest_tokens(word, V1)

all_text_clean_no <- all_text_clean %>%
  anti_join(stop_words)

b_t <- all_text_clean_no %>%
  count(text, word) %>%
  bind_tf_idf(word, text, n) %>%
  arrange(desc(tf_idf)) 

b_t %>%
  group_by(text) %>%
  top_n(10, tf_idf) %>% ggplot(aes(x=word, y=tf_idf,fill=text))+
  geom_bar(stat = "identity")+facet_wrap(.~text, scales = "free")+
  coord_flip()+theme(legend.position = "none")

## PCA and topic modelling

song_1<-read.delim("./Lecture Slides/Song 1.txt",header = FALSE)
song_2<-read.delim("./Lecture Slides/Song 2.txt",header = FALSE)
song_3<-read.delim("./Lecture Slides/Song 3.txt",header = FALSE)
song_4<-read.delim("./Lecture Slides/Song 4.txt",header = FALSE)
song_5<-read.delim("./Lecture Slides/Song 5.txt",header = FALSE)
song_6<-read.delim("./Lecture Slides/Song 6.txt",header = FALSE)
song_7<-read.delim("./Lecture Slides/Song 7.txt",header = FALSE)
song_8<-read.delim("./Lecture Slides/Song 8.txt",header = FALSE)

## Tokenise the data

song_1_words<-song_1 %>% select(V1) %>%  unnest_tokens(word, V1)
song_2_words<-song_2 %>% select(V1) %>%  unnest_tokens(word, V1)
song_3_words<-song_3 %>% select(V1) %>%  unnest_tokens(word, V1)
song_4_words<-song_4 %>% select(V1) %>%  unnest_tokens(word, V1)
song_5_words<-song_5 %>% select(V1) %>%  unnest_tokens(word, V1)
song_6_words<-song_6 %>% select(V1) %>%  unnest_tokens(word, V1)
song_7_words<-song_7 %>% select(V1) %>%  unnest_tokens(word, V1)
song_8_words<-song_8 %>% select(V1) %>%  unnest_tokens(word, V1)

## Remove the stop words

song_1_words_clean<-song_1_words %>% anti_join(stop_words)
song_2_words_clean<-song_2_words %>% anti_join(stop_words)
song_3_words_clean<-song_3_words %>% anti_join(stop_words)
song_4_words_clean<-song_4_words %>% anti_join(stop_words)
song_5_words_clean<-song_5_words %>% anti_join(stop_words)
song_6_words_clean<-song_6_words %>% anti_join(stop_words)
song_7_words_clean<-song_7_words %>% anti_join(stop_words)
song_8_words_clean<-song_8_words %>% anti_join(stop_words)

## PCA

song_1_words<-song_1_words_clean %>% mutate(text="song 1")
song_2_words<-song_2_words_clean %>% mutate(text="song 2")
song_3_words<-song_3_words_clean %>% mutate(text="song 3")
song_4_words<-song_4_words_clean %>% mutate(text="song 4")
song_5_words<-song_5_words_clean %>% mutate(text="song 5")
song_6_words<-song_6_words_clean %>% mutate(text="song 6")
song_7_words<-song_7_words_clean %>% mutate(text="song 7")
song_8_words<-song_8_words_clean %>% mutate(text="song 8")


all_songs<-rbind(song_1_words,song_2_words,song_3_words,song_4_words,
                 song_5_words,song_6_words,song_7_words,song_8_words)


zipfs_law<-all_songs %>%
  count(text,word) %>%
  bind_tf_idf(word,text,n)

clustering_data<-zipfs_law[,c("text","word","n")]

term_freq_table <- clustering_data %>%
  filter(!(word %in% stopwords('en'))) %>%  # remove stop words
  group_by(text, word) %>%
  summarize(freq = n()) %>%
  collect()

u_terms <- sort(unique(term_freq_table$word))
ids <- sort(unique(term_freq_table$text))
row.ix <- match(term_freq_table$text, ids)
col.ix <- match(term_freq_table$word, u_terms)
m <- sparseMatrix(i = row.ix, j = col.ix, x = term_freq_table$freq)
rownames(m) <- ids
colnames(m) <- u_terms

reduce.tdm <- function(m, rs = 1, cs = 2) {
  repeat {
    i <- sum(dim(m))
    #cat("Min row sum\t", min(rowSums(m)),"\tMin col sum\t",min(colSums(m)), "\n")
    m <- m[rowSums(m) >= rs, colSums(m) >= cs]
    if(sum(dim(m)) == i) break
  }
  return(m)
}
m <- reduce.tdm(m)

m.pca <- PCA(as.matrix(m))


## Topic modelling

song_1_words_clean<-song_1_words_clean %>% mutate(text="song 1")
song_2_words_clean<-song_2_words_clean %>% mutate(text="song 2")
song_3_words_clean<-song_3_words_clean %>% mutate(text="song 3")
song_4_words_clean<-song_4_words_clean %>% mutate(text="song 4")
song_5_words_clean<-song_5_words_clean %>% mutate(text="song 5")
song_6_words_clean<-song_6_words_clean %>% mutate(text="song 6")
song_7_words_clean<-song_7_words_clean %>% mutate(text="song 7")
song_8_words_clean<-song_8_words_clean %>% mutate(text="song 8")

all_songs<-rbind(song_1_words_clean,song_2_words_clean,song_3_words_clean,song_4_words_clean,song_5_words_clean,song_6_words_clean,song_7_words_clean,song_8_words_clean) %>% count(text,word,sort=TRUE)

songs_dtm<-all_songs %>% cast_dtm(text,word,n)

genre_lda <- LDA(songs_dtm, k = 2, control = list(seed = 2))
#genre_lda

genre_topics <- tidy(genre_lda, matrix = "beta")
#genre_topics

top_terms <- genre_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% 
  ungroup() %>%
  arrange(topic, -beta)


genre_gamma <- tidy(genre_lda, matrix = "gamma")
#genre_gamma
genre_gamma %>%
  mutate(document = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ document) +
  labs(x = "topic", y = expression(gamma))

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()










  
  
  
  
  
  
  
  
  
  

