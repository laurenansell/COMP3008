library(tidyverse)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(ggpubr)
library(igraph)
library(ggraph)
library(topicmodels)
library(tm)
library(mdsr) 
library(FactoMineR)
library(data.table)
library(Matrix)

## Load in the data

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

clustering_data<-zipfs_law[,c("text","word")]

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
    cat("Min row sum\t", min(rowSums(m)),"\tMin col sum\t",min(colSums(m)), "\n")
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

all_songs<-rbind(song_1_words_clean,song_2_words_clean,song_3_words_clean,song_4_words_clean,
                 song_5_words_clean,song_6_words_clean,song_7_words_clean,song_8_words_clean) %>% 
  count(text,word,sort=TRUE)

songs_dtm<-all_songs %>% cast_dtm(text,word,n)

genre_lda <- LDA(songs_dtm, k = 2, control = list(seed = 2))
genre_lda

genre_topics <- tidy(genre_lda, matrix = "beta")
genre_topics

top_terms <- genre_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


genre_gamma <- tidy(genre_lda, matrix = "gamma")
genre_gamma
genre_gamma %>%
  mutate(document = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ document) +
  labs(x = "topic", y = expression(gamma))

