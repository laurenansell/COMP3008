library(tidyverse)
library(tidytext)
library(wordcloud)
library(wordcloud2)

example_text<-read.delim("./Lecture Code/Example Text.txt",header = FALSE)

example_text<-example_text %>% select(V1) %>% unnest_tokens(word,V1)

example_text %>% inner_join(get_sentiments("bing")) %>% 
  count(word,sentiment,sort = TRUE) %>% 
  reshape2::acast(word~sentiment,value.var="n",fill=0) %>% 
  comparison.cloud(colors = c("pink","yellow"),
                   max.words = 100)


example_text %>% inner_join(get_sentiments("afinn")) %>% 
  count(word,value,sort = TRUE) %>% 
  reshape2::acast(word~value,value.var="n",fill=0) %>% 
  comparison.cloud(colors = c("pink","yellow","green","blue","red","orange",
                              "brown"),
                   max.words = 100,match.colors = TRUE)


example_text_TS<-example_text %>% mutate(wordnumber=row_number())

library(textdata)

data("stop_words")

cleaned_example_text_TS<-example_text_TS %>% anti_join(stop_words)

example_text_sentiment<-cleaned_example_text_TS %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(wordnumber,sentiment) %>% spread(sentiment,n,fill = 0) %>% 
  mutate(score=positive-negative)


example_text_sentiment %>% ggplot(aes(x=wordnumber,y=score))+geom_line()
