## Read in the required libraries
library(tidyverse)
library(ggthemes)
library(tidytext)
library(textdata)
library(wordcloud)
library(wordcloud2)
library(tidyEmoji)

## Read in the data
twitter_data<-read.csv("../endgame_tweets.csv", encoding = "UTF-8")
## The dataset is from Kaggle and can be downloaded from here: 
## https://www.kaggle.com/datasets/kavita5/twitter-dataset-avengersendgame

## Look at the variable names
names(twitter_data)

## Look at the structure of the data
str(twitter_data)

## We are going to keep the columns which hold the tweet text, time stamp it was created and the 
## the first column for the tweet ID.

endgame_tweets_clean <- twitter_data %>%
  select(text) %>% 
  mutate(tweetnumber = row_number()) %>% # create new variable denoting the tweet number
  unnest_tokens(word, text)

head(endgame_tweets_clean)


## Load list of stop words - from the tidytext package
data("stop_words")

## Remove stop words from your list of words
cleaned_tweet_words <- endgame_tweets_clean %>%
  anti_join(stop_words) # return all rows where there are not matching values in stop_words



############ Bing

## Join sentiment classification to the tweet words
bing_word_counts <- cleaned_tweet_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  mutate(word = reorder(word, n)) 


## Calculate sentiment scores for each tweet

## We count up how many positive and negative words there are in each tweet

## Associate sentiment scores to each tweet

endgame_sentiment <- cleaned_tweet_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(tweetnumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% # negative and positive sentiment in separate columns
  mutate(score = positive - negative) # score = net sentiment (positive - negative)
head(endgame_sentiment)


## Tabulate the scores

endgame_sentiment %>% count(score)

sentiment_means <- endgame_sentiment %>% 
  summarize(mean_score = mean(score)) 
sentiment_means


## Barplot

ggplot(endgame_sentiment, 
       aes(x = score)) + # Sentiment score on x-axis
  geom_bar(fill = "lightgreen", colour = "darkgreen") + # geom_bar will do the tabulation for you :-)
  geom_vline(aes(xintercept = mean_score), data = sentiment_means) +
  # Add a vertical line at the mean score, calculated and stored in sentiment_means_nba above
  geom_text(aes(x = mean_score, 
                y = Inf, 
                label = signif(mean_score, 3)), # Show to three significant figures
            vjust = 1.3, 
            data = sentiment_means) + 
  # Add the mean as a number; vjust moves it down from the top of the plot
  scale_x_continuous(breaks = -10:10,  # Specify a suitable integer range for the x-axis
                     minor_breaks = NULL) + # Show integers; set this to a suitably large range
  labs(title = paste("Sentiments of tweets containing #endgame give a mean of", signif(sentiment_means$mean_score, 3)),
       # Title that gives page name and mean sentiment score, to three significant figures
       x = "Sentiment Score (Bing)", 
       y = "Number of tweets")+
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size=16),
        axis.text = element_text(size=14))



## Time series

endgame_sentiment<-left_join(endgame_sentiment,twitter_data,by=c("tweetnumber"="X"))

## Convert the time stamp from a character string
endgame_sentiment$created<-as.POSIXct(endgame_sentiment$created)

endgame_sentiment %>% group_by(created) %>% summarise(total=sum(score)) %>% 
  ggplot(aes(x=created,y=total))+geom_line(linetype=2)+  
  labs(title = paste("Timeseries of sentiments of tweets containing #endgame"),
       x = "Time", y = "Total Sentiment (Bing)")+
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size=16),
        axis.text = element_text(size=14))

################ Afinn

## Join sentiment classification to the tweet words
afinn_word_counts <- cleaned_tweet_words %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = TRUE) %>%
  mutate(word = reorder(word, n)) 


## Calculate sentiment scores for each tweet

endgame_sentiment_afinn <- cleaned_tweet_words %>%
  inner_join(get_sentiments("afinn")) %>%
  count(tweetnumber,value) 


endgame_sentiment_afinn<-endgame_sentiment_afinn%>%group_by(tweetnumber)%>%
  mutate(total=sum(value*n))

## Remove duplicate values

endgame_sentiment_afinn<-endgame_sentiment_afinn[!duplicated(endgame_sentiment_afinn$tweetnumber),]


sentiment_means_afinn <- as.data.frame(mean(endgame_sentiment_afinn$total))   
sentiment_means_afinn
names(sentiment_means_afinn)[1]<-"mean_score"


# Distribution of the sentiments  
ggplot(endgame_sentiment_afinn, 
       aes(x = total)) + 
  geom_bar(fill = "lightgreen", colour = "darkgreen") +
  geom_vline(aes(xintercept = mean_score), data = sentiment_means_afinn) +
  geom_text(aes(x = mean_score, 
                y = Inf, 
                label = signif(mean_score, 3)), 
            vjust =1.3, 
            data = sentiment_means_afinn) + 
  scale_x_continuous(breaks = c(-15,-10,5,0,5,10,15)) + 
  labs(title = paste("Sentiments of tweets containing #endgame give a mean of", signif(sentiment_means_afinn$mean_score, 3)),
       x = "Sentiment Score (Afinn)", 
       y = "Number of tweets") +
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size=16),
        axis.text = element_text(size=14))


## Time series

endgame_sentiment_afinn<-left_join(endgame_sentiment_afinn,twitter_data,by=c("tweetnumber"="X"))

## Convert the time stamp from a character string
endgame_sentiment_afinn$created<-as.POSIXct(endgame_sentiment_afinn$created)

endgame_sentiment_afinn %>% group_by(created) %>% summarise(total_score=sum(total)) %>% 
  ggplot(aes(x=created,y=total_score))+geom_line(linetype=2)+  
  labs(title = paste("Timeseries of sentiments of tweets containing #endgame"),
       x = "Time", y = "Total Sentiment (Afinn)")+
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size=16),
        axis.text = element_text(size=14))

## Wordcloud example for each lexicon

example_text<-read.delim("./Lecture Slides/Example text.txt", header = FALSE)


example_text <- example_text %>%
  select(V1) %>% 
  unnest_tokens(word, V1)


example_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("blue", "red"),
                   max.words = 100)


example_text %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = TRUE) %>%
  reshape2::acast(word ~ value, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("blue", "red","gold","green","purple","hotpink","seagreen"),
                   max.words = 100)


example_text %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("blue", "red","gold","green","purple","hotpink","seagreen",
                              "darkblue","darkmagenta"),
                   max.words = 100)


example_text %>%
  inner_join(get_sentiments("loughran")) %>%
  count(word, sentiment, sort = TRUE) %>%
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("blue", "red","gold","green","purple","hotpink","seagreen",
                              "darkblue","darkmagenta"),
                   max.words = 100)


###### Emoji data

ata_tweets <- readr::read_csv("./Lecture Slides/ata_tweets.rda")

## See how many tweets contain emojis
ata_tweets %>% emoji_summary(full_text)

## See how many emojis are contained in each tweet
ata_tweets %>%
  emoji_extract_nest(full_text) %>%
  select(.emoji_unicode) 


## See which emojis have been used

emoji_count_per_tweet <- ata_tweets %>%
  emoji_extract_unnest(full_text) 

emoji_count_per_tweet

## Count the number of emojis
emoji_count_per_tweet %>%
  group_by(.emoji_count) %>%
  summarize(n = n()) %>%
  ggplot(aes(.emoji_count, n)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1,15)) +
  ggtitle("How many Emoji does each Emoji Tweet have?")


## Top 20 emojis

top_20_emojis <- ata_tweets %>%
  top_n_emojis(full_text)

top_20_emojis

## Plot these

top_20_emojis %>%
  ggplot(aes(n, emoji_name, fill = emoji_category)) +
  geom_col()

## we can add the emojis to the plot
top_20_emojis %>%
  mutate(emoji_name = stringr::str_replace_all(emoji_name, "_", " "),
         emoji_name = forcats::fct_reorder(emoji_name, n)) %>%
  ggplot(aes(n, emoji_name, fill = emoji_category)) +
  geom_col() +
  geom_text(aes(label = unicode), hjust = 0.1) +
  labs(x = "# of Emoji",
       y = "Emoji name",
       fill = "Emoji category",
       title = "The 20 most popular Emojis")

