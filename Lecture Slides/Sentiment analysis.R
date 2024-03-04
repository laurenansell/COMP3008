## Read in the required libraries
library(tidyverse)
library(ggthemes)
library(tidytext)
library(wordcloud)
library(wordcloud2)

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


# Barplot

ggplot(endgame_sentiment, 
       aes(x = score)) + # Sentiment score on x-axis
  geom_bar(fill = "lightgreen", colour = "darkgreen") + # geom_bar will do the tabulation for you :-)
  geom_vline(aes(xintercept = mean_score), data = sentiment_means) +
  # Add a vertical line at the mean score, calculated and stored in sentiment_means_nba above
  geom_text(aes(x = mean_score, 
                y = Inf, 
                label = signif(mean_score, 3)), # Show to three significant figures
            vjust = 2, 
            data = sentiment_means) + 
  # Add the mean as a number; vjust moves it down from the top of the plot
  scale_x_continuous(breaks = -10:10,  # Specify a suitable integer range for the x-axis
                     minor_breaks = NULL) + # Show integers; set this to a suitably large range
  labs(title = paste("Sentiments of tweets containing #endgame give a mean of", signif(sentiment_means$mean_score, 3)),
       # Title that gives page name and mean sentiment score, to three significant figures
       x = "Sentiment Score (Bing)", 
       y = "Number of tweets") 


cleaned_tweet_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("blue", "red"),
                   max.words = 100)

################ Afinn

## Join sentiment classification to the tweet words
afinn_word_counts <- cleaned_tweet_words %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = TRUE) %>%
  mutate(word = reorder(word, n)) 


## Calculate sentiment scores for each tweet

## We count up how many positive and negative words there are in each tweet

## Associate sentiment scores to each tweet

endgame_sentiment_afinn <- cleaned_tweet_words %>%
  inner_join(get_sentiments("afinn")) %>%
  count(tweetnumber, value) %>%
  spread(value, n, fill = 0) %>% # negative and positive sentiment in separate columns
  mutate(score = positive - negative) # score = net sentiment (positive - negative)
head(endgame_sentiment)


## Tabulate the scores

endgame_sentiment %>% count(score)

sentiment_means <- endgame_sentiment %>% 
  summarize(mean_score = mean(score)) 
sentiment_means


# Barplot

ggplot(endgame_sentiment, 
       aes(x = score)) + # Sentiment score on x-axis
  geom_bar(fill = "lightgreen", colour = "darkgreen") + # geom_bar will do the tabulation for you :-)
  geom_vline(aes(xintercept = mean_score), data = sentiment_means) +
  # Add a vertical line at the mean score, calculated and stored in sentiment_means_nba above
  geom_text(aes(x = mean_score, 
                y = Inf, 
                label = signif(mean_score, 3)), # Show to three significant figures
            vjust = 2, 
            data = sentiment_means) + 
  # Add the mean as a number; vjust moves it down from the top of the plot
  scale_x_continuous(breaks = -10:10,  # Specify a suitable integer range for the x-axis
                     minor_breaks = NULL) + # Show integers; set this to a suitably large range
  labs(title = paste("Sentiments of tweets containing #endgame give a mean of", signif(sentiment_means$mean_score, 3)),
       # Title that gives page name and mean sentiment score, to three significant figures
       x = "Sentiment Score", 
       y = "Number of tweets") 