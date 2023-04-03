#Wrangling for tweetGPT
#load packages
library(sf)
library(tidyverse)
library(leaflet)
library(maps)
library(janitor)
library(robotstxt)
library(rvest)
library(purrr)
library(dplyr)
library(stringi)
library(tidytext)
library(textdata)

#load files
afinn_lexicon <- get_sentiments("afinn")
nrc_lexicon <- get_sentiments("nrc")
#csvs
tweets_df <- read_csv("tweets2.csv")

#standardise column names
tweets_df <- tweets_df %>% 
  clean_names()

#change hashtag and bio column to characters
tweets_df$hashtags <- as.character(tweets_df$hashtags)
tweets_df$user_description <- as.character(tweets_df$user_description)
#tweets_df$user_name <- stri_replace_all_regex(tweets_df$user_name, "\\p{So}+", "")

#remove emojis from text and user description column
tweets_df$text <- stri_replace_all_regex(tweets_df$text, "\\p{So}+", "")
tweets_df$user_description <- stri_replace_all_regex(tweets_df$user_description, "\\p{So}+", "")

# Define function to calculate sentiment score and label
calculate_sentiment <- function(df) {
  df %>%
    # unnest text into individual words
    unnest_tokens(word, text) %>%
    # join with lexicons to get sentiment scores
    left_join(afinn_lexicon, by = "word") %>%
    left_join(nrc_lexicon, by = "word") %>%
    # group by tweet id and calculate sentiment score and label
    group_by(user_name) %>%
    summarise(sentiment_score = sum(value, na.rm = TRUE),
              sentiment_label = case_when(
                sentiment_score > 0 ~ "positive",
                sentiment_score < 0 ~ "negative",
                TRUE ~ "neutral"
              ))
}

#calculate sentiment tweets
sentiment_df <-
  calculate_sentiment(tweets_df)

# Join sentiment data with original data frame
tweets_df <- left_join(tweets_df, sentiment_df, by = "user_name")

#Save all data to data folder
save(tweets_df,
     file = "final_data/blog_data.RData")
