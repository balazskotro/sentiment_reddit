#delete everything
rm(list = ls())

library(readr)
library(lubridate)
library(dplyr)

library(dplyr)
library(tidytext)
library(textdata)
library(stringr)
library(tidyr)
library(ggplot2)
library(wordcloud2)
library(scales)
library(igraph)
library(ggraph)
library(widyr)
library(topicmodels)

#read the comments from csv file
comments <- read_delim("C:/Research/sentiment_reddit/sentiment_reddit/comments_0415.csv", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

comments$Number <- seq.int(nrow(comments))

#drop first, empty column
comments$Epoch <- NULL

#convert unix time integer to date object (Eastern time is used, reddit also uses that format)
comments$Dates <- lubridate::as_datetime(comments$Dates, origin="1970-01-01",tz = "America/New_York")

#drop all the observations which are not within 9:30 am. and 4 pm (NYSE trading hours)
comments<- comments[comments[["Dates"]] <= as_datetime("2021-04-15 16:00:00" ,tz = "America/New_York"), ]
comments<- comments[comments[["Dates"]] >= as_datetime("2021-04-15 09:00:00" ,tz = "America/New_York"), ]
comments <- dplyr::arrange(comments, Dates)

#get rid of duplicated emojis
##emojis are in the following format :emoji: › idea: split them by the colon and remove duplicates, and concatante the string again

emoji_converter <- function(input)
  {d <- unlist(strsplit(input, split=":"))
  input <- paste(unique(d), collapse = ' ')
  input <- trimws(input, which = c("both"), whitespace = "[ \t\r\n]")
  return(input)}


comments$Body <- sapply(comments$Body,emoji_converter)


tidy_texts <- comments %>%
  unnest_tokens(word, Body)

tidy_texts

tidy_texts %>%
  count(word, sort = TRUE)

data(stop_words)
tidy_texts <- tidy_texts %>%
  anti_join(stop_words)

tidy_texts %>%
  count(word, sort = TRUE)

tidy_texts %>%
  count(word, sort = TRUE) %>%
  filter(n > 400) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()+theme(text = element_text(size=20))

afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- read_csv("C:\\Users\\x\\Desktop\\nrc.csv")


tidy_texts %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

  SW_sentiment <- tidy_texts %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = Number %/% 1, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

  SW_sentiment

  ggplot(SW_sentiment, aes(index, sentiment)) +
    geom_col(show.legend = FALSE) +
    theme(text = element_text(size=20))
  
