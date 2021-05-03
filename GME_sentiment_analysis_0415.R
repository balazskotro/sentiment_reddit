#delete everything
rm(list = ls())

library(readr)
library(lubridate)
library(dplyr)
library(readxl)
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
library(reshape2)

setwd('C:\\Research\\sentiment_reddit')

#read the comments from csv file
comments <- read_delim("comments_0415.csv", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

comments$Number <- seq.int(nrow(comments))

#drop first, empty column
comments$Epoch <- NULL

#convert unix time integer to date object (Eastern time is used, reddit also uses that format)
comments$Dates <- lubridate::as_datetime(comments$Dates, origin="1970-01-01",tz = "America/New_York")

#drop all the observations which are not within 9:30 am. and 4 pm (NYSE trading hours)
comments<- comments[comments[["Dates"]] <= as_datetime("2021-04-15 16:00:00" ,tz = "America/New_York"), ]
comments<- comments[comments[["Dates"]] >= as_datetime("2021-04-15 09:30:00" ,tz = "America/New_York"), ]
comments <- dplyr::arrange(comments, Dates)

#get rid of duplicated emojis
##emojis are in the following format :emoji:  idea: split them by the colon and remove duplicates, and concatante the string again

emoji_converter <- function(input)
  {d <- unlist(strsplit(input, split=":"))
  input <- paste(unique(d), collapse = ' ')
  input <- trimws(input, which = c("both"), whitespace = "[ \t\r\n]")
  return(input)}

comments$Body <- sapply(comments$Body,emoji_converter)

#tokenize the comments
tidy_texts <- comments %>%
  unnest_tokens(word, Body)

#get rid of stop words
data(stop_words)
tidy_texts <- tidy_texts %>%
  anti_join(stop_words)

#count meaningful data
tidy_texts %>%
  count(word, sort = TRUE)

#load different dictionaries, we are going to use the "bing" one
afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- read_csv("nrc.csv")

# find the sentiment score of each possible comment (quite big data loss here because of poor dictionary)
sentiment_data <- tidy_texts %>%
  inner_join(get_sentiments("bing")) %>%
  count(Dates, Author, `Comment Karma`, `Link Karma`, Score, index = Number %/% 1, sentiment, Dates) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# group all sentiment scores by one minute interval (GT means Groupped Time)
sentiment_data$GT <- cut(sentiment_data$Dates, breaks = "1 min")
sentiment_data<-aggregate(x = sentiment_data$sentiment, by = list(sentiment_data$GT),  FUN = sum) 

#(SV means Sentiment Value)
colnames(sentiment_data)<- c("Date", "SV")
  
# standardize the values
sentiment_data$SSV <- (scale(sentiment_data$SV))

#read stock data from NASDAQ (collected manually)  
Stock_data <- read_excel("4_15_prices.xlsx", sheet = "Sheet1")

#convert string to date
Stock_data$Date<- lubridate::as_datetime(Stock_data$Date)

#create a dataset from the timestamp, the standardized stock price and the standardized sentiment value
examination_data <- as.data.frame(cbind(Stock_data$Date, sentiment_data$SSV, scale(Stock_data$Close)))
colnames(examination_data)<-c ("Date", "Sentiment", "Price")
examination_data$Date <- lubridate::as_datetime(examination_data$Date)

#melt for plot
melted<-melt(examination_data,id="Date")

#single plot
ggplot(melted, aes(Date, value, color=variable))+ geom_line()

#correlation calculation
cor(examination_data$Sentiment , examination_data$Price ,  method = "pearson")

#result: zero correlation, this naive model is weak!
