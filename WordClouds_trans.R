library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(wordcloud)
library(SnowballC)

setwd("/Users/jamesmartherus/Dropbox (Personal)/Transgender_Policies/WordCloud_Sentiment/")
data <- read.csv("Transgender_Sentiment_Analysis_09_27.csv")

###########
#Wordclouds
###########
data$tweet <- as.character(data$tweet)

#TRUMP
trump_corpus <- Corpus(VectorSource(data$tweet))
trump_corpus <- tm_map(trump_corpus,
                       content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                       mc.cores=1)
trump_corpus <- tm_map(trump_corpus, content_transformer(tolower), lazy=TRUE)
trump_corpus <- tm_map(trump_corpus, PlainTextDocument, lazy=TRUE)
trump_corpus <- tm_map(trump_corpus, removePunctuation, lazy=TRUE)
#stopwords are things like I, me, etc.
trump_corpus <- tm_map(trump_corpus, removeWords, c(stopwords('SMART'),'http\\w+','[a-z][0-9]+.','transgender','transgend','hollygonightly1','Transgender'), lazy=TRUE)
#this converts all words to stem, i.e., standing to stand
#trump_corpus <- tm_map(trump_corpus, stemDocument, lazy=TRUE)

wordcloud(trump_corpus, rot.per=.25,scale=c(5,.5), max.words=100, random.order=FALSE,colors=brewer.pal(8,"Dark2"))











