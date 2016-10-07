###########
#FileOrder
#1: TransgenderSentimentAnalysis
#2: WordCloud will work here
#3: AlternateReverseGeocode
#4: US HeatMaps will work here
###########

library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(plyr)
library(dplyr)
library(wordcloud)
library(SnowballC)

#############################
#Get Twitter Authentification
#############################

key="lQxWjN6LvpFdWpCcf6IetvcXC"
secret="f8c3753dddYR7DDsrZ4wrmElPyi925ZpXkEOJfpzpnOrLsn28m"
setwd("/Users/jamesmartherus/Dropbox (Personal)/Transgender_Policies/WordCloud_Sentiment/")

authenticate <- OAuthFactory$new(consumerKey=key,
                                 consumerSecret=secret,
                                 requestURL="https://api.twitter.com/oauth/request_token",
                                 accessURL="https://api.twitter.com/oauth/access_token",
                                 authURL="https://api.twitter.com/oauth/authorize")
setup_twitter_oauth(key, secret)
save(authenticate, file="twitter authentication.Rdata")

####################################
#Choose cities for tweets
####################################
N=300  # tweets to request from each query
S=200  # radius in miles
lats=c(38.9,40.7,37.8,39,37.4,28,30,42.4,48,36,33.5,33.8,37.2,41.2,46.8,
       46.6,37.2,43,42.7,40.8,36.2,38.6,35.8,40.3,43.6,40.8,29.95,45.79,43.54,44.99,35.48,38.35)

lons=c(-77,-74,-122,-105.5,-122,-82.5,-98,-71,-122,-115,-112,-84.4,-93.3,
       -104.8,-100.8,-112, -93.3,-89,-84.5,-111.8,-86.8,-92.2,-78.6,-76.8,-116.2,-98.7,-90.07,-108.49,-96.73,-93.26,-97.41,-81.63)

#cities=DC,New York,San Fransisco,Colorado,Mountainview,Tampa,Austin,Boston,
#       Seattle,Vegas,Phoenix,Atlanta,Springfield,
#       Cheyenne,Bisruk,Helena,Springfield,Madison,Lansing,Salt Lake City,Nashville
#       Jefferson City,Raleigh,Harrisburg,Boise,Lincoln,Salem,St. Paul, New Orleans
#       Billings,Sioux Falls, Minneapolis, Oklahoma City
#11,13,15,17,18,19,20,21

#####Get Tweets########
transgender=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('transgender',
                                                                      lang="en",n=N,resultType="recent",
                                                                      geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))))

translat=sapply(transgender, function(x) as.numeric(x$getLatitude()))
translat=sapply(translat, function(z) ifelse(length(z)==0,NA,z))  

translon=sapply(transgender, function(x) as.numeric(x$getLongitude()))
translon=sapply(translon, function(z) ifelse(length(z)==0,NA,z))  

transdate=lapply(transgender, function(x) x$getCreated())
transdate=sapply(transdate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

transtext=sapply(transgender, function(x) x$getText())
transtext=unlist(transtext)

isretweet=sapply(transgender, function(x) x$getIsRetweet())
retweeted=sapply(transgender, function(x) x$getRetweeted())
retweetcount=sapply(transgender, function(x) x$getRetweetCount())

favoritecount=sapply(transgender, function(x) x$getFavoriteCount())
favorited=sapply(transgender, function(x) x$getFavorited())

data_trans=as.data.frame(cbind(tweet=transtext,date=transdate,lat=translat,lon=translon,
                         isretweet=isretweet,retweeted=retweeted, retweetcount=retweetcount,favoritecount=favoritecount,favorited=favorited))


tweet=data_trans$tweet
tweet_list=lapply(tweet, function(x) iconv(x, "latin1", "ASCII", sub=""))
tweet_list=lapply(tweet, function(x) gsub("htt.*",' ',x))
tweet=unlist(tweet)
data_trans$tweet=tweet

###################
#Sentiment Analysis
###################

#Load positve and negative words for lexicon based sentiment analysis
positives= readLines("positivewords.txt")
negatives = readLines("negativewords.txt")

#Wrapper function to calculate sentiment scores
sent_score <- function(tweet, positive_words, negative_words){
  tweet = gsub("[[:punct:]]", "", tweet)    # remove punctuation
  tweet = gsub("[[:cntrl:]]", "", tweet)    # remove control characters
  tweet = gsub("[0-9]", "", tweet)          # remove digits
  
  # Error handling function when trying tolower function
  tryTolower = function(x){
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  # use tryTolower with sapply
  tweet = sapply(tweet, tryTolower)
  # split sentence into words with str_split function from stringr package
  word_list = str_split(tweet, " ")
  words = unlist(word_list)
  # compare words to the dictionaries of positive & negative terms
  positive_matches = match(words, positive_words)
  negative_matches = match(words, negative_words)
  # get the position of the matched term or NA
  # we just want a TRUE/FALSE
  positive_matches = !is.na(positive_matches)
  negative_matches = !is.na(negative_matches)
  # final score
  score = sum(positive_matches) - sum(negative_matches)
  return(score)
}
sentiment_scores = function(tweets, positive_words, negative_words){
  scores = lapply(tweets, sent_score, positive_words, negative_words)
  return(scores)
}

score = sentiment_scores(tweet, positives, negatives)
data_trans$score=unlist(as.numeric(score))







#########
#Analysis
#########
hist(as.numeric(data_trans$score), xlab="Sentiment Score", main="Sentiment of Tweets containing the word 'Transgender'",
     border="black",col="skyblue")



#######
#Export
#######
write.csv(as.data.frame(data_trans), "Transgender_Sentiment_Analysis_09_27.csv")

