################################# Twitter API Analysis #################################

#library's used

library(mcsm)
library(ggplot2)
library(Hmisc)
library(datasets)
library(gvlma)
library(Rcpp)
library(rpart)
library(Hmisc)
library(moments)
library(rattle)
library(rpart.plot)
library(mlbench)
library(RSpectra)
library(tm)
library(twitteR)
library(lubridate)
library(dplyr)
library(readr)

################################# Initial Data Cleaning #################################

#Read Files
CC <- read.csv("ConstructConnx_tweets.csv")
DDA <- read.csv("Dodgedata_tweets.csv")


#Create Dataframes
CC <- as.data.frame(CC)
DDA <- as.data.frame(DDA)

#Add identifier
CC <- cbind(Company = "CMD", CC)
DDA <- cbind(Company = "DDA", DDA)

#Combined to Make Master File
Master <- rbind(CC,DDA)

#Clean Date Columns

##Split Date/Time

#Master File
Master$Date <- as.Date(Master$created_at, tz = "MST")
Master$Month <- as.numeric(format(Master$Date, format = "%m"))

Master$Time <- format(as.POSIXct(Master$created_at) ,format = "%H:%M:%S")

with(Master, month.abb[Month])
Master <- transform(Master, MonthAbb = month.abb[Month])

#Dodge Only File
DDA$Date <- as.Date(DDA$created_at, tz = "MST")
DDA$Month <- as.numeric(format(DDA$Date, format = "%m"))

DDA$Time <- format(as.POSIXct(DDA$created_at) ,format = "%H:%M:%S")

#Construct Connect Only File

CC$Date <- as.Date(CC$created_at, tz = "MST")
CC$Month <- as.numeric(format(CC$Date, format = "%m"))

CC$Time <- format(as.POSIXct(CC$created_at) ,format = "%H:%M:%S")

################################# Word Cloud #################################

##Master Word Cloud

#clean tweets
mycorpus <- Corpus(VectorSource(Master$text))
mycorpus <- tm_map(mycorpus, removeWords, stopwords())

#Manual Removal of words
mystopwords <- c(stopwords('english'),"amp","xexxa")
mycorpus <- tm_map(mycorpus, removeWords, mystopwords)

#remove URL
remove_url <- function(x) gsub("http[^[:space:]]*","",x)
mycorpus <- tm_map(mycorpus, content_transformer(remove_url))

#Remove anything other than english letters and space
removenumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
mycorpus <- tm_map(mycorpus, content_transformer(removenumPunct))
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus <- tm_map(mycorpus, stemDocument)

dtm <- DocumentTermMatrix(mycorpus)

library(wordcloud)
wordcloud(mycorpus, min.freq = 20)
wordcloud(mycorpus, min.freq = 20, random.order = F, colors = brewer.pal(9,"Set1"))

mycorpus <- tm_map(mycorpus, PlainTextDocument)

## Dodge Word Cloud 

ddacorpus <- Corpus(VectorSource(DDA$text))
ddacorpus <- tm_map(ddacorpus, removeWords, stopwords())

#Manual Removal of words
mystopwords <- c(stopwords('english'),"amp","xexxa")
ddacorpus <- tm_map(ddacorpus, removeWords, mystopwords)

#remove URL
remove_url <- function(x) gsub("http[^[:space:]]*","",x)
ddacorpus <- tm_map(ddacorpus, content_transformer(remove_url))

#Remove anything other than english letters and space
removenumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
ddacorpus <- tm_map(ddacorpus, content_transformer(removenumPunct))
ddacorpus <- tm_map(ddacorpus, content_transformer(tolower))
ddacorpus <- tm_map(ddacorpus, stripWhitespace)
ddacorpus <- tm_map(ddacorpus, stemDocument)

dtm <- DocumentTermMatrix(ddacorpus)

library(wordcloud)
wordcloud(ddacorpus, min.freq = 20)
wordcloud(ddacorpus, min.freq = 50, random.order = F, colors = brewer.pal(11,"Set1"))

## Construct Connect Word Cloud

cccorpus <- Corpus(VectorSource(CC$text))
cccorpus <- tm_map(cccorpus, removeWords, stopwords())

#Manual Removal of words
mystopwords <- c(stopwords('english'),"amp","xexxa")
cccorpus <- tm_map(cccorpus, removeWords, mystopwords)

#remove URL
remove_url <- function(x) gsub("http[^[:space:]]*","",x)
cccorpus <- tm_map(cccorpus, content_transformer(remove_url))

#Remove anything other than english letters and space
removenumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
cccorpus <- tm_map(cccorpus, content_transformer(removenumPunct))
cccorpus <- tm_map(cccorpus, content_transformer(tolower))
cccorpus <- tm_map(cccorpus, stripWhitespace)
cccorpus <- tm_map(cccorpus, stemDocument)

dtm <- DocumentTermMatrix(cccorpus)

library(wordcloud)
wordcloud(cccorpus, min.freq = 20)
wordcloud(cccorpus, min.freq = 50, random.order = F, colors = brewer.pal(3,"Paired"))

################################# Hash Tag Frequency Chart #################################

## Master Frequency

vec1Con <- CC$text

extract.hashes <- function(vec){
  
  hash.pattern <- "#[[:alpha:]]+"
  have.hash <- grep(x = vec, pattern = hash.pattern)
  
  hash.matches <- gregexpr(pattern = hash.pattern,
                           text = vec[have.hash])
  extracted.hash <- regmatches(x = vec[have.hash], m = hash.matches)
  
  df <- data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) <- c("tag","freq")
  df <- df[order(df$freq,decreasing = TRUE),]
  return(df)
}

dat <- head(extract.hashes(vec1Con),10)
dat2 <- transform(dat,tag = reorder(tag,freq))


library(ggplot2)

p = ggplot(dat2, aes(x = tag, y = freq, count))  + geom_bar(stat = 'identity') 
p + coord_flip() + labs(title = "Hashtag frequencies in the tweets of both Dodge Data and Construct Connect")

##Dodge Frequency 

vec1 <- DDA$text

extract.hashes <- function(vec){
  
  hash.pattern <- "#[[:alpha:]]+"
  have.hash <- grep(x = vec, pattern = hash.pattern)
  
  hash.matches <- gregexpr(pattern = hash.pattern,
                           text = vec[have.hash])
  extracted.hash <- regmatches(x = vec[have.hash], m = hash.matches)
  
  df <- data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) <- c("tag","freq")
  df <- df[order(df$freq,decreasing = TRUE),]
  return(df)
}

dat <- head(extract.hashes(vec1),10)
dat2 <- transform(dat,tag = reorder(tag,freq))

try <- data.frame(dat2)

library(ggplot2)

p = ggplot(dat2, aes(x = tag, y = freq, count))  + geom_bar(stat = 'identity') 
p + coord_flip() + labs(title = "Hashtag frequencies in the tweets of Dodge Data")

## Construct Connect Frequency

vec1C <- CC$text

extract.hashes <- function(vec){
  
  hash.pattern <- "#[[:alpha:]]+"
  have.hash <- grep(x = vec, pattern = hash.pattern)
  
  hash.matches <- gregexpr(pattern = hash.pattern,
                           text = vec[have.hash])
  extracted.hash <- regmatches(x = vec[have.hash], m = hash.matches)
  
  df <- data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) <- c("tag","freq")
  df <- df[order(df$freq,decreasing = TRUE),]
  return(df)
}

dat <- head(extract.hashes(vec1C),10)
dat2 <- transform(dat,tag = reorder(tag,freq))


library(ggplot2)

p = ggplot(dat2, aes(x = tag, y = freq, count))  + geom_bar(stat = 'identity') 
p + coord_flip() + labs(title = "Hashtag frequencies in the tweets of Construct Connect")

################################# Total Tweets per Month #################################

#Subset to ensure only 2017 data

Master.sub <- subset(Master, Master$Date > "2017-01-01" & Master$Date < "2017-12-31")

names(Master.sub)

#Plot

ggplot(Master.sub, aes(x = MonthAbb, fill = Master.sub$Company)) +
  geom_bar(position = "identity", bins = 12, show.legend = FALSE) +
  facet_wrap(~Company, ncol = 1) + scale_x_discrete(limits = month.abb)

################################# Favorited Tweets by Month #################################

##Dodge Favorited Tweets by Month
# Subset to Dodge

DDA.Fav <- subset(Master, Company == "DDA")
DDA.Fav <- aggregate(favorite_count ~ MonthAbb, data = DDA.Fav, sum)

# Plot for Dodge

ggplot(DDA.Fav, aes(x = MonthAbb, y = favorite_count)) + 
  geom_bar(stat = "identity", bins = 12, show.legend = FALSE) +
  scale_x_discrete(limits = month.abb)

##Construct Connect Favorited Tweets by Month
# Subset to Construct Connect

CC.Fav <- subset(Master, Company == "CMD")
CC.Fav <- aggregate(favorite_count ~ MonthAbb, data = CC.Fav, sum)

describe(CC.Fav)
# Plot for Construct Connect 

ggplot(CC.Fav, aes(x = MonthAbb, y = favorite_count)) + 
  geom_bar(stat = "identity", bins = 12, show.legend = FALSE) +
  scale_x_discrete(limits = month.abb)

#################################  Retweets by Month #################################

##Dodge Retweets Tweets by Month
# Subset to Dodge

DDA.RTW <- subset(Master, Company == "DDA")
DDA.RTW <- aggregate(DDA.RTW$retweet_count ~ DDA.RTW$MonthAbb, data = DDA.RTW, sum)

#rename column
colnames(DDA.RTW) <- "MonthAbb"
colnames(DDA.RTW)[2] <- "retweet_count"

# Plot for Dodge

ggplot(DDA.RTW, aes(x = MonthAbb, y = retweet_count)) + 
  geom_bar(stat = "identity", bins = 12, show.legend = FALSE) +
  scale_x_discrete(limits = month.abb)

##Construct Connect Retweets Tweets by Month
# Subset to Construct Connect

CC.RTW <- subset(Master, Company == "CMD")
CC.RTW <- aggregate(CC.RTW$retweet_count ~ CC.RTW$MonthAbb, data = CC.RTW, sum)

colnames(CC.RTW) <- "MonthAbb"
colnames(CC.RTW)[2] <- "retweet_count"

describe(CC.RTW)
# Plot for Construct Connect

ggplot(CC.RTW, aes(x = MonthAbb, y = retweet_count)) + 
  geom_bar(stat = "identity", bins = 12, show.legend = FALSE) +
  scale_x_discrete(limits = month.abb)

################################# References #################################

#https://www.youtube.com/watch?v=qWmMKmPVtgk


