# Reading File
apple <- read.csv(file.choose(), header =T)

# Corpus
library(NLP) #for NLP package
library(tm) #for tex mining
corpus <- iconv(apple$text,  to='UTF-8', sub = "byte")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Data Cleaning starts
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

# Specify Stopwords
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

# Remove URLs
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))
cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'stocks', 
                   replacement = 'stock')

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

# Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

# Barplot
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,
        las = 2,
        col = "blue")

#Creating a Worldcloud
library(RColorBrewer)
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

letterCloud(w,
            word = "apple",
            size=1)

# Sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read file
apple <- read.csv(file.choose(), header = T)
tweets <- iconv(apple$text, to='UTF-8', sub = "byte")

# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]
get_nrc_sentiment('delay')

# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Apple Tweets')



