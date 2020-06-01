#Reading File
apple <- read.csv(file.choose(), header =T)

#Corpus
library(NLP) #for NLP package
library(tm) #for tex mining
corpus <- iconv(apple$text,  to='UTF-8', sub = "byte")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#Data Cleaning
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

#Specify Stopwords
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

#Remove URLs
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


