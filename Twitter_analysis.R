## Option 1: retrieve tweets from Twitter

library(twitteR)

library(twitteR)

tweets <- readRDS(file.choose())


n.tweet <- length(tweets)

# convert tweets to a data frame

tweets.df <- twListToDF(tweets)

# tweet #190

tweets.df[190, c("id", "created", "screenName", "replyToSN",
                 
                 "favoriteCount", "retweetCount", "longitude", "latitude", "text")]

# print tweet #190 and make text fit for slide width

writeLines(strwrap(tweets.df$text[190], 60))

library(tm)

# build a corpus, and specify the source to be character vectors

myCorpus <- Corpus(VectorSource(tweets.df$text))

# convert to lower case

myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# remove URLs

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

# remove anything other than English letters or space

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

# remove stopwords

myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),"use", "see", "used", "via", "amp")
                 
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
                 
# remove extra whitespace
                 
myCorpus <- tm_map(myCorpus, stripWhitespace)
                 
# keep a copy for stem completion later
                 
myCorpusCopy <- myCorpus

myCorpus <- tm_map(myCorpus, stemDocument) # stem words

writeLines(strwrap(myCorpus[[190]]$content, 60))

stemCompletion2 <- function(x, dictionary) {
  
  x <- unlist(strsplit(as.character(x), " "))
  
  x <- x[x != ""]
  
  x <- stemCompletion(x, dictionary=dictionary)
  
  x <- paste(x, sep="", collapse=" ")
  
  PlainTextDocument(stripWhitespace(x))
  
}

myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)

myCorpus <- Corpus(VectorSource(myCorpus))

writeLines(strwrap(myCorpus[[190]]$content, 60))

# count word frequence

wordFreq <- function(corpus, word) {
  
  results <- lapply(corpus,
                    
                    function(x) { grep(as.character(x), pattern=paste0("\\<",word)) }
                    
  )
  
  sum(unlist(results))
  
}

n.miner <- wordFreq(myCorpusCopy, "miner")

n.mining <- wordFreq(myCorpusCopy, "mining")

cat(n.miner, n.mining)


# replace oldword with newword

replaceWord <- function(corpus, oldword, newword) {
  
  tm_map(corpus, content_transformer(gsub),
         
         pattern=oldword, replacement=newword)
  
}

myCorpus <- replaceWord(myCorpus, "miner", "mining")

myCorpus <- replaceWord(myCorpus, "universidad", "university")

myCorpus <- replaceWord(myCorpus, "scienc", "science")

tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))

tdm

idx <- which(dimnames(tdm)$Terms %in% c("r", "data", "mining"))

as.matrix(tdm[idx, 21:30])


# inspect frequent words

(freq.terms <- findFreqTerms(tdm, lowfreq = 20))

term.freq <- rowSums(as.matrix(tdm))

term.freq <- subset(term.freq, term.freq >= 20)

df <- data.frame(term = names(term.freq), freq = term.freq)

library(ggplot2)

ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  
  xlab("Terms") + ylab("Count") + coord_flip() +
  
  theme(axis.text=element_text(size=7))


m <- as.matrix(tdm)

# calculate the frequency of words and sort it by frequency

word.freq <- sort(rowSums(m), decreasing = T)

# colors

pal <- brewer.pal(9, "BuGn")[-(1:4)]


# plot word cloud

library(wordcloud)

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,random.order = F, colors = "blue")

# which words are associated with 'r'?

findAssocs(tdm, "r", 0.2)

# which words are associated with 'data'?

findAssocs(tdm, "data", 0.2)


library(graph)

library(Rgraphviz)

source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)


dtm <- as.DocumentTermMatrix(tdm)

library(topicmodels)

lda <- LDA(dtm, k = 8) # find 8 topics

term <- terms(lda, 7) # first 7 terms of every topic

(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

topics <- topics(lda) # 1st topic identified for every document (tweet)

library(data.table)

topics <- data.frame(date=as.IDate(tweets.df$created), topic=topics)

qplot(date, ..count.., data=topics, geom="density",fill=term[topic], position="stack")


###################### SENTIMENT ANALYSIS #############################

# install package sentiment140

require(devtools)

install_github("sentiment140", "okugami79")

# sentiment analysis

library(sentiment)

sentiments <- sentiment(tweets.df$text)

table(sentiments$polarity)

# sentiment plot

sentiments$score <- 0

sentiments$score[sentiments$polarity == "positive"] <- 1

sentiments$score[sentiments$polarity == "negative"] <- -1

sentiments$date <- as.IDate(tweets.df$created)

result <- aggregate(score ~ date, data = sentiments, sum)

plot(result, type = "l")



######################## Follower Analysis #############################

#### Need a Token Authentican ###
library(twitteR)


setup_twitter_oauth(API_key,API_secret,access_token,access_token_secret)




user <- getUser("RDataMining")

user$toDataFrame()

friends <- user$getFriends() # who this user follows

followers <- user$getFollowers() # this user's followers

followers2 <- followers[[1]]$getFollowers() # a follower's followers

library(maps)
library(geosphere)


# Source the function
source("http://biostat.jhsph.edu/~jleek/code/twitterMap.R")

# Make your twittermap
twitterMap("vaibhavlaturkar",userLocation = "Mumbai")
twitterMap("RDataMining",plotType="both")




# select top retweeted tweets

table(tweets.df$retweetCount)

selected <- which(tweets.df$retweetCount >= 9)

# plot them

dates <- strptime(tweets.df$created, format="%Y-%m-%d")

plot(x=dates, y=tweets.df$retweetCount, type="l", col="grey",
     
     xlab="Date", ylab="Times retweeted")

colors <- rainbow(10)[1:length(selected)]

points(dates[selected], tweets.df$retweetCount[selected],
       
       pch=19, col=colors)

text(dates[selected], tweets.df$retweetCount[selected],
     
     tweets.df$text[selected], col=colors, cex=.9)



### Tracking Message Propagation
tweets[[1]]

retweeters(tweets[[1]]$id)

retweets(tweets[[1]]$id)
