library ('tm')
library ('class')
library ('akmeans')
library ('ggplot2')

setwd("/home/jawa/Downloads/d/erik-08-11.2/")

rd.sports <- read.csv('redditbrasil.csv', stringsAsFactors = FALSE, header = FALSE)

#each vector line into a document
tm <- Corpus(VectorSource(rd.sports$V1))

#cleaning
tm <- tm_map(tm, content_transformer(tolower))
tm <- tm_map(tm, removePunctuation)
tm <- tm_map(tm, stripWhitespace)
tm <- tm_map(tm, removeNumbers)
tm <- tm_map(tm, removeWords, stopwords("portuguese"))
dat <- readLines("stop_pt.txt")
tm<- tm_map(tm, removeWords, dat)
tm<- tm_map(tm, removeWords, c("deleted"))
rm(dat)
dat <- readLines("stop.txt")
tm<- tm_map(tm, removeWords, dat)
tm<- tm_map(tm, removeWords, c("deleted"))
rm(dat)

#dtm
dt <-DocumentTermMatrix(tm, control=list(weighing=weightTfIdf, minWordLength=2, minDocFreq=5))
dtm <- inspect(removeSparseTerms(dt, 0.995))
df <- as.data.frame( dtm )

groupsNumber <- 10

res <- norm.sim.ksc(dtm, groupsNumber, iter.max=10000)

clusters <- list()
for(i in 1:groupsNumber)
{
  clusters[[i]] <- c(1)
  counter <- 0
  for(j in 1:length(res$cluster))
  {
    if(i == res$cluster[[j]])
    {
      clusters[[i]][counter] <- as.numeric(j)
      counter <- counter + 1
      }
  }  
}

groupMatrix <- list()
dfMatrices <- list()
dtmS <- list()

for (i in 1:groupsNumber) 
{
  groupMatrix[[i]] <- matrix(NA, ncol=ncol(df), nrow=length(clusters[[i]]))
  
  for (j in 1:length(clusters[[i]]))
  {
    groupMatrix[[i]][j,] <- as.numeric(df[clusters[[i]][j],])
  }
  dfMatrices[[i]] <- as.data.frame(groupMatrix[[i]])
  names(dfMatrices[[i]]) <- names(df)
}

#change dfMatrices index to change the group
frequency <- colSums(as.matrix(dfMatrices[[10]]))
frequency <-sort(frequency, decreasing = TRUE)

words <- names(frequency)

# Wordcloud
#library(wordcloud)
#wordcloud(words[1:50], frequency[1:50])

qplot(y=frequency[1:10], x=words[1:10], xlab='Terms', ylab="Count", stat="identity", geom="bar")
#write.table(df, file="df_sports.csv", sep=",", row.names=FALSE)
#write.table(res$cluster, file="terms_sports.csv", sep=",", row.names=FALSE, col.names=FALSE)
