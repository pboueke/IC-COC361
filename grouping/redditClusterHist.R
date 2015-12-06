library ('tm')
library ('class')
library ('akmeans')

setwd("/home/jawa/Downloads/d/erik-08-11.2/")

rd.sports <- read.csv('sports.csv', stringsAsFactors = FALSE, header = FALSE)

#each vector line into a document
tm <- Corpus(VectorSource(rd.sports$V4))

#cleaning
tm <- tm_map(tm, content_transformer(tolower))
tm <- tm_map(tm, removePunctuation)
tm <- tm_map(tm, stripWhitespace)
tm <- tm_map(tm, removeNumbers)
#tm <- tm_map(tm, removeWords, stopwords("portuguese"))
dat <- readLines("stop.txt")
tm<- tm_map(tm, removeWords, dat)
tm<- tm_map(tm, removeWords, c("deleted"))
rm(dat)

#dtm
dt <-DocumentTermMatrix(tm, control=list(weighing=weightTfIdf, minWordLength=2, minDocFreq=5))
dtm <- inspect(removeSparseTerms(dt, 0.997))
df <- as.data.frame( dtm )

groupsNumber <- 5

res <- norm.sim.ksc(dtm, groupsNumber)

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

for (i in 1:1)#groupsNumber) 
{
  groupMatrix[[i]] <- matrix(NA, ncol=ncol(df), nrow=length(clusters[[i]]))
  
  for (j in 1:length(clusters[[i]]))
  {
    groupMatrix[[i]][j,] <- as.numeric(df[clusters[[i]][j],])
  }
  dfMatrices[[i]] <- as.data.frame(groupMatrix[[i]])
  names(dfMatrices[[i]]) <- names(df)
}

frequency <- colSums(as.matrix(dfMatrices[[3]]))
frequency <-sort(frequency, decreasing = TRUE)

library(wordcloud)

words <- names(frequency)

wordcloud(words[50:100], frequency[50:100])



#write.table(df, file="df_sports.csv", sep=",", row.names=FALSE)
#write.table(res$cluster, file="terms_sports.csv", sep=",", row.names=FALSE, col.names=FALSE)
