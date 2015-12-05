library ('tm')
library ('class')
library ('nnet')
library ('akmeans')

setwd("/home/jawa/Downloads/d/erik-08-11.2/")

docs <- c('redditcyberpunkknn','redditbrasilknn','reddittrains','redditsad','redditthepiratebay')
num.train <- list(cyberpunk = 4812, brasil = 14510, trains = 803, sad = 190, thepiratebay = 471)
num.test <- list(cyberpunk = 2406, brasil = 7254, trains = 401, sad = 94, thepiratebay = 235)

rd.cyberpunk <- read.csv('redditcyberpunkknn.csv', stringsAsFactors = FALSE, header = FALSE)
rd.cyberpunk.train <- head(rd.cyberpunk, n = ceiling(nrow(rd.cyberpunk) * 2/3))
rd.cyberpunk.test <- tail(rd.cyberpunk, n = floor(nrow(rd.cyberpunk) * 1/3))
num.train['cyberpunk'] = ceiling(nrow(rd.cyberpunk) * 2/3)
num.test['cyberpunk'] = floor(nrow(rd.cyberpunk) * 1/3)

rd.brasil <- read.csv('redditbrasilknn.csv', stringsAsFactors = FALSE, header = FALSE)
rd.brasil.train <- head(rd.brasil, n = ceiling(nrow(rd.brasil) * 2/3))
rd.brasil.test <- tail(rd.brasil, n = floor(nrow(rd.brasil) * 1/3))
num.train['brasil'] = ceiling(nrow(rd.brasil) * 2/3)
num.test['brasil'] = floor(nrow(rd.brasil) * 1/3)

rd.trains <- read.csv('reddittrains.csv', stringsAsFactors = FALSE, header = FALSE)
rd.trains.train <- head(rd.trains, n = ceiling(nrow(rd.trains) * 2/3))
rd.trains.test <- tail(rd.trains, n = floor(nrow(rd.trains) * 1/3))
num.train['trains'] = ceiling(nrow(rd.trains) * 2/3)
num.test['trains'] = floor(nrow(rd.trains) * 1/3)

rd.sad <- read.csv('redditsad.csv', stringsAsFactors = FALSE, header = FALSE)
rd.sad.train <- head(rd.sad, n = ceiling(nrow(rd.sad) * 2/3))
rd.sad.test <- tail(rd.sad, n = floor(nrow(rd.sad) * 1/3))
num.train['sad'] = ceiling(nrow(rd.sad) * 2/3)
num.test['sad'] = floor(nrow(rd.sad) * 1/3)

rd.thepiratebay <- read.csv('redditthepiratebay.csv', stringsAsFactors = FALSE, header = FALSE)
rd.thepiratebay.train <- head(rd.thepiratebay, n = ceiling(nrow(rd.thepiratebay) * 2/3))
rd.thepiratebay.test <- tail(rd.thepiratebay, n = floor(nrow(rd.thepiratebay) * 1/3))
num.train['thepiratebay'] = ceiling(nrow(rd.thepiratebay) * 2/3)
num.test['thepiratebay'] = floor(nrow(rd.thepiratebay) * 1/3)
 
rm(rd.cyberpunk)
rm(rd.brasil)
rm(rd.trains)
rm(rd.sad)
rm(rd.thepiratebay)

#each vector line into a document
tm <- c(Corpus(VectorSource(rd.cyberpunk.train$V1)),
        Corpus(VectorSource(rd.brasil.train$V1)),
        Corpus(VectorSource(rd.trains.train$V1)),
        Corpus(VectorSource(rd.sad.train$V1)),
        Corpus(VectorSource(rd.thepiratebay.train$V1)),
        Corpus(VectorSource(rd.cyberpunk.test$V1)),
        Corpus(VectorSource(rd.brasil.test$V1)),
        Corpus(VectorSource(rd.trains.test$V1)),
        Corpus(VectorSource(rd.sad.test$V1)),
        Corpus(VectorSource(rd.thepiratebay.test$V1)))

#cleaning
tm <- tm_map(tm, content_transformer(tolower))
tm <- tm_map(tm, removePunctuation)
tm <- tm_map(tm, stripWhitespace)
tm <- tm_map(tm, removeNumbers)
tm <- tm_map(tm, removeWords, stopwords("portuguese"))
dat <- readLines("stop.txt")
tm<- tm_map(tm, removeWords, dat)
tm<- tm_map(tm, removeWords, c("deleted"))
rm(dat)

#dtm
dtm <-DocumentTermMatrix(tm, control=list(weighing=weightTfIdf, minWordLength=2, minDocFreq=5))

dtm <- inspect(removeSparseTerms(dtm, 0.995))

classHist <- TRUE
clusterHist <- TRUE
groupsNumber <- 5

res <- norm.sim.ksc(dtm, groupsNumber)


if(classHist)
{
  clusterCyberpunk <- c()
  clusterBrasil <- c()
  clusterTrains <- c()
  clusterSad <- c()
  clusterPiratebay <- c()
  
  
  for (i in 1:as.numeric(num.train['cyberpunk']))
  {
    clusterCyberpunk[i] <- res$cluster[[i]]
  }
  
  sum <- as.numeric(num.train['cyberpunk'])
  
  for (i in 1:as.numeric(num.train['brasil']))
  {
    clusterBrasil[i] <- head(res$cluster, n = sum)[[i]]
  }
  
  sum <- sum + as.numeric(num.train['brasil'])
  
  for (i in 1:as.numeric(num.train['trains']))
  {
    clusterTrains[i] <- head(res$cluster, n = sum)[[i]]
  }
  
  sum <- sum + as.numeric(num.train['trains'])
  
  for (i in 1:as.numeric(num.train['sad']))
  {
    clusterSad[i] <- head(res$cluster, n = sum)[[i]]
  }
  
  sum <- sum + as.numeric(num.train['sad'])
  
  for (i in 1:as.numeric(num.train['thepiratebay']))
  {
    clusterPiratebay[i] <- head(res$cluster, n = sum)[[i]]
  }
  
  hist(
    clusterCyberpunk
    )
  
  rm(clusterCyberpunk)
  rm(clusterBrasil)
  rm(clusterTrains)
  rm(clusterSad)
  rm(clusterPiratebay)
}

df <- as.data.frame( dtm )

#write.table(df, file="df.csv", sep=",", row.names=FALSE)
#write.table(res$cluster, file="terms.csv", sep=",", row.names=FALSE, col.names=FALSE)

if(clusterHist)
{
  clusters <- list()
  groups <- list()
  for(i in 1:groupsNumber)
  {
    clusters[[i]] <- c(1)
    groups[[i]] <- c(1)
    counter <- 0
    aux <- 0
    for(j in 1:length(res$cluster))
    {
      if(i == res$cluster[[j]])
      {
        clusters[[i]][counter] <- j
        sum = 0
        for (k in 1:length(num.train))
        {
          aux = k
          sum = sum + as.numeric(num.train[k])
          if (j < sum) {
            break
          }
        }
        groups[[i]][counter] = aux
        counter <- counter + 1
        }
    }  
  }
}


