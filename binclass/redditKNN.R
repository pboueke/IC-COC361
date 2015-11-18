library('tm')
library('class')

setwd("/home/jawa/Downloads/d/erik-08-11.2/")

docs <- c('redditcyberpunk','redditbrasilknn','reddittrains','redditsad','redditthepiratebay')
num.train <- list(cyberpunk = 4812, brasil = 14510, trains = 803, sad = 190, thepiratebay = 471)
num.test <- list(cyberpunk = 2406, brasil = 7254, trains = 401, sad = 94, thepiratebay = 235)

rd.trains <- read.csv('reddittrains.csv', stringsAsFactors = FALSE, header = FALSE)
rd.trains.train <- head(rd.trains, n = ceiling(nrow(rd.trains) * 2/3))
rd.trains.test <- tail(rd.trains, n = floor(nrow(rd.trains) * 1/3))
num.train['trains'] = ceiling(nrow(rd.trains) * 2/3)
num.test['trains'] = floor(nrow(rd.trains) * 1/3)

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
tm <- c(Corpus(VectorSource(rd.trains.train$V1)),
        Corpus(VectorSource(rd.thepiratebay.train$V1)),
        Corpus(VectorSource(rd.trains.test$V1)),
        Corpus(VectorSource(rd.thepiratebay.test$V1)))

#cleaning
tm <- tm_map(tm, content_transformer(tolower))
tm <- tm_map(tm, removePunctuation)
tm <- tm_map(tm, stripWhitespace)
tm <- tm_map(tm, removeNumbers)
dat <- readLines("/home/jawa/Downloads/d/erik-08-11.2/stop.txt")
tm<- tm_map(tm, removeWords, dat)
tm<- tm_map(tm, removeWords, c("deleted"))
rm(dat)

#dtm
dtm <-DocumentTermMatrix(tm, control=list(weighing=weightTfIdf, minWordLength=2, minDocFreq=5))

#dtm into a dataframe and reducing sparsity
df <- as.data.frame( inspect(removeSparseTerms(dtm, 0.99)) )

#apending class info
class <- c(rep('trains', as.numeric(num.train['trains'])),
           rep('thepiratebay', as.numeric(num.train['thepiratebay'])),
           rep('trains', as.numeric(num.test['trains'])),
           rep('thepiratebay', as.numeric(num.test['thepiratebay']))
           )
class.tr <- c(rep('trains', as.numeric(num.train['trains'])),
              rep('thepiratebay', as.numeric(num.train['thepiratebay']))
           )
class.ts <- c(rep('trains', as.numeric(num.test['trains'])),
              rep('thepiratebay', as.numeric(num.test['thepiratebay']))
           )
df <- cbind(df, class)

#==**KNN**=====================================================
#generating training data for KNN clasifier
#test and training vectors must have the same size for k-nn

last.col <- ncol(df) - 1

require(imputation)

#for loop, without the for
it1 <- 1
it2 <- as.numeric(num.test["trains"])
it3 <- 1
it4 <- 0
knn.dtm.tr <- df[it1:it2, 1:last.col]
knn.class.tr <- df[it1:it2, last.col + 1]

it1 <- it1 + as.numeric(num.test["trains"])
it2 <- it2 + as.numeric(num.test["thepiratebay"])
it3 <- it3 + as.numeric(num.train["trains"])
it4 <- it4 + as.numeric(num.train["trains"]) + as.numeric(num.test["thepiratebay"])
knn.dtm.tr[it1:it2,] <- df[it3:it4, 1:last.col]
knn.class.tr[it1:it2] <- df[it3:it4, last.col + 1]

#generating test data

knn.dtm.ts <- df[(as.numeric(num.train["trains"])             +
                  as.numeric(num.train["thepiratebay"])       +
                  1) : (as.numeric(num.train["trains"])       +
                        as.numeric(num.train["thepiratebay"]) +
                        as.numeric(num.test["trains"])        +
                        as.numeric(num.test["thepiratebay"])
                  ), 1:last.col]

knn.class.ts <- df[(as.numeric(num.train["trains"])             +
                    as.numeric(num.train["thepiratebay"])       +
                    1) : (as.numeric(num.train["trains"])       +
                          as.numeric(num.train["thepiratebay"]) +
                          as.numeric(num.test["trains"])        +
                          as.numeric(num.test["thepiratebay"])
                      ), last.col + 1]

knn.dtm.tr2 <- knn.dtm.tr

#add noise
for (i in 1:length(knn.dtm.tr2)){
  for (j in 1:length(knn.dtm.tr2[[i]])){
    knn.dtm.tr2[[i]][[j]] <- knn.dtm.tr2[[i]][[j]] + runif(1,0,0.001)
  }
}


knn.pred = knn(train = knn.dtm.tr2, test = knn.dtm.ts, cl = knn.class.tr, k = 1, l = 0, prob = T, use.all = F)

mat.conf <-table(knn.pred, knn.class.ts)
mat.conf







