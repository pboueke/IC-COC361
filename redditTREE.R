library('tm')
library('class')
library('rpart')

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

rm(rd.cyberpunk)
rm(rd.brasil)
rm(rd.trains)
rm(rd.sad)
rm(rd.thepiratebay)

#cleaning
tm <- tm_map(tm, content_transformer(tolower))
tm <- tm_map(tm, removePunctuation)
tm <- tm_map(tm, stripWhitespace)
tm <- tm_map(tm, removeNumbers)
dat <- readLines("stop_pt.txt")
tm <- tm_map(tm, removeWords, dat)
rm(dat)
tm <- tm_map(tm, removeWords, stopwords("portuguese"))
dat <- readLines("stop.txt")
tm<- tm_map(tm, removeWords, dat)
tm<- tm_map(tm, removeWords, c("deleted"))
rm(dat)

#dtm
dtm <-DocumentTermMatrix(tm, control=list(weighing=weightTfIdf, minWordLength=2, minDocFreq=5))

freqterms40 <- findFreqTerms(dtm, 100)
freqterms40

#dtm into a dataframe and reducing sparsity
df <- as.data.frame( inspect(removeSparseTerms(dtm, 0.9985)) )

#apending class info
class <- c(rep('cyberpunk', as.numeric(num.train['cyberpunk'])),
           rep('brasil', as.numeric(num.train['brasil'])),
           rep('trains', as.numeric(num.train['trains'])),
           rep('sad', as.numeric(num.train['sad'])),
           rep('thepiratebay', as.numeric(num.train['thepiratebay'])),
           rep('cyberpunk', as.numeric(num.test['cyberpunk'])),
           rep('brasil', as.numeric(num.test['brasil'])),
           rep('trains', as.numeric(num.test['trains'])),
           rep('sad', as.numeric(num.test['sad'])),
           rep('thepiratebay', as.numeric(num.test['thepiratebay']))
           )
class.tr <- c(rep('cyberpunk', as.numeric(num.train['cyberpunk'])),
              rep('brasil', as.numeric(num.train['brasil'])),
              rep('trains', as.numeric(num.train['trains'])),
              rep('sad', as.numeric(num.train['sad'])),
              rep('thepiratebay', as.numeric(num.train['thepiratebay']))
           )
class.ts <- c(rep('cyberpunk', as.numeric(num.test['cyberpunk'])),
              rep('brasil', as.numeric(num.test['brasil'])),
              rep('trains', as.numeric(num.test['trains'])),
              rep('sad', as.numeric(num.test['sad'])),
              rep('thepiratebay', as.numeric(num.test['thepiratebay']))
           )
df <- cbind(df, class)

#==**KNN**=====================================================
#generating training data for KNN clasifier
#test and training vectors must have the same size

last.col <- ncol(df) - 1

dtm.tr <- df[1 : (as.numeric(num.train["cyberpunk"])    +
                  as.numeric(num.train["brasil"])       +
                  as.numeric(num.train["trains"])       +
                  as.numeric(num.train["sad"])          +
                  as.numeric(num.train["thepiratebay"])
            ), 1:last.col]

class.tr <- df[1 : (as.numeric(num.train["cyberpunk"])    +
                    as.numeric(num.train["brasil"])       +
                    as.numeric(num.train["trains"])       +
                    as.numeric(num.train["sad"])          +
                    as.numeric(num.train["thepiratebay"])
            ), last.col + 1]



#generating test data

dtm.ts <- df[(as.numeric(num.train["cyberpunk"])          +
                    as.numeric(num.train["brasil"])             +
                    as.numeric(num.train["trains"])             +
                    as.numeric(num.train["sad"])                +
                    as.numeric(num.train["thepiratebay"])       +
                    1) : (as.numeric(num.train["cyberpunk"])    +
                            as.numeric(num.train["brasil"])       +
                            as.numeric(num.train["trains"])       +
                            as.numeric(num.train["sad"])          +
                            as.numeric(num.train["thepiratebay"]) +
                            as.numeric(num.test["cyberpunk"])     +
                            as.numeric(num.test["brasil"])        + 
                            as.numeric(num.test["trains"])        +
                            as.numeric(num.test["sad"])           +
                            as.numeric(num.test["thepiratebay"])
                    ), 1:last.col]

class.ts <- df[(as.numeric(num.train["cyberpunk"])          +
                      as.numeric(num.train["brasil"])             +
                      as.numeric(num.train["trains"])             +
                      as.numeric(num.train["sad"])                +
                      as.numeric(num.train["thepiratebay"])       +
                      1) : (as.numeric(num.train["cyberpunk"])    +
                              as.numeric(num.train["brasil"])       +
                              as.numeric(num.train["trains"])       +
                              as.numeric(num.train["sad"])          +
                              as.numeric(num.train["thepiratebay"]) +
                              as.numeric(num.test["cyberpunk"])     +
                              as.numeric(num.test["brasil"])        + 
                              as.numeric(num.test["trains"])        +
                              as.numeric(num.test["sad"])           +
                              as.numeric(num.test["thepiratebay"])
                      ), last.col + 1]

dtree <- rpart(class.tr ~ algo + amtrak + better + bit + cara + cars + ciência + cyberpunk + day + feel + find ,method='class',data=dtm.tr)
print(dtree)



