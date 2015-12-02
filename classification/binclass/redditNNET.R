library('tm')
library('class')
library ('nnet')

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

#generating training data for KNN clasifier

last.col <- ncol(df) - 1
dtm.tr <- df[1 : (as.numeric(num.train["trains"])       +
                 as.numeric(num.train["thepiratebay"])
), 1:last.col]

class.tr <- df[1 : (as.numeric(num.train["trains"])       +
                    as.numeric(num.train["thepiratebay"])
), last.col + 1]

#generating test data

dtm.ts <- df[(as.numeric(num.train["trains"])             +
                    as.numeric(num.train["thepiratebay"])       +
                    1) : (as.numeric(num.train["trains"])       +
                            as.numeric(num.train["thepiratebay"]) +
                            as.numeric(num.test["trains"])        +
                            as.numeric(num.test["thepiratebay"])
                    ), 1:last.col]

class.ts <- df[(as.numeric(num.train["trains"])             +
                      as.numeric(num.train["thepiratebay"])       +
                      1) : (as.numeric(num.train["trains"])       +
                              as.numeric(num.train["thepiratebay"]) +
                              as.numeric(num.test["trains"])        +
                              as.numeric(num.test["thepiratebay"])
                      ), last.col + 1]


#nnet.classifier <- nnet(dtm.tr, class.ind(class.tr),  size=2, rang=0.1, decay=5e-4, maxit=200)
nnet.classifier <- nnet(class.tr~., data=cbind(dtm.tr,class.tr),  size=2, rang=0.1, decay=5e-4, maxit=200)

predictions <- predict(nnet.classifier, dtm.ts, type="class")

conf.mx <- table(class.ts, predictions)
conf.mx 

