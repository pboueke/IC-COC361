library('tm')

setwd("/home/jawa/Downloads/d/")

#documento csv com uma coluna entitulada 'body'
rd <- read.csv("cyberpunk_body.csv", stringsAsFactors = FALSE, header = FALSE)

#transforma cada linha do vetor (cada comentário) em um documento
cyber.train <- Corpus(VectorSource(rd$V1))

#limpeza
cyber.train <-tm_map(cyber.train, content_transformer(tolower))
cyber.train <- tm_map(cyber.train, removePunctuation)
cyber.train <- tm_map(cyber.train, stripWhitespace)
cyber.train <- tm_map(cyber.train, removeNumbers)
dat <- readLines("/home/jawa/Documents/studies/IC/reddit/stop.txt")
cyber.train <- tm_map(cyber.train, removeWords, 'cyberpunk')
cyber.train <- tm_map(cyber.train, removeWords, dat)
rm(dat)

#dtm
cyber.train.dtm <-DocumentTermMatrix(cyber.train, control=list(weighing=weightTfIdf, minWordLength=2, minDocFreq=5))

#gets most frequent terms
freqterms <- findFreqTerms(cyber.train.dtm, 100)

#transformando a dtm em um dataframe
cyber.train.frm <- as.data.frame(inspect( cyber.train.dtm ))


#study  below at https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
#reduz esparcidade 
dtms <- removeSparseTerms(dtm, 0.1)
#encontra distancia entre termos na dtm 
d <- dist(t(cyber.train.dtm), method="euclidian") 
#dendograma
fit <- hclust(d=d, method="ward.D")   
