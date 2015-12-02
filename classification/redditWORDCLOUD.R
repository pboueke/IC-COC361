#install.packages('tm')

library('tm')

#set working directory
setwd("/home/jawa/Downloads/d/")

#Loading data
#(Using '''sed -i 1i"column1,...,columnN" file.csv''' 
#will add a new first line for the csv file)
rd <- read.csv("cyberpunk_ups_body.csv", stringsAsFactors = FALSE)

#View table
#View(rd)

#Combining all text together
alltext <- paste(rd$body, collapse=" ")

#setting up source and corpus
rd_source <- VectorSource(alltext)
corpus <- Corpus(rd_source)

#cleaning
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
#corpus <- tm_map(corpus, removeWords, stopwords('SMART'))
dat <- readLines("/home/jawa/Documents/studies/IC/reddit/stop.txt")
corpus <- tm_map(corpus, removeWords, dat)

#Making a document-term matrix
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)

#Finding the most frequent terms
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)

library(wordcloud)

words <- names(frequency)

wordcloud(words[1:100], frequency[1:100])

