library(SnowballC)
library(tm)
library(tokenizers)
library(plyr)

library(DataComputing)

data <- read.csv("/Users/Steven_Tom/Downloads/cleaned_data.csv")

#Changing "1" to filter data based on sender's number 
data1 <- data %>%filter(. == "1") #for selecting sender 1

data_corpus <- Corpus(VectorSource(data1[,2]))

tdm <- TermDocumentMatrix(data_corpus, control=list(wordLengths=c(1, Inf)))
tdm

(freq.terms <- findFreqTerms(tdm, lowfreq =10))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq>=10)
df <- data.frame(term=names(term.freq), freq=term.freq)

m <- as.matrix(tdm)
word.freq <- sort(rowSums(m), decreasing=T)
library(RColorBrewer)
pal <- brewer.pal(9, "BuGn")[-(1:4)]
library(wordcloud)
wordcloud(words=names(word.freq), freq=word.freq, min.freq=3, random.order=F, colors=pal)
