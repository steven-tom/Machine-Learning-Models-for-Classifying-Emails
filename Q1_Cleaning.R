library(SnowballC)
library(tm)
library(tidyr)
library(plyr)
library(dplyr)
library(randomForest)
library(MASS)
library(e1071)
library(readr)

#install.packages("tm")
stopwords('en')



data <- read.table("~/Downloads/HRC_train.tsv", header=FALSE, stringsAsFactors = FALSE)%>%as.data.frame()

words <- c("unclassified","us","department","state","case","doc",
           "date","dept","subject","sent","am","pm","re","fw",letters[1:26], 
           "monday", "tuesday", "wednesday", "thursday",
           "friday", "saturday", "sunday", "today", "tomorrow","yesterday",
           "bb", "bbd", "das","will","release","can","also","now","just","get","like","let","according","full","said","message","part",
           "know","agreement","say","full","original","one","two")
more_word <-c("back","call","clinton","foreign","house","may","meeting","minister","new","november","president","secretary","see","time") 



## Question 1


# Change to lowercase, remove punctuations
data_corpus <- Corpus(DataframeSource(data.frame(data[,2])))
data_corpus <- tm_map(data_corpus, content_transformer(tolower))
data_corpus <- tm_map(data_corpus, removePunctuation)
#data_corpus <- tm_map(data_corpus, removeNumbers) # we actually need the numbers
data_corpus <- tm_map(data_corpus, removeWords, c(stopwords("en"),words,more_word))

data_corpus <- tm_map(data_corpus,stemDocument)
data_corpus <-tm_map(data_corpus,stripWhitespace)
data_corpus <- tm_map(data_corpus, PlainTextDocument)

tdm <- DocumentTermMatrix(data_corpus)
train <- as.matrix(tdm) %>% 
  as.data.frame()

freq <- colSums(as.matrix(tdm)) 
freq <- freq[freq>50]
word_feature <- train[,names(train) %in% names(freq)]

#Power feature for length of each emails
len_vector <- c()

for( i in 1:dim(word_feature)[1]){
  len_vector[i] <- sum(word_feature[i,-1 ])
}


word_feature <- cbind( scale(len_vector), word_feature)
colnames(word_feature)[1]<- "NormLen"

#Other power feature 
power_feat <- read_csv("~/Downloads/power_feature.csv")
power_feat <- power_feat[ ,-1 ]
word_feature <- cbind(power_feat, word_feature)

word_feature <- cbind(factor(data$V1),word_feature)

colnames(word_feature)[1]<- "sender"


write.csv(word_feature,"wordfeature.csv")

