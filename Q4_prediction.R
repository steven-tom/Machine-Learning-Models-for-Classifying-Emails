h_test <- read.table("~/Downloads/HRC_test.tsv", header=FALSE, stringsAsFactors = FALSE) %>% 
  as.data.frame()

df_word <- read.csv("~/Desktop/wordfeature1.csv", header = T, stringsAsFactors = T) %>% 
  .[,-c(1,2)]

pow_test <- read.csv("~/Desktop/power_pre.csv") %>% 
  .[,-c(1,11,12,15,16,21,22)]

words <- c("unclassified","us","department","state","case","doc",
           "date","dept","subject","sent","am","pm","re","fw",letters[1:26], 
           "monday", "tuesday", "wednesday", "thursday",
           "friday", "saturday", "sunday", "today", "tomorrow","yesterday",
           "bb", "bbd", "das","will","release","can","also","now","just","get","like","let","according","full","said","message","part",
           "know","agreement","say","full","original","one","two")
more_word <-c("back","call","clinton","foreign","house","may","meeting","minister","new","november","president","secretary","see","time") 


# Change to lowercase, remove punctuations
data_corpus <- Corpus(DataframeSource(h_test))
data_corpus <- tm_map(data_corpus, content_transformer(tolower))
data_corpus <- tm_map(data_corpus, removePunctuation)
#data_corpus <- tm_map(data_corpus, removeNumbers)
data_corpus <- tm_map(data_corpus, removeWords, c(stopwords("en"),words,more_word))
# copy <- data_corpus
# data_corpus <- tm_map(data_corpus,stemCompletion,dictionary = copy)
data_corpus <- tm_map(data_corpus,stemDocument)
data_corpus <-tm_map(data_corpus,stripWhitespace)
data_corpus <- tm_map(data_corpus, PlainTextDocument)

tdm <- DocumentTermMatrix(data_corpus)
test_set <- as.matrix(tdm) %>% 
  as.data.frame()

word_test<- test_set[colnames(test_set) %in% colnames(df_word)]
not_in <- df_word[!(colnames(df_word)%in% colnames(word_test))]
m<- matrix(0,ncol =dim(not_in)[2],nrow=389) %>% 
  as.data.frame()
colnames(m)<-colnames(not_in)
word_test <- cbind(word_test,m)
word_test <- word_test[,order(names(word_test))]
word_test <- cbind(word_test,pow_test)


yhat_test <- predict(model,word_test,type="response")
write.table(as.numeric(yhat_test), "predict.txt", row.names = FALSE, col.names = FALSE)
