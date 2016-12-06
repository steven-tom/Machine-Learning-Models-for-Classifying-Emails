library(SnowballC)
library(tm)
library(tidyr)
library(plyr)
library(dplyr)
library(randomForest)
library(MASS)
library(e1071)


data <- read.table("HRC_train.tsv", header=FALSE, stringsAsFactors = FALSE) %>% 
  as.data.frame() 


power_feature <- as.data.frame(matrix(rep(NA,nrow(data)*28),nrow=nrow(data)))
colnames(power_feature)<-c("am","pm","monday", "tuesday", "wednesday", "thursday",
               "friday", "saturday", "sunday", "today", "tomorrow","yesterday","sent","thx",
               "pls","fw","\\?","\\!","\\$","deal","libya","\\ssid\\s","special\\sassistant\\sto\\ssecretary",
               "mini\\sschedule","\\*en\\sroute","sent\\svia\\scingular\\sxpress\\smail\\swith\\sblackberry",
               "sent\\sfrom\\smy\\sverizon\\swireless\\sblackberry","copyright\\sthe\\sfinancial")

# NOTE: We need to somehow compare "!", "$" and "deal" and decide if this email contains ads.

match_am <-"[0-9][0-9]am"
for (i in 1:nrow(data)) {
  string <-data[i,2]
  gsub("\\s", "", string)
  power_feature[i,1] <-str_count(string,match_am)
}

match_pm <-"[0-9][0-9]pm"
for (i in 1:nrow(data)) {
  string <-data[i,2]
  gsub("\\s", "", string)
  power_feature[i,2] <-str_count(string,match_pm)
}

for (i in 3:28) {
  match<-colnames(power_feature)[i]
  for (j in 1:nrow(data)) {
    string <-data[j,2]
    power_feature[j,i] <-str_count(string,match)
  }
}
