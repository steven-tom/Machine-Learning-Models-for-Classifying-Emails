# fit model 
library(tree)
df_word <- read.csv("~/Downloads/wordfeature.csv", header = T, stringsAsFactors = T) %>% 
  .[,-c(1,2)]

y= as.character(data$V1)
y= factor(y)
df = cbind(y,df_word)

set.seed(1)
ntest <- floor(nrow(df_word)*0.3) #0.7 proportion as test set
index <- sample(1:nrow(df_word),ntest) 
test <- df_word[index,]
ytest <- y[index]
train <- df_word[-index,]
ytrain <- y[-index]


model <- randomForest(ytrain~., data = train,ntree=500)#change ntree based on results below
yhat <- predict(model,test,type="response")

model

confusion <- table(yhat,ytest)
error_rate1 <- 1-sum(diag(confusion))/sum(confusion)
confusion


##### This will take 10 years to run 
ntree_list <- seq(10,310,by=50)
error_rate <- numeric(0)
for(n in ntree_list){
  model <- randomForest(ytrain~., data = train, ntree = n)
  yhat <- predict(model,test,type="response")
  confusion <- table(yhat,ytest)
  error_rate1 <- 1-sum(diag(confusion))/sum(confusion)
  error_rate <- c(error_rate,error_rate1)  
}
plot(ntree_list,error_rate,type = "l")
num <- which.min(error_rate)
ntree_list[num]
error_rate

#save the number of minimal tree for the error rate 
#then use that to make the random forest model -> trees
####



important <-  importance(model) #list of top words

varImpPlot(model) #Pretty chart of words


