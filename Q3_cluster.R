library(datasets)
choose_word <- model$importance %>% 
  as.data.frame()  
w<-choose_word %>% 
  rownames()   
od<-order(choose_word,decreasing = TRUE) %>% 
  .[1:100] 
top100<- w[od]

ktest <- train[,colnames(train)%in%top100]
km.out<-kmeans(ktest,5,nstart=20)
result <- km.out$cluster

# compare the true value with the predicted 
idx <- list()
truey <-list()
for (i in 1:5){
  idx[[i]] <- which(result ==i)
  truey[[i]] <-which(ytrain ==i)
}
