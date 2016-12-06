# svm part ＃ around 30% 
train <-cbind(ytrain,train)
tune.out <- tune(svm, ytrain~., data =train, kernel="radial",
                 ranges = list(cost = c(0.1,1,5,10,100,1000),gamma = c(1e-5,5e-5,1e-4,5e-4,1e-3,1e-2)),scale = F)
summary(tune.out) # cost 100, gamma 0.001

yhat <- predict(tune.out$best.model,test,type="response")
confusion2 <- table(yhat,ytest)
error_rate2 <- 1-sum(diag(confusion))/sum(confusion)

# produce ROC PLOTS
library(ISLR)
library(ROCR)

train_svm <-cbind(ytrain,train)
out <- svm(ytrain~., data =train_svm, kernel="radial",
           cost = 100,gamma = 1e-5,scale = F,probability = TRUE)

pred.te=predict(out,test,probability = TRUE)

for ( i in 1:5){
  te <-attr(pred.te, "probabilities") %>% 
    .[,i] 
  la <- ifelse(ytest ==i,1,0)
  pred <- prediction(te,la)
  pref <- performance(pred,"tpr","fpr")
  plot(pref,col="black",lty=3,lwd=3)
  abline(a=0,b=1,lty="dashed",col="green")
}
