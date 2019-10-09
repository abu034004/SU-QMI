library(ROSE) 
library(e1071)
library(caret)
library(ROCR)
library(FSelector)
list_over<-c()
list_under<-c()
title<-paste0("Subset", ",", "Accuracy")
list_over<-c(list_over, title)
list_under<-c(list_under, title)


for(i in 1:30){
  file_n<-paste0("E:\\Research\\All ML works\\Graph Theory approach\\data\\combn\\bl_after_del_row_no_15\\Merged_bl_with non_AR_S_",i, ".csv")
  
  data <- read.csv(file_n, header = TRUE)
  
  data$Output <- as.factor(data$Output)
  
  set.seed(123)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  
  train <- data[ind==1,]
  
  k<-summary(train$Output)
  
  maxim<-0
  minim<-0
  if(k[1]>=k[2]){
    maxim=k[1]
    minim=k[2]
  } else {
    maxim =k[2]
    minim=k[1]
  }
  
  test <- data[ind==2,] 
  
  over <- ovun.sample(Output~., data = train, method = "over", N = 2*maxim)$data
  table(over$Output)
  
  set.seed(123)
  
  tmodel1<-tune(svm, Output~., data = over, ranges = list(epsilon =seq(0,1,0.1), cost=2^(2:7)))
  mymodel1<-tmodel1$best.model
  
  conf1<-confusionMatrix(predict(mymodel1, test), test$Output, positive = '1')
  acc<-conf1$overall[1]
  
  
  
  #add_over<-paste0(i, ",", conf1$table, ",",conf1$overall, ",",conf1$byClass,",", conf1_rel$table, ",",conf1_rel$overall, ",",conf1_rel$byClass, ",", conf1_mr$table, ",",conf1_mr$overall, ",",conf1_mr$byClass )
  add_over<-paste0(i, ",", round(acc,4))
  list_over<-c(list_over, add_over)
  
  
  
  under <- ovun.sample(Output~., data = train, method = "under", N = 2*minim)$data
  
  set.seed(123)
  
  tmodel2<-tune(svm, Output~., data = under, ranges = list(epsilon =seq(0,1,0.1), cost=2^(2:7)))
  mymodel2<-tmodel2$best.model
  
  conf2<-confusionMatrix(predict(mymodel2, test), test$Output, positive = '1')
  acc_under<-conf2$overall[1]
  
  
  add_under<-paste0(i, ",", round(acc_under,4)) 
  list_under<-c(list_under, add_under)
  
  
}


file_over<-paste0("E:\\Research\\All ML works\\Graph Theory approach\\data\\stats\\bl_after_del_row_no_15\\Over_statistics_svm_final_graph_latest",i, ".txt")
file_under<-paste0("E:\\Research\\All ML works\\Graph Theory approach\\data\\stats\\bl_after_del_row_no_15\\Under_statistics_svm_final_graph_latest",i, ".txt")
write.csv(list_over, file_over, row.names = FALSE)
write.csv(list_under, file_under, row.names = FALSE)































