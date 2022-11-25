library(data.table)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(partykit)
library(ROCR)
library(ggparty)
library(caret)
library(randomForest)
library(mice)


### data check

Data_checking <- function(x){
  data <- x %>% as.data.frame
  
  for (i in 1:ncol(data)) {
    print(c(names(data[i]),class(data[,i])))
  }
  
  num_of_na <- sum(is.na(data))
  print(paste0("omit number is ",num_of_na))
  
  # str(x)
  summary(x)  
}

### change factor

change.factor <- function(x){
  
  num <-length(character_data)
  for (i in num) {
    Summary_data[,character_data[i]] <- as.factor(Summary_data[,character_data[i]])
    print(c(names(Summary_data[,character_data[i]]),class(Summary_data[,character_data[i]])))
  }
  
}



### imputation

Mean.imputation <- function(x){
  
  print("make dataset and call")
  Train_data_imp <- Train_data
  head(Train_data_imp)
  Test_data_imp  <- Test_data
  head(Test_data_imp)
  
  Imputation <- mice(subset(Train_data_imp,select=select_data), method="mean", m=1, maxit=1)
  
  num <-length(select_data)
  for (i in num) {
    Train_data_imp[,select_data[i]] <- complete(Imputation)[,select_data[i]]
  }
  
  rownames(Train_data_imp) <- Train_data$Name
  
  Imputation.1 <- mice(subset(Test_data_imp,select=select_data), method="mean", m=1, maxit=1) 
  for (i in num) {
    Test_data_imp[,select_data[i]] <- complete(Imputation.1)[,select_data[i]]
  }
  
  rownames(Test_data_imp) <- Test_data$Name
  
  print("finish imputation and made Train_data_imp and Test_data_imp dataset ")
  
  
  
}

# test
call.function <- function(x){
  Train_data_imp <- Train_data
  head(Train_data_imp)
  Test_data_imp  <- Test_data
  head(Test_data_imp)
}

