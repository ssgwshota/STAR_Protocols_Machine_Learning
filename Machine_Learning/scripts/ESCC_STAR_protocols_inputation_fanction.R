

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
#Train_data_imp <- na.omit(Train_data_imp)



Imputation.1 <- mice(subset(Test_data_imp,select=select_data), method="mean", m=1, maxit=1) 
for (i in num) {
  Test_data_imp[,select_data[i]] <- complete(Imputation.1)[,select_data[i]]
}

rownames(Test_data_imp) <- Test_data$Name
#Test_data_imp <- na.omit(Test_data_imp)
print("finish imputation and made Train_data_imp and Test_data_imp dataset ")

