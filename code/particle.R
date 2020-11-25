rm(list = ls())    #delete objects
cat("\014")        #clear console

library(readr)
library(glmnet)
library(tidyverse)
library(randomForest)
library(DescTools)

data <- read_table2("C:/Users/Allen/Downloads/MiniBooNE.txt", col_names = FALSE, skip = 1)
#data <- read_table2("https://archive.ics.uci.edu/ml/machine-learning-databases/00199/MiniBooNE_PID.txt", col_names = FALSE, skip = 1)

start_time = proc.time()

#These numbers can be found in the data file in the first line. They were excluded from the read_table2 line
elec_num = 36499
muon_num = 93565
n = elec_num + muon_num


#Identify bad data/outliers
bad_data = which(abs(data$X20) == 999)
bad_elec = table(bad_data <= elec_num)[2]
bad_muon = table(bad_data <= elec_num)[1]


#Remove outliers and bad data
data = data[-bad_data,]
n = n - bad_elec - bad_muon
train_size = round(n*0.9,0)
test_size = n-train_size
p = dim(data)[2]

#Generate the response vector assigning 1 to electron neutrino signals and 0 to muon neutrino signals
label = c(replicate(elec_num - bad_elec, 1), replicate(muon_num - bad_muon, 0))

#Create an empty dataframe to store AUC values
auc_table = data.frame(matrix(ncol = 4, nrow = 0))
colnames(auc_table) = c("Round", "Model", "Dataset", "AUC")

get_auc = function (fit) { #function to create a dataframe of FPR and TPR for both train and test
  
  beta0.hat = fit$a0
  beta.hat = as.vector(fit$beta)
  prob.train = exp(x_train %*% beta.hat + beta0.hat)/(1 + exp(x_train %*% beta.hat + beta0.hat))
  prob.test = exp(x_test %*% beta.hat + beta0.hat)/(1 + exp(x_test %*% beta.hat + beta0.hat))
  
  roc = data.frame(matrix(ncol = 4, nrow = 0))
  for (i in seq(0, 1, by = 0.01)){
    roc.y.hat.train             =        ifelse(prob.train > i, 1, 0)
    roc.FP.train                =        sum(y_train[roc.y.hat.train==1] == 0) 
    roc.TP.train                =        sum(roc.y.hat.train[y_train==1] == 1) 
    roc.P.train                 =        sum(y_train==1) 
    roc.N.train                 =        sum(y_train==0) 
    roc.FPR.train               =        roc.FP.train/roc.N.train # false positive rate = type 1 error 
    roc.TPR.train               =        roc.TP.train/roc.P.train # true positive rate = 1 - type 2 error
    
    roc.y.hat.test              =        ifelse(prob.test > i,1,0) 
    roc.FP.test                 =        sum(y_test[roc.y.hat.test==1] == 0) 
    roc.TP.test                 =        sum(roc.y.hat.test[y_test==1] == 1) 
    roc.P.test                  =        sum(y_test==1)
    roc.N.test                  =        sum(y_test==0) 
    roc.TN.test                 =        sum(roc.y.hat.test[y_test==0] == 0)
    roc.FPR.test                =        roc.FP.test/roc.N.test # false positive rate = type 1 error 
    roc.TPR.test                =        roc.TP.test/roc.P.test # true positive rate = 1 - type 2 error
    
    roc = rbind(roc,c(roc.FPR.train, roc.TPR.train, roc.FPR.test, roc.TPR.test))
  }
  colnames(roc) = c("FPR_train", "TPR_train", "FPR_test", "TPR_test")
  auc_train = AUC(x=roc$FPR_train, y=roc$TPR_train)
  auc_test = AUC(x=roc$FPR_test, y=roc$TPR_test)
  return(c(auc_train, auc_test))
}

total_start = proc.time()

#Main for loop to run all the regression models using 0.9 train / 0.1 test
for (i in c(1:50)){

  #Shuffle the predictors and labels
  set.seed(i)
  shuffle = sample(nrow(data))
  data = data[shuffle,]
  label = label[shuffle]
  
  
  #Split the dataframe into train and test
  x_train = data.matrix(data[1:train_size,])
  y_train = as.numeric(label[1:train_size])
  x_test = data.matrix(data[(train_size+1):n,])
  y_test = as.numeric(label[(train_size+1):n])
  
  
  #Create a weight vector based on the number of electron neutrino signals in the training dataframe
  # weight = sum(y_train)/length(y_train)
  # w = sapply(y_train, function(i){if (i==1) 1 else weight})
  
  
  #Run cross validation on the training dataset to find optimal lambda for elastic net
  cv_elnet_fit = cv.glmnet(x_train, y_train, family="binomial", 
                           alpha=0.5, type.measure="auc")
  
  #Use the lambda with minimum error from CV to run elastic net logistic regression
  elnet_fit = glmnet(x_train, y_train, family = "binomial", 
                     lambda = cv_elnet_fit$lambda.min, alpha = 0.5)

  auc_table = rbind(auc_table, c(i, "Elastic Net", 
                                 "Train", get_auc(elnet_fit)[1])) 
  auc_table = rbind(auc_table, c(i, "Elastic Net", 
                                 "Test", get_auc(elnet_fit)[2]))
  
  #Run cross validation on the training dataset to find optimal lambda for lasso
  cv_lasso_fit = cv.glmnet(x_train, y_train, family="binomial", 
                           alpha = 1,  type.measure="auc")
  
  #Use the lambda with minimum from CV to run lasso logistic regression
  lasso_fit = glmnet(x_train, y_train, family = "binomial", 
                     lambda = cv_lasso_fit$lambda.min, alpha = 1)

  auc_table = rbind(auc_table, c(i, "Lasso", 
                                 "Train", get_auc(lasso_fit)[1])) 
  auc_table = rbind(auc_table, c(i, "Lasso", 
                                 "Test", get_auc(lasso_fit)[2]))
  
  #Run cross validation on the training dataset to find optimal lambda for ridge
  cv_ridge_fit = cv.glmnet(x_train, y_train, family="binomial", 
                           alpha = 0, type.measure="auc")
  
  #Use the lambda with minimum error from CV to run ridge logistic regression
  ridge_fit = glmnet(x_train, y_train, family = "binomial", 
                     lambda = cv_ridge_fit$lambda.min, alpha = 0)

  auc_table = rbind(auc_table, c(i, "Ridge", 
                                 "Train", get_auc(ridge_fit)[1])) 
  auc_table = rbind(auc_table, c(i, "Ridge", 
                                 "Test", get_auc(ridge_fit)[2]))
  
  
  dat = data.frame(x=x_train, y = as.factor(y_train))
  rf.fit     =    randomForest(y~., data = dat, mtry = sqrt(p))
  y.hat      =    predict(rf.fit, dat)
  
  print(table(y.hat,y_train))
  print(mean(y.hat==y_train))
  print(mean(y.hat[y_train=="1"]==y_train[y_train=="1"]))
  print( mean(y.hat[y_train=="0"]==y_train[y_train=="0"]))
  
  #Gives an indication of how far the loop has progressed
  cat("Round", i, "complete. ")
}


#Fix the column names for the error df
# colnames(error) = c("pass", "model", "type", "rate")
colnames(auc_table) =  c("Round", "Model", "Dataset", "AUC")


data = data.matrix(data)
label = as.numeric(label)


#Run cross validation on the entire dataset to find optimal lambda for elastic net
cv_elnet_start = proc.time()
cv_elnet_fit = cv.glmnet(data, label, family="binomial", 
                         alpha=0.5, type.measure="auc")

#Use the minimum lambda from cross validation to run elastic net logistic regression
elnet_fit = glmnet(data, label, family = "binomial", 
                   lambda = cv_elnet_fit$lambda.min, alpha = 0.5)
elnet_time = proc.time() - cv_elnet_start


#Run cross validation on the entire dataset to find optimal lambda for lasso
cv_lasso_start = proc.time()
cv_lasso_fit = cv.glmnet(data, label, family="binomial", 
                         alpha = 1,  type.measure="auc")

#Use the minimum lambda from cross validation to run lasso logistic regression
lasso_fit = glmnet(data, label, family = "binomial", 
                   lambda = cv_lasso_fit$lambda.min, alpha = 1)
lasso_time = proc.time() - cv_lasso_start


#Run cross validation on the entire dataset to find optimal lambda for ridge
cv_ridge_start = proc.time()
cv_ridge_fit = cv.glmnet(data, label, family="binomial", 
                         alpha = 0, type.measure="auc")

#Use the minimum lambda from cross validation to run ridge logistic regression
ridge_fit = glmnet(data, label, family = "binomial", 
                   lambda = cv_ridge_fit$lambda.min, alpha = 0)
ridge_time = proc.time() - cv_ridge_start

#Generate boxplot of error rates
# ggplot(error, aes(x=model, y=as.numeric(rate), fill = type)) + geom_boxplot()
ggplot(auc_table) + 
  aes(x=Dataset, y=as.numeric(AUC)) + 
  geom_boxplot()

#Generate barplot of beta coefficients
el_beta = cbind(as.vector(elnet_fit$beta),rep("Elastic Net",50), seq(1,50,by=1))
ls_beta = cbind(as.vector(lasso_fit$beta),rep("Lasso",50), seq(1,50,by=1))
rd_beta = cbind(as.vector(ridge_fit$beta),rep("Ridge",50), seq(1,50,by=1))

el_beta = data.frame(el_beta)
ls_beta = data.frame(ls_beta)
rd_beta = data.frame(rd_beta)

colnames(el_beta) = c("beta", "model", "coef")
colnames(ls_beta) = c("beta", "model", "coef")
colnames(rd_beta) = c("beta", "model", "coef")

el_order = el_beta %>% arrange(as.numeric(beta)) %>% select(coef)
el_order = as.numeric(el_order[["coef"]])

el_beta = el_beta[el_order,]
ls_beta = ls_beta[el_order,]
rd_beta = rd_beta[el_order,]

beta_df = rbind(el_beta, ls_beta, rd_beta)

beta_df = beta_df %>% mutate(pos = rep(seq(1,50,by=1),3))


total_time = proc.time() - start_time

ggplot(beta_df) + 
  aes(x=as.numeric(pos), y = as.numeric(beta)) + 
  geom_col() + 
  facet_grid(model ~ .)

