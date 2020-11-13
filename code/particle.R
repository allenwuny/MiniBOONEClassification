library(readr)
library(glmnet)
library(tidyverse)
library(randomForest)

#data <- read_table2("C:/Users/Allen/Downloads/MiniBooNE.txt", col_names = FALSE, skip = 1)
#data <- read_table2("https://archive.ics.uci.edu/ml/machine-learning-databases/00199/MiniBooNE_PID.txt", col_names = FALSE, skip = 1)


#These numbers can be found in the data file in the first line. They were excluded from the read_table2 line
elec_num = 36499
muon_num = 93565
n = elec_num + muon_num


#Identify bad data/outliers
bad_data = which(abs(data$X20) >= 999)
bad_elec = table(bad_data <= elec_num)[2]
bad_muon = table(bad_data <= elec_num)[1]


#Remove outliers and bad data
data = data[-bad_data,]
n = n - bad_elec - bad_muon
train_size = round(n*0.9,0)
test_size = n-train_size


#Generate the response vector assigning 1 to electron neutrino signals and 0 to muon neutrino signals
label = c(replicate(elec_num - bad_elec, 1), replicate(muon_num - bad_muon, 0))


#Create empty dataframe to store error rates from each model
error = data.frame(pass=numeric(), model=character(), type=character(), rate=numeric())


#Function to return the threshold that yields the smallest difference between type I and II error from the training dataset
get_thrs = function(prob, y) {
  thrs_vector = c()
  
  for (i in seq(0, 1, by = 0.01)){
    
    y_hat = ifelse(prob > i, 1, 0) #table(y.hat.train, y.train)
    FP = sum(y[y_hat==1] == 0) # false positives = negatives in the data that were predicted as positive
    TP = sum(y_hat[y==1] == 1) # true positives = positives in the data that were predicted as positive
    P = sum(y==1) # total positives in the data
    N = sum(y==0) # total negatives in the data
    FPR = FP/N # false positive rate = type 1 error = 1 - specificity
    TPR = TP/P # true positive rate = 1 - type 2 error = sensitivity
    
    type1 = FPR
    type2 = 1 - TPR
    
    thrs_vector = c(thrs_vector, abs(type1 - type2))
  }
  
  return((which.min(thrs_vector) - 1)/100)
}


#Functions to calculate the train and test error rates based on the model
#Function returns overall error rate, type I error rate, type II error rate, and the result of the sigmoid function as a list in a vector
train_error_rate = function(fit){
  
  beta0.hat = fit$a0
  beta.hat = as.vector(fit$beta)
  prob = exp(x_train %*% beta.hat + beta0.hat )/(1 + exp(x_train %*% beta.hat + beta0.hat))
  
  train_thrs = get_thrs(prob, y_train)
  
  y_hat = ifelse(prob > train_thrs, 1, 0) #table(y.hat.train, y.train)
  FP = sum(y_train[y_hat==1] == 0) # false positives = negatives in the data that were predicted as positive
  TP = sum(y_hat[y_train==1] == 1) # true positives = positives in the data that were predicted as positive
  P = sum(y_train==1) # total positives in the data
  N = sum(y_train==0) # total negatives in the data
  FPR = FP/N # false positive rate = type 1 error = 1 - specificity
  TPR = TP/P # true positive rate = 1 - type 2 error = sensitivity
  
  typeI_err = FPR
  typeII_err = 1 - TPR
  overall_err = mean(y_train != y_hat)
  return(overall_err)
  #return(c(overall_err, typeI_err, typeII_err, list(prob)))
}


test_error_rate = function(fit){
  
  beta0.hat = fit$a0
  beta.hat = as.vector(fit$beta)
  prob = exp(x_test %*% beta.hat + beta0.hat )/(1 + exp(x_test %*% beta.hat + beta0.hat))

  test_thrs = get_thrs(prob, y_test)

  y_hat = ifelse(prob > test_thrs, 1, 0) #table(y.hat.train, y.train)
  FP = sum(y_test[y_hat==1] == 0) # false positives = negatives in the data that were predicted as positive
  TP = sum(y_hat[y_test==1] == 1) # true positives = positives in the data that were predicted as positive
  P = sum(y_test==1) # total positives in the data
  N = sum(y_test==0) # total negatives in the data
  FPR = FP/N # false positive rate = type 1 error = 1 - specificity
  TPR = TP/P # true positive rate = 1 - type 2 error = sensitivity
  
  typeI_err = FPR
  typeII_err = 1 - TPR
  overall_err = mean(y_test != y_hat)
  
  return(overall_err)
  #return(c(overall_err, typeI_err, typeII_err, list(prob)))
}

total_start = proc.time()

#Main for loop to run all the regression models
for (i in c(1:1)){

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
  weight = sum(y_train)/length(y_train)
  w = sapply(y_train, function(i){if (i==1) 1 else weight})
  
  
  #Run cross validation on the training dataset to find optimal lambda for elastic net
  cv_elnet_start = proc.time()
  cv_elnet_fit = cv.glmnet(x_train, y_train, family="binomial", type.measure="class")
  cv_elnet_time = proc.time() - cv_elnet_start
  elnet_lambda = cv_elnet_fit$lambda[which.min(cv_elnet_fit$lambda)]
  
  #Use the minimum lambda from cross validation to run elastic net logistic regression
  elnet_start = proc.time()
  elnet_fit = glmnet(x_train, y_train, family = "binomial", lambda = elnet_lambda, 
                     weights = w, alpha = 0.5, type.measure="class", standardize = FALSE)
  elnet_time = proc.time() - elnet_start
  
  #Run cross validation on the training dataset to find optimal lambda for lasso
  cv_lasso_start = proc.time()
  cv_lasso_fit = cv.glmnet(x_train, y_train, family="binomial", type.measure="class",
                           weights = w, alpha = 1, standardize = FALSE)
  cv_lasso_time = proc.time() - cv_lasso_start
  lasso_lambda = cv_lasso_fit$lambda[which.min(cv_lasso_fit$lambda)]
  
  #Use the minimum lambda from cross validation to run lasso logistic regression
  lasso_start = proc.time()
  lasso_fit = glmnet(x_train, y_train, family = "binomial", lambda = lasso_lambda,
                     weights = w, alpha = 1, type.measure="class", standardize = FALSE)
  lasso_time = proc.time() - lasso_start
  
  #Run cross validation on the training dataset to find optimal lambda for ridge
  cv_ridge_start = proc.time()
  cv_ridge_fit = cv.glmnet(x_train, y_train, family="binomial", type.measure="class",
                           weights = w, alpha = 0, standardize = FALSE)
  cv_ridge_time = proc.time() - cv_ridge_start
  ridge_lambda = cv_ridge_fit$lambda[which.min(cv_ridge_fit$lambda)]
  
  #Use the minimum lambda from cross validation to run ridge logistic regression
  ridge_start = proc.time()
  ridge_fit = glmnet(x_train, y_train, family = "binomial", lambda = ridge_lambda,
                     weights = w, alpha = 0, type.measure="class", standardize = FALSE)
  ridge_time = proc.time() - ridge_start
  
  
  #Append error rates of each model to the error df
  error = rbind(error, c(i, "Elastic Net","Train", train_error_rate(elnet_fit)[1]))
  error = rbind(error, c(i, "Elastic Net", "Test", test_error_rate(elnet_fit)[1]))
  error = rbind(error, c(i, "Lasso", "Train", train_error_rate(lasso_fit)[1]))
  error = rbind(error, c(i, "Lasso", "Test", test_error_rate(lasso_fit)[1]))
  error = rbind(error, c(i, "Ridge", "Train", train_error_rate(ridge_fit)[1]))
  error = rbind(error, c(i, "Ridge", "Test", test_error_rate(ridge_fit)[1]))
  
  
  #Gives an indication of how far the loop has progressed
  cat("Pass", i, "complete. ")
}

total_time = proc.time() - total_start

#Fix the column names for the error df
colnames(error) = c("pass", "model", "type", "rate")


#Generate boxplot of error rates
ggplot(error, aes(x=model, y=as.numeric(rate), fill = type)) + geom_boxplot()


#Generate histogram of beta coefficients
test = rbind(cbind(as.vector(elnet_fit$beta),rep("Elastic",50)), 
             cbind(as.vector(lasso_fit$beta),rep("Lasso",50)), 
             cbind(as.vector(ridge_fit$beta),rep("Ridge",50)))
test = data.frame(test)
ggplot(test) + aes(x=as.numeric(beta)) + geom_histogram(bins = 50) + facet_grid(model ~ .)
