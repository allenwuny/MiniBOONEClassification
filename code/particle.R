library(readr)
library(glmnet)
library(tidyverse)

#data <- read_table2("C:/Users/Allen/Downloads/MiniBooNE.txt", col_names = FALSE, skip = 1)
#data <- read_table2("https://archive.ics.uci.edu/ml/machine-learning-databases/00199/MiniBooNE_PID.txt", col_names = FALSE, skip = 1)

#These numbers can be found in the data file in the first line. They were excluded from the read_table2 line
elec_num = 36499
muon_num = 93565
n = elec_num + muon_num

#Identify bad data/outliers where the values are -999
bad_data = which(abs(data$X20) >= 500)
bad_elec = table(bad_data <= elec_num)[2]
bad_muon = table(bad_data <= elec_num)[1]

#Remove outliers and bad data
data = data[-bad_data,]


#Generate the response vector assigning 1 to electron neutrino signals and 0 to muon neutrino signals
label = c(replicate(elec_num - bad_elec, 1), replicate(muon_num - bad_muon, 0))


for (i in c(1:1)){

  #Shuffle the predictors and labels
  set.seed(i)
  shuffle = sample(nrow(data))
  data = data[shuffle,]
  label = label[shuffle]
  
  #Split the dataframe into train and test
  train_size = round(n*0.9,0)
  test_size = n-train_size
  x_train = data.matrix(data[1:train_size,])
  y_train = as.numeric(label[1:train_size])
  x_test = data.matrix(data[(train_size+1):n,])
  y_test = as.numeric(label[(train_size+1):n])
  
  #Create a weight vector based on the number of electron neutrino signals in the training dataframe
  weight = sum(y_train)/length(y_train)
  w = sapply(y_train, function(i){x = if (i==1) 1 else weight})
  
  
  #Run cross validation on the training dataset to find optimal lambda
  cv_start = proc.time()
  cv_fit = cv.glmnet(x_train, y_train, family="binomial", type.measure="class")
  cv_time = proc.time() - cv_start
  
  #Use the minimum lambda from cross validation to run elastic net logistic regression
  elnet_start = proc.time()
  elnet_fit = glmnet(x_train, y_train, family = "binomial", lambda = cv_fit$lambda[which.min(cv_fit$lambda)], 
               maxit = 1000000, weights = w, alpha = 0.5, type.measure="class", standardize = FALSE)
  elnet_time = proc.time() - elnet_start
  
  #Use the minimum lambda from cross validation to run lasso logistic regression
  lasso_start = proc.time()
  lasso_fit = glmnet(x_train, y_train, family = "binomial", lambda = cv_fit$lambda[which.min(cv_fit$lambda)], 
                  maxit = 1000000, weights = w, alpha = 1, type.measure="class", standardize = FALSE)
  lasso_time = proc.time() - lasso_start
  
  #Use the minimum lambda from cross validation to run ridge logistic regression
  ridge_start = proc.time()
  ridge_fit = glmnet(x_train, y_train, family = "binomial", lambda = cv_fit$lambda[which.min(cv_fit$lambda)], 
                  maxit = 1000000, weights = w, alpha = 0, type.measure="class", standardize = FALSE)
  ridge_time = proc.time() - ridge_start
  
  beta0.hat = fit$a0
  beta.hat = as.vector(fit$beta)
  
  thrs = 0.5
  
  prob_train = exp(x_train %*% beta.hat + beta0.hat )/(1 + exp(x_train %*% beta.hat + beta0.hat))
  y_hat_train = ifelse(prob_train > thrs, 1, 0) #table(y.hat.train, y.train)
  FP_train                =        sum(y_train[y_hat_train==1] == 0) # false positives = negatives in the data that were predicted as positive
  TP_train                =        sum(y_hat_train[y_train==1] == 1) # true positives = positives in the data that were predicted as positive
  P_train                 =        sum(y_train==1) # total positives in the data
  N_train                 =        sum(y_train==0) # total negatives in the data
  FPR_train               =        FP_train/N_train # false positive rate = type 1 error = 1 - specificity
  TPR_train               =        TP_train/P_train # true positive rate = 1 - type 2 error = sensitivity
  typeI_err_train         =        FPR_train
  typeII_err_train        =        1 - TPR_train
  
  print(paste( "train: err        = ", sprintf("%.2f" , mean(y_train != y_hat_train))))
  print(paste( "train: typeI.err  = ", sprintf("%.2f" , typeI_err_train)))
  print(paste( "train: typeII.err = ", sprintf("%.2f" , typeII_err_train)))
  
  plot1 = data.frame(cbind(prob_train, y_train))
  colnames(plot1) = c("dist", "label")
  plot1$label = as.character(plot1$label)
  
  ggplot(plot1, aes(dist, fill=label)) + geom_histogram(alpha=0.5, position ="identity", bins=200) + labs(x="Probability", y="Count") + 
    scale_fill_discrete(name = "Source", labels = c("Electron", "Muon"))
  
  
  
  prob_test = exp(x_test %*% beta.hat + beta0.hat )/(1 + exp(x_test %*% beta.hat + beta0.hat))
  y_hat_test = ifelse(prob_test > thrs, 1, 0) #table(y.hat.train, y.train)
  FP_test                =        sum(y_test[y_hat_test==1] == 0) # false positives = negatives in the data that were predicted as positive
  TP_test                =        sum(y_hat_test[y_test==1] == 1) # true positives = positives in the data that were predicted as positive
  P_test                 =        sum(y_test==1) # total positives in the data
  N_test                 =        sum(y_test==0) # total negatives in the data
  FPR_test               =        FP_test/N_test # false positive rate = type 1 error = 1 - specificity
  TPR_test               =        TP_test/P_test # true positive rate = 1 - type 2 error = sensitivity
  typeI_err_test         =        FPR_test
  typeII_err_test        =        1 - TPR_test
  
  print(paste( "test: err        = ", sprintf("%.2f" , mean(y_test != y_hat_test))))
  print(paste( "test: typeI.err  = ", sprintf("%.2f" , typeI_err_test)))
  print(paste( "test: typeII.err = ", sprintf("%.2f" , typeII_err_test)))
  
  plot2 = data.frame(cbind(prob_test, y_test))
  colnames(plot2) = c("dist", "label")
  plot2$label = as.character(plot2$label)
  
  ggplot(plot2, aes(dist, fill=label)) + geom_histogram(alpha=0.5, position ="identity", bins=200) + labs(x="Probability", y="Count") + 
    scale_fill_discrete(name = "Source", labels = c("Electron", "Muon"))

}
