# load packages 

library(readr)
library(gridExtra)
library(pROC)
library(reshape2)
library(glmnet)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(randomForest)
library(DescTools)
library(kableExtra)
library(plotly)
library(ggpubr)
library(rbenchmark)
library(caTools)
theme_set(theme_pubr())


# load data 
data <- read_table2("https://archive.ics.uci.edu/ml/machine-learning-databases/00199/MiniBooNE_PID.txt", col_names = FALSE, skip = 1)
data <- read_table2("C:/Users/Allen/Downloads/MiniBooNE.txt", col_names = FALSE, skip = 1)


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

# Factors  
data$label <- as.numeric(c(replicate(elec_num - bad_elec, 1), replicate(muon_num - bad_muon, 0)))


auc_table = data.frame(matrix(nrow = 50, ncol = 8))
colnames(auc_table) = c("Model", "Round", 
                        "L.Train.FPR", "L.Test.FPR", 
                        "R.Train.FPR", "R.Test.FPR",  
                        "E.Train.FPR", "E.Test.FPR")


starting <- function(plots = F) { 
  round = round + 1
  set.seed(round)
  
  #splitting data 10/90 ratio
  n_train <-as.integer(0.9*n)
  n_test = n - n_train
  idx <- sample.int(n, n_train)
  #mutually exclusive
  D_train <- data[idx, ]
  D_test <- data[-idx, ]
  # Create X/Y train/test
  X_train <- scale(select(D_train, -label)) %>% data.matrix()
  X_test <- scale(select(D_test, -label)) %>% data.matrix()
  y_train <-as.numeric(D_train$label)
  y_test <- as.numeric(D_test$label)
  
  
  #function to create a dataframe of FPR and TPR for both train and test
  roc_table = function(prob_train, prob_test) {
    
    roc = data.frame (matrix(ncol =4, nrow =0))
    for (i in seq(0, 1, by =0.01)) {
      roc.y.hat.train = ifelse(prob_train > i, 1, 0)
      roc.FP.train = sum(y_train[roc.y.hat.train ==1] ==0)
      roc.TP.train = sum(roc.y.hat.train[y_train==1]==1)
      roc.P.train = sum(y_train==1)
      roc.N.train   = sum(y_train ==0)
      roc.FPR.train = roc.FP.train/roc.N.train #FPR
      roc.TPR.train = roc.TP.train/roc.P.train #TPR
      
      roc.y.hat.test  = ifelse(prob_test >i, 1, 0 )
      roc.FP.test    = sum(y_test[roc.y.hat.test==1]==0)
      roc.TP.test     = sum(roc.y.hat.test[y_test==1] ==1)
      roc.P.test     = sum(y_test==1)
      roc.N.test = sum(y_test==0)
      roc.TN.test = sum(roc.y.hat.test[y_test==0] == 0)
      roc.FPR.test = roc.FP.test/roc.N.test # FPR = type 1 error
      roc.TPR.test = roc.TP.test/roc.P.test # TPR = 1 - type 2 error

      roc = rbind(roc,c(roc.FPR.train, roc.TPR.train, roc.FPR.test, roc.TPR.test))
    }
    colnames(roc) = c("FPR_train", "TPR_train", "FPR_test", "TPR_test")
    return(roc)
  }
  
  
  #function to plot the ROC curve 
  
  plot_roc = function(roc, penalty) { 
    
    auc_train = AUC(x=roc$FPR_train, y=roc$TPR_train)
    auc_test = AUC(x=roc$FPR_test, y=roc$TPR_test)
    
    ggplot(roc) + geom_line(aes(FPR_train, TPR_train, color = "Train")) + 
      geom_line(aes(FPR_test, TPR_test, color = "Test")) + 
      scale_color_manual(name = "Dataset", values=c("Train" = "steelblue","Test" = "darkred")) +
      labs(x="FPR", y="TPR") + ggtitle(paste(penalty,"ROC Curve")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      annotate("label", x=0.5, y=0.5, label = paste("Train AUC:", round(auc_train,3),
   "\nTest AUC:", round(auc_test,3))) }
  
 
  ##################################################################################
  #                        Lasso Regression                                     #
  ##################################################################################
  l.time.start <-Sys.time()  
  l.cv <- cv.glmnet(X_train, y_train, family ="binomial", type.measure="auc", alpha=1)
  l.fit <- glmnet(X_train, y_train, family="binomial", lambda =l.cv$lambda.min, alpha=1) 
  # fitting
  l.beta0.hat = as.vector(l.fit$a0)
  l.beta.hat = as.vector(l.fit$beta)
  #Probabilities
  l.prob_train = exp(X_train %*% l.beta.hat + l.beta0.hat )/(1+ exp(X_train %*% l.beta.hat + l.beta0.hat )) 
  l.prob_test = exp(X_test %*% l.beta.hat + l.beta0.hat )/(1 + exp(X_test %*% l.beta.hat + l.beta0.hat ))
  plot(l.cv, sub = paste("LASSO:", l.cv$lambda.min))
  # Compute ROC
  l.roc = roc_table(l.prob_train, l.prob_test)
  plot_roc(l.roc, "Lasso")
  # Store AUC 
  l.auc = c(round, 
            AUC(x=l.roc$FPR_train, y=l.roc$TPR_train), 
            AUC(x=l.roc$FPR_test, y=l.roc$TPR_test))
  l.time <- Sys.time() - l.time.start
 
  
  
  ##################################################################################
  #                        Ridge Regression                                     #
  ##################################################################################
  r.time.start <-Sys.time()   
  r.cv <- cv.glmnet(X_train, y_train, family ="binomial", type.measure="auc", alpha=0)
  r.fit <- glmnet(X_train, y_train, family="binomial", lambda = r.cv$lambda.min, alpha=0) 
  # fitting
  r.beta0.hat = as.vector(r.fit$a0)
  r.beta.hat = as.vector(r.fit$beta)
  #Probabilities
  r.prob_train = exp(X_train %*% r.beta.hat + r.beta0.hat )/(1+ exp(X_train %*% r.beta.hat + r.beta0.hat )) 
  r.prob_test = exp(X_test %*% r.beta.hat + r.beta0.hat )/(1 + exp(X_test %*% r.beta.hat + r.beta0.hat ))
  plot(r.cv, sub = paste("RIDGE:", r.cv$lambda.min))
  # Compute ROC
  r.roc = roc_table(r.prob_train, r.prob_test)
  plot_roc(r.roc, "Ridge")
  # Store AUC 
  r.auc = c(round, 
            AUC(x=r.roc$FPR_train, y=r.roc$TPR_train), 
            AUC(x=r.roc$FPR_test, y=r.roc$TPR_test))

  r.time <- Sys.time() - r.time.start
  
  
  ##################################################################################
  #                        Elastic Net Regression                                     #
  ##################################################################################
  # 
  e.time.start <-Sys.time()  
  e.cv <- cv.glmnet(X_train, y_train, family = "binomial", type.measure="auc", alpha=0.5)
  e.fit <- glmnet(X_train, y_train, family="binomial", lambda =e.cv$lambda.min, alpha=0.5) 
  # fitting
  e.beta0.hat = as.vector(e.fit$a0)
  e.beta.hat = as.vector(e.fit$beta)
  #Probabilities
  e.prob_train = exp(X_train %*% e.beta.hat + e.beta0.hat )/(1+ exp(X_train %*% e.beta.hat + e.beta0.hat )) 
  e.prob_test = exp(X_test %*% e.beta.hat + e.beta0.hat )/(1 + exp(X_test %*% e.beta.hat + e.beta0.hat ))
  plot(e.cv, sub = paste("ELASTIC-NET:", e.cv$lambda.min))
  # Compute ROC
  e.roc = roc_table(e.prob_train, e.prob_test)
  plot_roc(e.roc, "Ridge")
  # Store AUC 
  e.auc = c(round, 
            AUC(x=e.roc$FPR_train, y=e.roc$TPR_train), 
            AUC(x=e.roc$FPR_test, y=e.roc$TPR_test))
  
  e.time <- Sys.time() - e.time.start
  
  
  ##################################################################################
  #                         Random Forest  Classification                         #
  ##################################################################################
  # Random Forest
  rf_time_start <- Sys.time()
  classifier = randomForest(X_train, y_train, ntree = 10)
  # rf <- randomForest(label ~ . , X_train)
  # fiting
  rf.y_pred_train <- predict(classifier, X_train)
  rf.y_pred_test <- predict(classifier, X_test)
  rf.residual_train <- as.vector(y_train - rf.y_pred_train)
  rf.residual_test <- as.vector(y_test - rf.y_pred_test)
  # Difference
  delta  <- abs((1 - roc_rf$sensitivities) - (1 - roc_rf$specificties))
  # Best Threshold
  classifier$y
  roc_rf <- roc(y_train, classifier$y)
  theta_rf <- roc_rf$thresholds(which.min(delta))
  rf <- randomForest(y_train ~ ., X_train, cutoff=c(theta_rf, 1- theta_rf))
  # Compute ROC
  rf.roc.train <- roc(y_train, rf$votes[,2])
  rf.roc.test <- roc(y_test, rf$votes[,2])
  # Store AUC
  RF.AUC_TRAIN <- rf.roc.train$auc
  RF.AUC_TEST <- rf.roc.test$auc
  #plots
  plot(classifier)
  rf_time <- Sys.time() - rf_time_start

  if(plots) {
    ## 10 fold CV plots
    plot(l.cv, sub = paste("Lasso:", l.cv$lambda.min)) #lambda.min
    plot(r.cv, sub = paste("Ridge:", r.cv$lambda.min)) #lambda.min
    plot(e.cv, sub = paste("Ridge:", e.cv$lambda.min))

  }




  AUC_TRAIN <- list(lasso =L.AUC_TRAIN, ridge =R.AUC_TRAIN, elnet = E.AUC_TRAIN, rf = RF.AUC_TRAIN )
  AUC_TEST <- list(lasso =L.AUC_TEST, ridge =R.AUC_TEST, elnet = E.AUC_TEST, rf = RF.AUC_TEST)


  times <- list(lasso = l.time, ridge = r.time,
                elnet = e.time, rf = rf.time)
  
  aucs = matrix(cbind(l.auc, r.auc, e.auc), ncol=3)
  
  return(aucs)
  #return(list(AUC_TRAIN, AUC_TEST, times))
}

just_one <- starting(plots = T)

###################################################################################
#
#################################################################################


round = 0
M <- 2
models <- replicate(M, unlist(starting(plots = F))) %>% 
  data.frame()%>% 
  t()%>% 
  data.frame()%>% 
  mutate(Model = rep(c("Lasso", "Ridge", "Elastic Net"), M))


#AUC_TRAIN <- models[, 1:4] %>% data.frame() %>% mutate(dataset="test")
#AUC_TEST <- models[, 5:8] %>% data.frame() %>% mutate(dataset="train")
#time_models <- models[, 9:12] %>% data.frame() 
# auc_data <- rbind(AUC_TRAIN, AUC_TEST) %>% gather(model, auc, lasso:rf)
# auc_plot <- auc_data %>% mutate(dataset = factor(dataset, levels=c("train", "test"))) %>%
#   ggplot(aes(x=model, y=AUC)) + geom_boxplot(aes(fill=model)) + facet_wrap(~dataset, scales = "free_y") + labels + 
#   labs(x = "Model", y = "AUC") 
# 
# auc_plot <- ggplotly(auc_plot)
# 
# auc_plot



time_plot <- time_models %>%
  gather(model, time, lasso:rf) %>%
  ggplot(aes(x=model, y=time, fill=model)) +
  geom_boxplot()
#time_plot <- ggplotly(time_plot) # Un-comment for ggplotly plot
time_plot
avg_model_times <- time_models %>%
  gather(model, time, lasso:rf) %>%
  group_by(model) %>%
  summarize(mean=mean(time), median=median(time), min=min(time), max=max(time))
kable(avg_model_times, digits=2)







All <- function(plots = F) { 
  y <-as.numeric(data$label)
  X <- data[,1:50] %>% data.matrix()
  ##################################################################################
  #                        Lasso Regression                                     #
  ##################################################################################
  
  l.cv<- cv.glmnet(X, y, family="binomial", alpha=1)
  l.fit <- glmnet(X, y, family="binomial", lambda = l.cv$lambda.min)

  ##################################################################################
  #                        Ridge Regression                                     #
  ##################################################################################
  
  ridge <- cv.glmnet(X, y, family = "binomial", alpha =0)
  r.fit <- glmnet(X, y, family = "binomial", lambda = ridge$lambda.min,)
  
  ##################################################################################
  #                        Elastic Net Regression                                     #
  ##################################################################################
  elastic <- cv.glmnet(X, y, family = "binomial", alpha=0.5)
  e.fit <- glmnet(X, y, family="binomial", lambda =elastic$lambda.min,)
  
  if(plots) { 
    ## 10 fold CV plots 
 a <-    barplot(sort(l.fit$beta, decreasing = T),
            main = "Lasso Estimated Coefficients",
            ylab = "Beta Coefficients", 
            col ="lightblue")
b <-     barplot(sort(r.fit$beta, decreasing = T),
            main = "Ridge Estimated Coefficients",
            ylab = "Beta Coefficients", 
            col ="lightblue")
 c <-    barplot(sort(e.fit$beta, decreasing = T),
            main = "Elastic Net Estimated Coefficients",
            ylab = "Beta Coefficients", 
            col ="lightblue")
plot <- grid.arrange(a, b, c)
  
  }
  return(plot)
}  

All_Plots <- All(plots = T)    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  
