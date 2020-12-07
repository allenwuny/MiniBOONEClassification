# load packages 

library(gridExtra)
library(pROC)
library(reshape2)
library(glmnet)
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
p = dim(data)[2]

# Factors  
data$label <- as.factor(c(replicate(elec_num - bad_elec, 1), replicate(muon_num - bad_muon, 0)))



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

auc_df = data.frame(matrix(nrow = 0, ncol=6))

for (i in 1:50){
  set.seed(i+100)
  #splitting data 10/90 ratio
  n_train <-as.integer(0.9*n)
  n_test = n - n_train
  idx <- sample.int(n, n_train)
  #mutually exclusive
  D_train <- data[idx, ]
  D_test <- data[-idx, ]
  # Create X/Y train/test
  X_train <- select(D_train, -label) %>% data.matrix()
  X_test <- select(D_test, -label) %>% data.matrix()
  y_train <-D_train$label
  y_test <- D_test$label
  

  ##################################################################################
  #                        Lasso Regression                                     #
  ##################################################################################
  l.cv.time.start <-Sys.time()  
  l.cv <- cv.glmnet(X_train, y_train, family ="binomial", type.measure="auc", alpha=1)
  l.cv.time <- round(Sys.time() - l.cv.time.start, 1)
  
  l.time.start <-Sys.time() 
  l.fit <- glmnet(X_train, y_train, family="binomial", lambda =l.cv$lambda.min, alpha=1) 
  l.time <- round(Sys.time() - l.time.start, 1)

  
  # fitting
  l.beta0.hat = as.vector(l.fit$a0)
  l.beta.hat = as.vector(l.fit$beta)
  #Probabilities
  l.prob_train = exp(X_train %*% l.beta.hat + l.beta0.hat )/(1+ exp(X_train %*% l.beta.hat + l.beta0.hat )) 
  l.prob_test = exp(X_test %*% l.beta.hat + l.beta0.hat )/(1 + exp(X_test %*% l.beta.hat + l.beta0.hat ))
  #plot(l.cv, sub = paste("LASSO:", l.cv$lambda.min))
  # Compute ROC
  l.roc = roc_table(l.prob_train, l.prob_test)
  # plot_roc(l.roc, "Lasso")
  
  # Store AUC 
  l.train.auc = round(AUC(x=l.roc$FPR_train, y=l.roc$TPR_train),3)
  l.test.auc = round(AUC(x=l.roc$FPR_test, y=l.roc$TPR_test),3)
  auc_df = rbind(auc_df, list(i, "Lasso", 
                              l.train.auc, l.test.auc, 
                              l.cv.time, l.time))
  
  
  ##################################################################################
  #                        Ridge Regression                                     #
  ##################################################################################
  r.cv.time.start <-Sys.time()   
  r.cv <- cv.glmnet(X_train, y_train, family ="binomial", type.measure="auc", alpha=0)
  r.cv.time <- round(Sys.time() - r.cv.time.start,1)
  
  r.time.start <-Sys.time()   
  r.fit <- glmnet(X_train, y_train, family="binomial", lambda = r.cv$lambda.min, alpha=0) 
  r.time <- round(Sys.time() - r.time.start,1)
  
  # fitting
  r.beta0.hat = as.vector(r.fit$a0)
  r.beta.hat = as.vector(r.fit$beta)
  #Probabilities
  r.prob_train = exp(X_train %*% r.beta.hat + r.beta0.hat )/(1+ exp(X_train %*% r.beta.hat + r.beta0.hat )) 
  r.prob_test = exp(X_test %*% r.beta.hat + r.beta0.hat )/(1 + exp(X_test %*% r.beta.hat + r.beta0.hat ))
  #plot(r.cv, sub = paste("RIDGE:", r.cv$lambda.min))
  # Compute ROC
  r.roc = roc_table(r.prob_train, r.prob_test)
  # plot_roc(r.roc, "Ridge")
  
  # Store AUC 
  r.train.auc = round(AUC(x=r.roc$FPR_train, y=r.roc$TPR_train),3)
  r.test.auc = round(AUC(x=r.roc$FPR_test, y=r.roc$TPR_test),3)
  auc_df = rbind(auc_df, list(i, "Ridge", 
                              r.train.auc, r.test.auc, 
                              r.cv.time, r.time))

  ##################################################################################
  #                        Elastic Net Regression                                     #
  ##################################################################################
  # 
  e.cv.time.start <-Sys.time()  
  e.cv <- cv.glmnet(X_train, y_train, family = "binomial", type.measure="auc", alpha=0.5)
  e.cv.time <- round(Sys.time() - e.cv.time.start,1)
  
  e.time.start <-Sys.time()  
  e.fit <- glmnet(X_train, y_train, family="binomial", lambda =e.cv$lambda.min, alpha=0.5)
  e.time <- round(Sys.time() - e.time.start,1)
  
  # fitting
  e.beta0.hat = as.vector(e.fit$a0)
  e.beta.hat = as.vector(e.fit$beta)
  #Probabilities
  e.prob_train = exp(X_train %*% e.beta.hat + e.beta0.hat )/(1+ exp(X_train %*% e.beta.hat + e.beta0.hat )) 
  e.prob_test = exp(X_test %*% e.beta.hat + e.beta0.hat )/(1 + exp(X_test %*% e.beta.hat + e.beta0.hat ))
  #plot(e.cv, sub = paste("ELASTIC-NET:", e.cv$lambda.min))
  # Compute ROC
  e.roc = roc_table(e.prob_train, e.prob_test)
  # plot_roc(e.roc, "Elnet")
  
  
  # Store AUC 
  e.train.auc = round(AUC(x=e.roc$FPR_train, y=e.roc$TPR_train) ,3)
  e.test.auc = round(AUC(x=e.roc$FPR_test, y=e.roc$TPR_test),3)
  auc_df = rbind(auc_df, list(i, "Elnet", 
                              e.train.auc, e.test.auc, 
                              e.cv.time, e.time))
  
  
  ##################################################################################
  #                         Random Forest  Classification                         #
  ##################################################################################
  # Random Forest
  rf.time.start <- Sys.time()
  rf.fit = randomForest(x = X_train, y = as.factor(y_train), mtry=sqrt(p), importance=TRUE)
  rf.time <- round(Sys.time() - rf.time.start,1)
  
  # fiting
  rf.y_pred_train <- predict(rf.fit, X_train)
  rf.y_pred_test <- predict(rf.fit, X_test)

  rf.train.roc <- roc(y_train, as.numeric(rf.y_pred_train))
  rf.test.roc <- roc(y_test, as.numeric(rf.y_pred_test))
  rf.train.auc = round(rf.train.roc$auc[1],3)
  rf.test.auc = round(rf.test.roc$auc[1],3)

  auc_df = rbind(auc_df, list(i, "Random Forest", 
                              rf.train.auc, rf.test.auc, 
                              0, rf.time))
  
  print(cat("Round", i, "Complete. "))

  if(i == 1) {
    ## 10 fold CV plots
    plot(l.cv) 
    plot(r.cv)
    plot(e.cv)

  }
}

colnames(auc_df) = c("Round", "Model", "AUC.Train", "AUC.Test", "CV.Time", "Fit.Time")


##################################################################################
#                         Fit Model Using All Data                            #
##################################################################################


y <-as.numeric(data$label)-1
X <- data[,1:50] %>% data.matrix()
##################################################################################
#                        Lasso Regression                                     #
##################################################################################

l.time.start = Sys.time()
lasso<- cv.glmnet(X, y, family="binomial", alpha=1, type.measure = "auc")
l.fit <- glmnet(X, y, family="binomial", lambda = lasso$lambda.min, alpha=1)
l.time.all = Sys.time() - l.time.start

##################################################################################
#                        Ridge Regression                                     #
##################################################################################

r.time.start = Sys.time()
ridge <- cv.glmnet(X, y, family = "binomial", alpha =0, type.measure = "auc")
r.fit <- glmnet(X, y, family = "binomial", lambda = ridge$lambda.min, alpha =0)
r.time.all = Sys.time() - r.time.start

##################################################################################
#                        Elastic Net Regression                               #
##################################################################################

e.time.start = Sys.time()
elastic <- cv.glmnet(X, y, family = "binomial", alpha=0.5, type.measure = "auc")
e.fit <- glmnet(X, y, family="binomial", lambda =elastic$lambda.min, alpha=0.5)
e.time.all = Sys.time() - e.time.start

##################################################################################
#                        Random Forest                                        #
##################################################################################

rf.time.start <- Sys.time()
rf.fit = randomForest(x = X, y = as.factor(y), mtry=sqrt(p), importance=TRUE)
rf.time.all <- round(Sys.time() - rf.time.start,1)



# Merge coefficients and importance into 1 dataframe to plot
# plots the coefficient and importance by ascending order of elnet values
beta_df = data.frame(Variable = seq(1,50,by=1),
                     Lasso = as.numeric(l.fit$beta), 
                     Ridge = as.numeric(r.fit$beta),
                     Elnet = as.numeric(e.fit$beta),
                     RF = rf.fit$importance[,4])

beta_df = beta_df %>% arrange(Elnet) %>% mutate(Order = seq(1,50,by=1))
beta_df$Variable = paste("X",beta_df$Variable,sep="")
beta_df$Variable = factor(beta_df$Variable)
beta_df$Variable = reorder(beta_df$Variable, beta_df$Order)

beta_df = melt(beta_df, id.vars = c("Variable", "Order"))
beta_df = beta_df %>% rename(Coefficient = value, Model = variable)

ggplot(beta_df) + 
  aes(x=Variable, y=Coefficient) + 
  geom_col() + 
  facet_grid(Model ~., scales="free") + 
  theme(axis.text.x = element_text(size = 8, angle = 30, hjust = 1)) + 
  theme(axis.text.y = element_text(size = 8))

    

    
# Getting 90% intervals
a = auc_df %>% filter(Model=="Lasso") %>% arrange(AUC.Test)
c(a[3,"AUC.Test"], a[48,"AUC.Test"])

b = auc_df %>% filter(Model=="Ridge") %>% arrange(AUC.Test)
c(b[3,"AUC.Test"], b[48,"AUC.Test"])

c = auc_df %>% filter(Model=="Elastic Net") %>% arrange(AUC.Test)
c(c[3,"AUC.Test"], c[48,"AUC.Test"])

d = auc_df %>% filter(Model=="Random Forest") %>% arrange(AUC.Test)
c(d[3,"AUC.Test"], d[48,"AUC.Test"])


# Generate boxplots of AUCs
auc_df1 = auc_df %>% select(Round, Model, AUC.Train, AUC.Test)
auc_df1 = melt(auc_df1, id.vars = c("Round", "Model"))
ggplot(auc_df) + 
  aes(x = Model, y = AUC.Train) + 
  geom_boxplot() + 
  ylab("AUC") + 
  ggtitle("Training")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(auc_df) + 
  aes(x = Model, y = AUC.Test) + 
  geom_boxplot() + 
  ylab("AUC") + 
  ggtitle("Testing")+
  theme(plot.title = element_text(hjust = 0.5))
