library(tidyverse)
library(randomForest)
library(caret)

setwd("~/work/hana_train/Kaggle/banco/src")
list.files("../dat")

test <- readr::read_csv("../dat/test.csv")
train <- readr::read_csv("../dat/train.csv")

test %>% dim # 49342  4992
train %>% dim # 4459 4993

# NA col
colnames(train)[colSums(is.na(train)) > 0]
sum(is.na(train)) # 6

# NA <- 0 
train[is.na(train)] <- 0
sum(is.na(train)) #0

# t <- proc.time()
# tune.result <- tuneRF(train[-1:-2], train$target, doBest = TRUE)
# proc.time() - t

# mtry = 1663  OOB error = 5.30115e+13 
# Searching left ...
# mtry = 832 	OOB error = 5.235893e+13 
# 0.01230986 0.05 
# Searching right ...
# mtry = 3326 	OOB error = 5.310557e+13 
# -0.001774507 0.05 
# Call:
#   randomForest(x = x, y = y, mtry = res[which.min(res[, 2]), 1]) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 832
# 
# Mean of squared residuals: 5.040636e+13
# % Var explained: 25.64
# > proc.time() - t
# ユーザ   システム       経過  
# 12220.560      1.992  12219.826 

# colnames(train) <- c("ID","target",paste0("X",colnames(train[-1:-2])))

t <- proc.time()
train.rf <- randomForest(target~.-ID , train, mtry=832, ntree=1000)
proc.time() - t

