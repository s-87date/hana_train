library(tidyverse)
library(randomForest)
library(caret)

setwd("~/work/hana_train/Kaggle/banco/src")
list.files("../dat")

test <- readr::read_csv("../dat/test.csv")
train <- readr::read_csv("../dat/train.csv")
sample <- readr::read_csv("../dat/sample_submission.csv")

test %>% dim # 49342  4992
train %>% dim # 4459 4993

# NA col
colnames(train)[colSums(is.na(train)) > 0]
sum(is.na(train)) # 6

# NA <- 0 
train[is.na(train)] <- 0
sum(is.na(train)) #0

colnames(train) <- c("ID","target",paste0("X",colnames(train[-1:-2])))
colnames(test) <- c("ID",paste0("X",colnames(test[-1])))

##=====rf_01=====
t <- proc.time()
tune.result <- tuneRF(train[-1:-2], train$target, doBest = TRUE)
proc.time() - t

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


## public lb 1.76
t <- proc.time()
train.rf <- randomForest(target~.-ID , train, mtry=832, ntree=1000)
proc.time() - t
saveRDS(train.rf,"../model/rf_01.obj")
# ユーザ   システム       経過  
# 15504.956      2.228  15502.865 
# importance(train.rf) %>% write.csv("../output/rf_importance_01.csv")
# predict test
test.pred <- predict(train.rf, test)
data.frame(ID=test$ID, target=test.pred) %>% write_csv("../output/rf_pred_01.csv")


##=====rf_02=====
t <- proc.time()
tune.result.02 <- tuneRF(train[-1:-2], log(train$target+1), doBest = TRUE)
proc.time() - t
# mtry = 1663  OOB error = 2.129765 
# Searching left ...
# mtry = 832 	OOB error = 2.137345 
# -0.003559186 0.05 
# Searching right ...
# mtry = 3326 	OOB error = 2.133793 
# -0.001891287 0.05 
# ユーザ   システム       経過  
# 9884.728      2.532   9887.875 

t <- proc.time()
train.rf.02 <- randomForest(log(target+1)~.-ID , train, mtry=1663, ntree=1000)
proc.time() - t
saveRDS(train.rf.02,"../model/rf_02.obj")

test.pred.02 <- predict(train.rf.02, test)
data.frame(ID=test$ID, target=test.pred.02) %>% write_csv("../output/rf_pred_02.csv")


