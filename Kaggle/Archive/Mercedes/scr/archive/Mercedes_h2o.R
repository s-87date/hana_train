#===library===
library(readr)
library(caret)
library(h2o)
library(Metrics) # for mae() function
library(data.table)

# Load libraries
library(data.table) # data wrangling
library(Matrix)     # xgboost input
library(xgboost)    # gbm modelling
library(fastICA)    # ICA
library(MLmetrics)  # R Squared evaluation
library(DT)         # Interactive tables
library(plotly)     # Interactive visuals
library(tidyverse)
library(fastICA)
library(caret)

# set global val
ID = 'ID'
TARGET = 'y'
SEED = 8787
NUMFOLDS = 5
TRAIN_FILE = "../input/train.csv"
TEST_FILE = "../input/test.csv"
SUBMISSION_FILE = "../input/sample_submission.csv"

# load data
train <- fread('../input/train.csv')
test  <- fread('../input/test.csv')


# CV
ntrain = nrow(train)
train$train <- 1
test$train <- 0

y_train = train[[TARGET]]
train_test = rbind(train, test, fill = TRUE)

train_test$Rand10 <- as.integer(runif(nrow(train_test),min = 0, max = 10)+1)
train_test$fold <- rep(1:NUMFOLDS)
train_test$row_id <- 1:nrow(train_test)

trainset <- which (train_test$train == 1)
testset <- which (train_test$train == 0)

folds = list()
for (i in 1:NUMFOLDS) folds[[i]] =  which (train_test[trainset]$fold == i)


# magic feature
train_test$meanX0 <- mean(train_test$y, na.rm = TRUE)
for (i in 1:NUMFOLDS) {
  meanX0 <- train_test[train_test$fold != i,
                       .(meanX0 = mean(y, na.rm = TRUE)),
                       by = X0]
  tmp <- merge(train_test[train_test$fold ==i, c('row_id', 'fold', 'X0'), with = FALSE], meanX0, by = 'X0')
  train_test[ tmp$row_id , 'meanX0'] <- tmp[['meanX0']]
}
train_test[is.na(train_test$meanX0), 'meanX0'] <- mean(train_test$y, na.rm = TRUE)
train_test<- as.data.frame(train_test)



# #encode character features as numeric
# # SHOULD one hot encode
# features = names(train_test)
# for (f in features) {
#   if (class(train_test[[f]])=="character") {
#     #cat("VARIABLE : ",f,"\n")
#     levels <- unique(train_test[[f]])
#     train_test[[f]] <- as.integer(factor(train_test[[f]], levels=levels))
#   }
# }
# train_test[] <- lapply(train_test, as.numeric)

# one-hot-encoding features
ohe_feats = c('X0', 'X1', 'X2', 'X3', 'X4', 'X5','X6', 'X8')
dummies = dummyVars(~ X0 + X1 + X2 + X3 + X4 + X5 + X6 + X8 , data = train_test)
df_all_ohe <- as.data.frame(predict(dummies, newdata = train_test))
df_all_combined <- bind_cols(train_test[,-c(which(colnames(train_test) %in% ohe_feats))],df_all_ohe)

train_test = as.data.table(df_all_combined)

# train = train_test[data$ID %in% df_train$ID,]
# y_train <- train[!is.na(y),y]
# train = train[,y:=NULL]
# train = train[,ID:=NULL]
# #train_sparse <- as(data.matrix(train),"dgCMatrix")
# train_sparse <- data.matrix(train)
# 
# test = data[data$ID  %in% df_test$ID,]
# test_ids <- test[,ID]
# test[,y:=NULL]
# test[,ID:=NULL]
# #test_sparse <- as(data.matrix(test),"dgCMatrix")
# test_sparse <- data.matrix(test)
# 
# dtrain <- xgb.DMatrix(data=train_sparse, label=y_train)
# dtest <- xgb.DMatrix(data=test_sparse)





# number of principal components
n.comp <- 100

# PCA
pca  <- prcomp(train_test[, -c(ID, TARGET, "train", "Rand10", "fold", "row_id"), with = FALSE], scale. = T, center = T)

# ICA
ica  <- fastICA(train_test[, -c(ID, TARGET, "train", "Rand10", "fold", "row_id"), with = FALSE], n.comp, alg.typ = "parallel",
                fun = "logcosh", alpha = 1.0, method = "C",
                row.norm = FALSE, maxit = 200, tol = 1e-04, verbose = FALSE,
                w.init = NULL)


# Append decomposition components to datasets
for (i in 1:n.comp) {
  train_test[[paste0('pca_', i)]] = pca$x[,i]
  train_test[[paste0('ica_', i)]] = ica$S[,i]
}



# XGboost
# prepare

x_train = train_test[1:ntrain, - c('row_id', 'fold', 'Rand10', 'train'), with = FALSE]
x_test = train_test[(ntrain+1):nrow(train_test),- c('row_id', 'fold', 'Rand10', 'train'), with = FALSE]

# Log
# x_train[["y"]] <- log(x_train[["y"]])
# x_test[["y"]] <- log(x_test[["y"]])

# initialize h2o
h2o.init(nthreads=-1,max_mem_size='6G')
testHex = as.h2o(x_test)
trainHex = as.h2o(x_train)

# define the features
features<-colnames(x_train)[!(colnames(x_train) %in% c("ID","y"))]

# training function
mpreds = data.table(id=x_test$ID)

for(random.seed.num in 1:2) {
  
  set.seed(random.seed.num)
  
  print(paste("[", random.seed.num, "] training dnn begin ",sep=""," : ",Sys.time()))
  
  dl <- h2o.deeplearning(x = features
                         ,y = "y" 
                         ,hidden = c(300,300,300)
                         ,seed = random.seed.num
                         #,reproducible = TRUE
                         ,epochs = 1000
                         #,variable_importances = TRUE
                         ,training_frame=trainHex
                         #,nfolds = 10
  )
  
  vpreds = as.matrix(predict(dl,testHex)) 
  mpreds = cbind(mpreds, vpreds)    
  colnames(mpreds)[random.seed.num+1] = paste("pred_seed_", random.seed.num, sep="")
}

# average

mpreds_2 = mpreds[, id:= NULL]
mpreds_2 = mpreds_2[, y := rowMeans(.SD)]

submission = data.table(ID=x_test$ID, y=mpreds_2$y)
# submission[["y"]] <- exp(submission[["y"]])
# write.table(submission, "../output/mercedes_submission_dnn_v02.csv", sep=",", dec=".", quote=FALSE, row.names=FALSE)

plot_ly(data = data.table(x= mpreds_2$y, y = x_train$y), x = ~x) %>%
  add_markers(y = ~y, showlegend = FALSE) %>%
  add_lines(y = ~x, showlegend = FALSE) %>%
  layout(title = 'Actual vs Predicted',
         yaxis = list(title = 'Actual capped'),
         xaxis = list(title = 'Predicted'))

