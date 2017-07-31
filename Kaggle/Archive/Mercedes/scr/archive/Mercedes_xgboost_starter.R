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

x_train = train_test[1:ntrain, -c(ID, TARGET, 'row_id', 'fold','train','Rand10'), with = FALSE]
x_test = train_test[(ntrain+1):nrow(train_test), -c(ID, TARGET, 'row_id', 'fold','train','Rand10'), with = FALSE]


# Log
y_train <- log(y_train)

dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dtest = xgb.DMatrix(as.matrix(x_test))

xg_R_squared <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= R2_Score(yhat, y)
  return (list(metric = "error", value = err))
}

# CV fit

xgb_params = list(
  seed = SEED,
  subsample = 0.95,
  colsample_bytree = 0.7,
  eta = 0.01,
  objective = 'reg:linear',
  max_depth = 2,
  num_parallel_tree = 2,
  min_child_weight = 3,
  base_score = mean(y_train)
)

# res = xgb.cv(xgb_params,
#             dtrain,
#             nrounds=5000,
#             folds= folds,
#             early_stopping_rounds=15,
#             print_every_n = 50,
#             verbose= 1,
#             feval=xg_R_squared,
#             maximize = TRUE)
# 
# best_nrounds = res$best_iteration
# cv_mean = res$evaluation_log$test_error_mean[best_nrounds]
# cv_std = res$evaluation_log$test_error_std[best_nrounds]
# cat(paste0('CV-Mean: ',cv_mean,' ', cv_std))

# Stopping. Best iteration:
#   [544]	train-error:0.584773+0.015741	test-error:0.562824+0.053626

best_nrounds = 583

# Fit on all data

gbdt = xgb.train(xgb_params, dtrain, best_nrounds)
xgb_importance <- xgb.importance(colnames(train_test[, -c(ID, TARGET, 'row_id', 'fold', 'train', 'Rand10'), with = FALSE]), model = gbdt)
# xgb.plot.importance(xgb_importance)
datatable(xgb_importance, rownames = FALSE, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

plot_ly(data = data.table(x= predict(gbdt,dtrain), y = y_train), x = ~x) %>%
  add_markers(y = ~y, showlegend = FALSE) %>%
  add_lines(y = ~x, showlegend = FALSE) %>%
  layout(title = 'Actual vs Predicted',
         yaxis = list(title = 'Actual capped'),
         xaxis = list(title = 'Predicted'))


# Create csv
submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission[[TARGET]] = predict(gbdt,dtest)
submission[[TARGET]] <- exp(submission[[TARGET]])
# write.csv(submission,'../output/xgb_starter_v7.csv',row.names = FALSE)

# Fir on significant data
lst_features <- xgb_importance$Feature#[-(which(xgb_importance$Feature == "Rand10"))]

x_train = train_test[1:ntrain, lst_features, with = FALSE]
x_test = train_test[(ntrain+1):nrow(train_test), lst_features, with = FALSE]

dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dtest = xgb.DMatrix(as.matrix(x_test))

# res = xgb.cv(xgb_params,
#             dtrain,
#             nrounds=5000,
#             folds= folds,
#             early_stopping_rounds=15,
#             print_every_n = 50,
#             verbose= 1,
#             feval=xg_R_squared,
#             maximize = TRUE)
# 
# best_nrounds = res$best_iteration
# cv_mean = res$evaluation_log$test_error_mean[best_nrounds]
# cv_std = res$evaluation_log$test_error_std[best_nrounds]

cat(paste0('CV-Mean: ',cv_mean,' ', cv_std))

# Stopping. Best iteration:
#   [569]	train-error:0.578955+0.016948	test-error:0.567305+0.055439

best_nrounds = 874

gbdt2 = xgb.train(xgb_params, dtrain, best_nrounds)

xgb_importance2 <- xgb.importance(colnames(train_test[,lst_features, with = FALSE]), model = gbdt2)
datatable(xgb_importance2, rownames = FALSE, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
plot_ly(data = data.table(x= predict(gbdt2,dtrain), y = y_train), x = ~x) %>%
  add_markers(y = ~y, showlegend = FALSE) %>%
  add_lines(y = ~x, showlegend = FALSE) %>%
  layout(title = 'Actual vs Predicted',
         yaxis = list(title = 'Actual capped'),
         xaxis = list(title = 'Predicted'))


# Create csv
submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission[[TARGET]] = predict(gbdt2,dtest)
submission[[TARGET]] <- exp(submission[[TARGET]])
# write.csv(submission,'../output/xgb_starter_v8.csv',row.names = FALSE)






