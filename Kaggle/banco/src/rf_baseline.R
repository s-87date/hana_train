library(tidyverse)
library(randomForest)
library(caret)

setwd("~/work/hana_train/Kaggle/banco/src")
list.files("../dat")

test.df <- readr::read_csv("../dat/test.csv")
train.df <- readr::read_csv("../dat/train.csv")

test.df %>% dim # 49342  4992
train.df %>% dim # 4459 4993

# NA col
colnames(train.df)[colSums(is.na(train.df)) > 0]
sum(is.na(train.df)) # 6

# NA <- 0 
train.df[is.na(train.df)] <- 0
sum(is.na(train.df)) #0

t <- proc.time()
tune.result <- tuneRF(train.df[-1:-2], train.df$target, doBest = TRUE)
proc.time() - t

