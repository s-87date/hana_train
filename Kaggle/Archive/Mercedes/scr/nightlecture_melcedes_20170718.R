# Libraries
library(tidyverse)
library(magrittr)
library(plotly)
library(caret)
library(MLmetrics)

dat <- read_delim("../input/train.csv",",")
# test <- read_delim("../input/test.csv",",")

# one hot encoding
X0.dummy <- dummyVars(~X0, data=dat)
dat.dummy <- as.data.frame(predict(X0.dummy, dat))

# devide dat
set.seed(8787)
dat.row <- c(1:nrow(dat.dummy))
train_ind <- sample(seq_len(nrow(dat.dummy)), size = round(nrow(dat.dummy)*0.8))
train <- dat[train_ind,] %>%
  dplyr::select(ID,y,X0) %>% 
  dplyr::bind_cols(., dat.dummy[train_ind, ])
test <- dat[-train_ind,] %>% 
  dplyr::select(ID,y,X0) %>% 
  dplyr::bind_cols(.,dat.dummy[-train_ind, ])

glimpse(train); ncol(train) # 4209*378
glimpse(train[,c(1:20)])
train[c(1:100),]$y

# view y 
plot_ly(x = ~train$y, type="histogram")
cat(" AVG: ", mean(train$y), "\n",
    "MAX: ", max(train$y), "\n",
    "MIN: ", min(train$y), "\n",
    "MED: ", median(train$y), "\n",
    "VAR: ", var(train$y), "\n",
    " SD: ", sd(train$y), "\n")

# view X0
mode(train$X0)
sort(unique(train$X0))
str(train)

# linear regression
res.lm <- lm(formula = y ~ .-ID-X0, data = train)

pred.test <- predict(res.lm, test)
test.pred <- test %>% 
  dplyr::mutate(PRED= pred.test) %>% 
  dplyr::select(ID,y,PRED)
glimpse(test.pred)  

plot_ly(data = data.table(x= test.pred$PRED, y = test.pred$y), x = ~x) %>%
  add_markers(y = ~y, showlegend = FALSE) %>%
  add_lines(y = ~x, showlegend = FALSE) %>%
  layout(title = 'Actual vs Predicted',
         yaxis = list(title = 'Actual capped'),
         xaxis = list(title = 'Predicted'))
cat("  COR: ", cor(test.pred$PRED, test.pred$y), "\n",
    "MAPE: ", MAPE(test.pred$PRED, test.pred$y))
