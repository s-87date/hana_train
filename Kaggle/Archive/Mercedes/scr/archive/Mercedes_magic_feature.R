# Libraries
library(tidyverse)
library(magrittr)
library(plotly)
library(caret)
library(MLmetrics)

dat <- read_delim("../input/train.csv",",")
# test <- read_delim("../input/test.csv",",")

# devide dat
set.seed(8787)
dat.row <- c(1:nrow(dat))
train_ind <- sample(seq_len(nrow(dat)), size = round(nrow(dat)*0.8))
train <- dat[train_ind, ]
test <- dat[-train_ind, ]

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
# group_by X0 and mean y
train.meanX0 <- train %>%
  dplyr::select(ID, y, X0) %>% 
  dplyr::group_by(X0) %>% 
  dplyr::mutate(meanX0 = mean(y)) %>% 
  dplyr::ungroup(.)
meanX0 <- train.meanX0 %>% 
  dplyr::distinct(X0, meanX0) 
# View(meanX0)
glimpse(train.meanX0)

# linear regression
res.lm <- lm(formula = y ~ meanX0, data =train.meanX0)

plot_ly(data = data.table(x= train.meanX0$meanX0, y = train.meanX0$y), x = ~x) %>%
  add_markers(y = ~y, showlegend = FALSE) %>%
  add_lines(y = ~x, showlegend = FALSE) %>%
  layout(title = 'Actual vs meanX0',
         yaxis = list(title = 'Actual capped'),
         xaxis = list(title = 'meanX0'))

test.meanX0 <- test %>% 
  dplyr::select(ID, y, X0) %>% 
  dplyr::inner_join(., meanX0, by="X0") %>% 
  tidyr::replace_na(meanX0 = mean(na.omit(.$meanX0)))

pred.test <- predict(res.lm, test.meanX0)

test.meanX0 %<>% 
  dplyr::mutate(PRED= pred.test) %>% 
  dplyr::select(-meanX0)
glimpse(test.meanX0)  


plot_ly(data = data.table(x= test.meanX0$PRED, y = test.meanX0$y), x = ~x) %>%
  add_markers(y = ~y, showlegend = FALSE) %>%
  add_lines(y = ~x, showlegend = FALSE) %>%
  layout(title = 'Actual vs Predicted',
         yaxis = list(title = 'Actual capped'),
         xaxis = list(title = 'Predicted'))
cat("  COR: ", cor(test.meanX0$PRED, test.meanX0$y), "\n",
    "MAPE: ", MAPE(test.meanX0$PRED, test.meanX0$y))
