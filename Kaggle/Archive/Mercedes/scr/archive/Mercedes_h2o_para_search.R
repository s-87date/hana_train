library(readr)
library(caret)
library(h2o)
library(Metrics) # for mae() function
library(data.table)



# load data
tr <- fread('../input/train.csv', na.strings = "NA")
ts <- fread("../input/test.csv", sep=",", na.strings = "NA")


# initialize h2o
h2o.init(nthreads=-1,max_mem_size='6G')
testHex = as.h2o(ts)
trainHex = as.h2o(tr)

# define the features
predictors <-colnames(tr)[!(colnames(tr) %in% c("ID","y"))]
response = "y"

#### Random Hyper-Parameter Search
#Often, hyper-parameter search for more than 4 parameters can be done more efficiently with random parameter search than with grid search. Basically, chances are good to find one of many good models in less time than performing an exhaustive grid search. We simply build up to `max_models` models with parameters drawn randomly from user-specified distributions (here, uniform). For this example, we use the adaptive learning rate and focus on tuning the network architecture and the regularization parameters. We also let the grid search stop automatically once the performance at the top of the leaderboard doesn't change much anymore, i.e., once the search has converged.
hyper_params <- list(
  activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  hidden=list(c(20,20),c(50,50),c(75,75),c(100,100),c(30,30,30),c(25,25,25,25)),
  input_dropout_ratio=c(0,0.03,0.05),
  #rate=c(0.01,0.02,0.05),
  l1=seq(0,1e-4,1e-6),
  l2=seq(0,1e-4,1e-6)
)

## Stop once the top 5 models are within 1% of each other (i.e., the windowed average varies less than 1%)
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 360, max_models = 100, seed=1234567, stopping_rounds=5, stopping_tolerance=1e-2)
dl_random_grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id = "dl_grid_random",
  training_frame=trainHex,
  x=predictors, 
  y=response,
  epochs=1,
  stopping_metric="RMSE",
  stopping_tolerance=1e-2,        ## stop when logloss does not improve by >=1% for 2 scoring events
  stopping_rounds=2,
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  max_w2=10,                      ## can help improve stability for Rectifier
  hyper_params = hyper_params,
  search_criteria = search_criteria
)                                
grid <- h2o.getGrid("dl_grid_random",sort_by="mae",decreasing=FALSE)
grid

grid@summary_table[1,]
best_model <- h2o.getModel(grid@model_ids[[1]]) ## model with lowest logloss
best_model
