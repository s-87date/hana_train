library(mlr)
library(tidyverse)
library(mlbench)

# == prep
data("Soybean")
Soybean %>% glimpse
soy = createDummyFeatures(Soybean, target="Class")
tsk = makeClassifTask(data=soy, target="Class")
ho = makeResampleInstance("Holdout", tsk)
tsk.train = subsetTask(tsk, ho$train.inds[[1]])
tsk.test = subsetTask(tsk, ho$test.inds[[1]])

# == create learner
lrn = makeLearner("classif.xgboost", nrounds=10)
cv = makeResampleDesc("CV", iters=5)
res = resample(lrn, tsk.train, cv, kappa)
# res = resample(lrn, tsk.train, cv, acc)

# tune
ps = makeParamSet(makeNumericParam("eta",0,1),
                  makeNumericParam("lambda",0,200),
                  makeIntegerParam("max_depth",1,20))
tc = makeTuneControlMBO(budget=100)
tr = tuneParams(lrn, tsk.train, cv5, kappa, ps, tc)
lrn = setHyperPars(lrn, par.vals=tr$x)

mdl = train(lrn,tsk.train)
prd = predict(mdl,tsk.test)
calculateConfusionMatrix(prd)
mdl = train(lrn, tsk)

soy.new = soy %>% 
  dplyr::select(-Class)

prd.new = predict(mdl, newdata = soy.new)
prd.new$data %>% as.data.frame %>% head
