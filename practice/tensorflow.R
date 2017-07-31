#==!!!!!!!!!==
# You can only load one Python interpreter per R session 
# (it can't really be cleanly unloaded without messy side effects that cause crashes down the road) 
# so the use_python call only applies before you actually initialize the interpreter.
#==!!!!!!!!!==
# install.packages("devtools")
# library(devtools)
# devtools::install_github("rstudio/tensorflow")
# Sys.setenv(TENSORFLOW_PYTHON='/Users/hanadate/.pyenv/versions/anaconda3-4.3.1/bin/python')
library(reticulate)
reticulate::use_python('/Users/hanadate/.pyenv/versions/anaconda3-4.3.1/bin/python', required = TRUE)
# py_discover_config()
# py_config()

library(tensorflow)
hello = tf$constant('Hello TensorFlow for R.')
sess = tf$Session()
sess$run(hello)
