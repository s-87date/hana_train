setwd("~/work/hana_train/practice/scr")
# remove.packages("rPython")
Sys.setenv(RPYTHON_PYTHON_VERSION=3.6) 
Sys.setenv(RPYTHON_PYTHON_PATH='/Users/hanadate/.pyenv/versions/anaconda3-4.3.1/bin/python')
install.packages("rPython")
library(reticulate)
reticulate::use_python('/Users/hanadate/.pyenv/versions/anaconda3-4.3.1/bin/python', required = TRUE)

library(rPython)
python.load("python_version.py")
# システムのPythonが呼び出される。。。