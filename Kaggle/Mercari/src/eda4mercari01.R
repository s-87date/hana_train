# setwd("/Users/hanadate/work/hana_train/Kaggle/Mercari/working")
getwd()
paste0("DIR_STR: ", list.files("../"))
paste0("WORKING: ", list.files())
paste0("INPUT: ",list.files("../input"))
paste0("SRC: ",list.files("../src"))

library(tidyverse)

your.dat.path <- "../input/"
list.files(paste0(your.dat.path))

train.dat <- readr::read_tsv(paste0(your.dat.path, "train.tsv"))
train.dat %>% glimpse() # Observations: 1,482,535

test.dat <- readr::read_tsv(paste0(your.dat.path, "test.tsv"))
test.dat %>% glimpse() # Observations: 693,359

sample.sub <- readr::read_csv(paste0(your.dat.path, "sample_submission.csv"))
sample.sub %>% glimpse() # Observations: 693,359
