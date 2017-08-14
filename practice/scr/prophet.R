library(prophet)
library(tidyverse)

dat <- read_delim("example_wp_peyton_manning.csv",",")
dat

res <- prophet(dat)
f <- make_future_dataframe(res, 365)
glimpse(f)
