library(foreach)
library(doParallel)
library(dplyr)

# preparing df
dat <- data.frame(a=seq(10000),b=seq(seq(10000)))

# seq proc
t <- proc.time()
zero.10000 <- foreach(i=seq(nrow(dat)), .combine = c) %do% { 
  zero <- dat[i,] %>% mutate(ab = a-b) %>% .$ab
}
sum(zero.10000); length(zero.10000)
proc.time()-t # 22sec

# pre-process for parallel computing
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# doing prallel computing
t <- proc.time()
zero.10000 <- foreach(i=seq(nrow(dat)), .combine = c, .packages = "dplyr") %dopar% { 
  zero <- dat[i,] %>% mutate(ab = a-b) %>% .$ab
}
sum(zero.10000); length(zero.10000)
proc.time()-t # 12sec


# stop parallel computing
stopImplicitCluster2 <- function() {
  options <- doParallel:::.options
  if (exists(".revoDoParCluster", where = options) &&
      !is.null(get(".revoDoParCluster", envir = options))) {
    stopCluster(get(".revoDoParCluster", envir = options))
    remove(".revoDoParCluster", envir = options)
  }
}
stopImplicitCluster2()



df <- data.frame(x=c(100,1000))

# do の中で処理を流すと正しい返り値
res1 <- foreach(i = iter(df, by="row"), .combine = "c") %do% {
  xxx <- i
  xxx + 1
}
res1

# do の中の処理をsourceで外出すると、オブジェクトがグローバル環境にないため返り値がおかしい
res2 <- foreach(i = iter(df, by="row"), .combine = "c") %do% {
  xxx <- i
  source("tashizan1.R")
}
res2

# source内を関数にして呼び出せばよい
source("tashizan2.R")
res3 <- foreach(i = iter(df, by="row"), .combine = "c") %do% {
  xxx <- i
  tashizan2.func(xxx)
}
res3
