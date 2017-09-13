library(doParallel)

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
