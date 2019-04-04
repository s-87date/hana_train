library(compiler)

mysum <- function(x)
{
  s <- 0
  for(y in x){
    s <- s + y
  }
  s
}
mysum.compiled <- cmpfun(mysum)

saveRDS(mysum, "~/work/hana_train/compiler/mysum")
saveRDS(mysum.compiled, "~/work/hana_train/compiler/mysum_compiled")


mysum
mysum.compiled
