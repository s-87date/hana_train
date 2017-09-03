library(foreach)
library(iterators)
library(doParallel)
library(tcltk)
library(utils)

# Choose number of iterations
n <- 1000

cores = detectCores(logical=FALSE) # logical=TRUEでthread数
cluster = makeCluster(cores)
registerDoParallel(cluster)

time3 <- system.time({
  clusterExport(cluster, c("n")) # Export max number of iteration to workers
  k <- foreach(i = icount(n), .packages = "tcltk", .combine = c) %dopar% {
    if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=n)
    setTkProgressBar(pb, i)
    Sys.sleep(0.1)
    log2(i)
  }
})

#Stop the cluster
stopCluster(cluster)

print(time3)

#=================
cores = detectCores(logical=FALSE) # logical=TRUEでthread数
cl <- makeCluster(cores, type='SOCK')
registerDoParallel(cl)
f <- function(){
  pb <- txtProgressBar(min=1, max=n-1,style=3)
  count <- 0
  function(...) {
    count <<- count + length(list(...)) - 1
    setTxtProgressBar(pb,count)
    flush.console()
    rbind(...)
  }
}

n <- 1000
# Run the loop in parallel
k <- foreach(i = seq(n), .final=c, .combine=f()) %dopar% {
  log2(i)
}
k
