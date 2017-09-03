library(foreach)
library(iterators)
library(doParallel)
library(tcltk)

# Choose number of iterations
n <- 1000

cl <- makeCluster(8)

registerDoParallel(cl)

time3 <- system.time({
  clusterExport(cl, c("n")) # Export max number of iteration to workers
  k <- foreach(i = icount(n), .packages = "tcltk", .combine = c) %dopar% {
    if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=n)
    setTkProgressBar(pb, i)
    Sys.sleep(0.05)
    log2(i)
  }
})

#Stop the cluster
stopCluster(cl)

print(time3)