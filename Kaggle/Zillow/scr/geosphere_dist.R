setwd("~/work/hana_train/Kaggle/Zillow")
library(geosphere)
library(doParallel)
library(iterators)

properties <- read_csv('input/properties_2016.csv')
latlng <- properties %>% 
  dplyr::select(parcelid,latitude,longitude) %>% 
  dplyr::mutate(latitude=latitude/1e06, longitude=longitude/1e06) %>% #glimpse() #Observations: 2,985,217
  na.omit() #Observations: 2,973,780
glimpse(latlng)
temp.latlng <- latlng[1:10000,]
glimpse(temp.latlng)

# doParallel
cores = detectCores(logical=FALSE) # logical=TRUEでthread数
cluster = makeCluster(cores)
registerDoParallel(cluster)

t<-proc.time()
i1.temp.latlng <- iter(temp.latlng, by="row")
res <- 
  foreach(i1 = i1.temp.latlng, .combine = "c", .packages = c("doParallel")) %dopar% {
    i2.temp.latlng <- iter(temp.latlng, by="row")
    dist.v <- 
      foreach(i2 = i2.temp.latlng, .combine = "c", .packages = c("geosphere")) %dopar% {
        distHaversine(c(i1$longitude,i1$latitude),c(i2$longitude,i2$latitude))
      }
    sum(sort(dist.v)[1:5])
  }
proc.time()-t
#### 1core ####
# 1:10 -> 0.246
# 1:100 -> 10.289
# 1:1000 -> 927.181
#### 4core ####
# 1:10 -> -
# 1:100 -> 2.248
# 1:1000 -> 162.515
# 1:10000 -> 


# stopImplicitCluster2
stopImplicitCluster2 <- function() {
  options <- doParallel:::.options
  if (exists(".revoDoParCluster", where = options) &&
      !is.null(get(".revoDoParCluster", envir = options))) {
    stopCluster(get(".revoDoParCluster", envir = options))
    remove(".revoDoParCluster", envir = options)
  }
}
stopImplicitCluster2()
length(res)
