# set working path
setwd("~/workspace/hana_train/Kaggle/Zillow")
getwd()

# lib
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(doParallel)
library(foreach)
library(utils)
library(iterators)
library(tcltk)

# load dat
properties <- read_csv('input/properties_2016.csv')

# select col
latlng <- properties %>% 
  dplyr::select(parcelid,latitude,longitude) %>% 
  dplyr::mutate(latitude=latitude/1e06, longitude=longitude/1e06) %>% #glimpse() #Observations: 2,985,217
  na.omit() #Observations: 2,973,780
glimpse(latlng)

# hubeny formula
pi = 3.1415926535
p = 6378137
hubeny <- function(ax,ay, bx,by){
  xdif <- (ax - bx)*(pi/180) # x dif (経度の差) longitude
  ydif <- (ay - by)*(pi/180) # y dif (緯度の差) latitude
  deltax <- p*xdif*cos(ay*(pi/180)) # 経度の差に基づく東西距離(m)
  deltay <- p*ydif           # 緯度の差に基づく南北距離(m)
  sqrt(deltax^2 + deltay^2)  # Distance(m)
}
rm(properties)
gc(); gc()

# id.pair <- latlng %>%
#   dplyr::mutate(parcelid.a = parcelid, parcelid.b = parcelid) %>%
#   dplyr::select(parcelid.a, parcelid.b) %>% 
#   expand.grid(.) %>% 
#   dplyr::filter(parcelid.a!=parcelid.b)
# glimpse(id.pair)  

# (lat.min.max <- range(latlng$latitude,na.rm=T))
# (lng.min.max <- range(latlng$longitude,na.rm=T))
# (lat.range <- abs(lat.min.max[1]-lat.min.max[2]))
# (lng.range <- abs(lng.min.max[1]-lng.min.max[2]))

# progress bar
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

# doParallel
cores = detectCores(logical=FALSE) # logical=TRUEでthread数
cluster = makeCluster(cores)
registerDoParallel(cluster)
# registerDoParallel(detectCores()-4)

# 2.0GHz*6cores 1:30で175sec つまり全部で200days
t<-proc.time()
n <- 30
isodens <-  foreach(i=seq(n), .combine=f(), .packages="dplyr") %dopar% {
  id.a <- latlng[i,]$parcelid
  lat.a <- latlng[i,]$latitude
  lng.a <- latlng[i,]$longitude
  
  latlng[-i,] %>% 
    dplyr::mutate(dist=hubeny(lng.a, lat.a, longitude, latitude)) %>% 
    dplyr::arrange(dist) %>% {
      isolation5 <<- .[c(1:5),] %>% 
        dplyr::summarise(isolation5=sum(dist)) %>% 
        as.numeric(.)
      isolation25 <<- .[c(1:25),] %>% 
        dplyr::summarise(isolation25=sum(dist)) %>% 
        as.numeric(.)
      density100 <<- 
        dplyr::filter(., dist <= 100) %>% 
        dplyr::summarise(density100=n()) %>% 
        as.numeric(.)
      density1000 <<- 
        dplyr::filter(., dist <= 1000) %>% 
        dplyr::summarise(density1000=n()) %>% 
        as.numeric(.)
    }
  c(id.a, isolation5, isolation25, density100, density1000)
}
# isolationX: 直近X棟との総距離
# densityX: Xm以内の距離にある棟数
isodens %<>% as.data.frame(.) %>% 
  dplyr::rename(parcelid=V1, 
                isolation5=V2, isolation25=V3,
                density100=V4, density1000=V5)
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
proc.time()-t

glimpse(isodens)
saveRDS(isodens,"output/isodens.rds")