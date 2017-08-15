# set working path
setwd("~/hana_train/Kaggle/Zillow")
getwd()

# lib
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(doParallel)

# load dat
properties <- read_delim('input/properties_2016.csv',',')

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

# doParallel
registerDoParallel(detectCores()-1)

t<-proc.time()
isolation <-  foreach(i=1:nrow(latlng[c(1:5),]), .combine="rbind") %dopar% {
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
      isolation125 <<- .[c(1:125),] %>% 
        dplyr::summarise(isolation125=sum(dist)) %>% 
        as.numeric(.)
      isolation625 <<- .[c(1:625),] %>% 
        dplyr::summarise(isolation625=sum(dist)) %>% 
        as.numeric(.)
      density10 <<- 
        dplyr::filter(., dist <= 10) %>% 
        dplyr::summarise(density10=n()) %>% 
        as.numeric(.)
      density100 <<- 
        dplyr::filter(., dist <= 100) %>% 
        dplyr::summarise(density100=n()) %>% 
        as.numeric(.)
      density1000 <<- 
        dplyr::filter(., dist <= 1000) %>% 
        dplyr::summarise(density1000=n()) %>% 
        as.numeric(.)
      density10000 <<- 
        dplyr::filter(., dist <= 10000) %>% 
        dplyr::summarise(density10000=n()) %>% 
        as.numeric(.)
    }
  c(id.a, isolation5, isolation25, isolation125, isolation625, density10, density100, density1000, density10000)
}
isodens %<>% as.data.frame(.) %>% 
  dplyr::rename(parcelid=V1, 
                isolation5=V2, isolation25=V3, isolation125=V4, isolation625=V5,
                dencity10=V6, density100=V7, density1000=V8, density10000=V9)
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

glimpse(isodense)
