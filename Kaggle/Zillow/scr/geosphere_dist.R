library(geosphere)
library(doParallel)
glimpse(latlng)
temp.latlng <- latlng[1:10,]
glimpse(temp.latlng)

i.temp.latlng <- iter(temp.latlng, by="row")

res <- foreach(i1 = i.temp.latlng, .combine="rbind") %do% {
  init.lat <- i1$latitude; init.lon <- i1$longitude
  foreach(i2 = i.temp.latlng, .combine="rbind")
}
