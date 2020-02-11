library(ape)
ozone <- read.table("https://stats.idre.ucla.edu/stat/r/faq/ozone.csv", sep=",", header=T)
head(ozone, n=10)

ozone.dists <- as.matrix(dist(cbind(ozone$Lon, ozone$Lat)))

ozone.dists.inv <- 1/ozone.dists
diag(ozone.dists.inv) <- 0

ozone.dists.inv[1:5, 1:5]

Moran.I(ozone$Av8top, ozone.dists.inv)

ozone.dists.bin <- (ozone.dists > 0 & ozone.dists <= .75)
Moran.I(ozone$Av8top, ozone.dists.bin)
