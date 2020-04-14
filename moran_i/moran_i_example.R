# example 1
# ref: https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
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

# example 2
# ref: https://mgimond.github.io/Spatial/spatial-autocorrelation-in-r.html
load(url("http://github.com/mgimond/Spatial/raw/master/Data/moransI.RData"))
library(tmap)
library(spdep)
tm_shape(s1) + tm_polygons(style="quantile", col = "Income") +
  tm_legend(outside = TRUE, text.size = .8) 
nb <- poly2nb(s1, queen=TRUE)

nb[[1]]
s1$NAME[1]
s1$NAME[c(2,3,4,5)]

lw <- nb2listw(nb, style="W", zero.policy=TRUE)
lw$weights[1]
Inc.lag <- lag.listw(lw, s1$Income)

# Create a regression model
M <- lm(Inc.lag ~ s1$Income)
# Plot the data
plot( Inc.lag ~ s1$Income, pch=20, asp=1, las=1)
coef(M)[2]
s1$Income 

n <- 599L   # Define the number of simulations
I.r <- vector(length=n)  # Create an empty vector

for (i in 1:n){
  # Randomly shuffle income values
  x <- sample(s1$Income, replace=FALSE)
  # Compute new set of lagged values
  x.lag <- lag.listw(lw, x)
  # Compute the regression slope and store its value
  M.r    <- lm(x.lag ~ x)
  I.r[i] <- coef(M.r)[2]
}

# Plot the histogram of simulated Moran's I values
# then add our observed Moran's I value to the plot
hist(I.r, main=NULL, xlab="Moran's I", las=1)
abline(v=coef(M)[2], col="red")


N.greater <- sum(coef(M)[2] > I.r)
p <- min(N.greater + 1, n + 1 - N.greater) / (n + 1)
p

moran.test(s1$Income,lw)
MC<- moran.mc(s1$Income, lw, nsim=599)
# View results (including p-value)
MC
plot(MC, main="", las=1)


coo <- coordinates(s1)
S.dist  <-  dnearneigh(coo, 0, 50000)
lw <- nb2listw(S.dist, style="W",zero.policy=T)
MI  <-  moran.mc(s1$Income, lw, nsim=599,zero.policy=T)
plot(MI, main="", las=1)

MI
