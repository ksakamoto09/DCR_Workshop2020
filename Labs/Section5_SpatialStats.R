library(sf)
library(sp)
library(maptools)
library(spatstat)

# as PPP with down sample
nycMPPP <-  as(nyc_supermarkets %>% sample_frac(.5), "Spatial") %>% 
    as("ppp")

nycMKerneldefault <- density(nycMPPP)

plot(nycMKerneldefault, main=NULL, las=1)
raster::contour(nycMKerneldefault, add = TRUE)

?density
nycMKernel5000 <- density(nycMPPP, sigma = 5000)
plot(nycMKernel5000, main="Non-Weighted", las=1)
raster::contour(nycMKernel5000, add = TRUE)


#ANN
## nndist function

# average distance to first nearest neighbor
annNycMPPP <- mean(nndist(nycMPPP, k=1))
annNycMPPP

# average distance to second nearest neighbor
mean(nndist(nycMPPP, k=2))

## The average nearest neighbor function can be expanded to generate an 
## ANN vs neighbor order plot. In the following example, 
## we’ll plot ANN as a function of neighbor order for the for all of the neighbors:
    
ANN <- 1:(nycMPPP$n-1) %>% 
    as.list() %>% 
    purrr::map_dbl(function(x)mean(nndist(nycMPPP, k = x)))

plot(ANN ~eval(1:(nycMPPP$n-1)), type = "b",main = "Average Nearest Neighbor")

# Create ANN ratio with total area
annRatio <- annNycMPPP / (0.5/(sqrt(nycMPPP$n/(area(nycMPPP)))))
annRatio

# Change Area and see the effect
annRatio2 <- annNycMPPP / (0.5/(sqrt(nycMPPP$n/(area(nycMPPP)/10))))
annRatio2

# Number of simulations
n     <- 200L  

# Create an empty object to be used to store simulated ANN values
annR <- vector(length = n) 

## Run simulation
for (i in 1:n){
    # Generate random point locations in NYC
    randP   <- rpoint(n=nycMPPP$n, win= as(nycbb,"Spatial") %>% as("owin"))
    # Tally the ANN values
    annR[i] <- mean(nndist(randP, k=1)) 
}

# let's take a look at what random points distributed along NYC would look like:

Window(randP) <- as(nycbb,"Spatial") %>% as("owin")
plot(randP, pch=16, main=NULL, cols=rgb(0,0,0,0.5))


#Can you easily tell that it's random? How sure can you be? more on that later...

#Let's see the distribution of the ANNs from the Monte Carlo simulation.
library(ggplot2)

tibble(ANN = annR) %>% 
    ggplot(aes(x = ANN)) + 
    geom_histogram(fill = "green", col = "gray") +
    theme_minimal()

#... and where does our actual ANN value sit?
    
tibble(ANN = annR) %>% 
    ggplot(aes(x = ANN)) + 
    geom_histogram(fill = "green", col = "gray") +
    geom_vline(xintercept = annNycMPPP,linetype = "dashed") + 
    theme_minimal()

#A (pseudo) p-value can be extracted from a Monte Carlo simulation.
#We’ll work off of the last simulation. 
#First, we need to find the number of simulated ANN values greater than our observed ANN value.

nGreater <- sum(annR > annNycMPPP)
p <- min(nGreater + 1, n + 1 - nGreater) / (n +1)
p


