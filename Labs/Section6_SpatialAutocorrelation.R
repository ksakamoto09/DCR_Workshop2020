library(spdep)
library(sf)
library(sp)
library(tmap)
library(ggplot2)

nyct <- st_read("Shapes/nyct2010")

nycACS <- readr::read_csv("Data/nycACS2017.csv")

# 
nycACS %<>% dplyr::select(Geo_COUNTY, Geo_TRACT, SE_A14006_001) %>% 
    dplyr::rename(BoroCode = Geo_COUNTY,
                  CT2010 =Geo_TRACT,
                  MedIncome = SE_A14006_001) %>% 
    dplyr::mutate(BoroCode = dplyr::case_when(
        BoroCode == "047" ~"3",
        BoroCode == "061" ~"1",
        BoroCode == "081" ~"4",
        BoroCode == "085" ~"5",
        BoroCode == "005" ~ "2"
        
    ))

## joining data
nyct <- nyct %>% dplyr::left_join(nycACS, 
                           by = c("BoroCode" = "BoroCode", 
                                  "CT2010" = "CT2010")) %>% 
    dplyr::mutate(MedIncome = tidyr::replace_na(MedIncome, 0 ))

# plot median income by census tracts
tm_shape(nyct) + tm_polygons(col = "MedIncome")

nyct <- nyct %>% 
    # adding a little noise for 0s
    dplyr::mutate(MedIncome = dplyr::if_else(MedIncome == 0, 0.1, MedIncome)) 

# Cast as a sp object
nyctSP  <- as(nyct, "Spatial")

# we are going to use the `poly2nb` function with the queen case as `TRUE` to 
# create our neighbors list. 
nb <- poly2nb(nyctSP, queen=TRUE)

nb
# first item
nb[[1]]

#inspecting our neighbors list, we can see that three polygons
#do not have any neighbors! This is not good and will have to be removed.

tm_shape(nyct) + tm_borders(col = "gray") + 
    tm_shape(nyct[c(612, 806, 1623),]) + tm_fill(col = "red") + 
    tm_text(text = "CT2010", size = .6, remove.overlap = FALSE, col = "black")

#You can also see when we call the first item in our neighbors list, it returns
#the ids for it's neighbors which matches the queens case!
nyctSP$CT2010[1]
nyctSP$CT2010[nb[[1]]]

## take a look at first census tracts and neighbors
tm_shape(nyct[nb[[1]],]) + tm_fill(col = "orange") + 
    tm_shape(nyct[1,]) + tm_fill(col = "red") +
    tm_shape(nyct) + tm_borders(col = "gray")  + 
    tm_text( text = "BoroCT2010")

# look at coordinates
par(bg = 'black')
plot(nb, coordinates(nyctSP), points=FALSE, lwd = 0.5, col = "#DC16C3")


nb2 <- poly2nb(nyctSP[-c(612,806,1623),], queen=TRUE)

##Next, we need to assign weights to each neighboring polygon. In our case, 
#each neighboring polygon will be assigned equal weight 

lw <- nb2listw(nb2, style="W", zero.policy=TRUE)

## see weights of first object sould be evenly distributed
lw$weights[[1]]

# pulling income data
nyctMedInc <-  nyct %>% 
    dplyr::slice(-c(612, 806, 1623)) %>% 
    dplyr::pull(MedIncome)

## adding lag with median income
MedIncLag <- lag.listw(lw, nyctMedInc)

#Now that we have gotten through the concepts of what criteria a neighbor consists of
#and creating those lagged variables. If we plot the lagged median income(mean income
#of neighbors) vs. the median income and get the OLS regression line, that is the 
#Moran's I which is an measure of spatial autocorrelation.

#Let's take our lagged median income and make our plot comparing the median income
#variable. We can see that our Moran's I value is 0.58 which is the correlation
#coefficient between our lagged and non-lagged variable.

lagModel <- lm(MedIncLag ~ nyctMedInc)

## check coefficient
lagModel

# plot against eachother.

data.frame(MedIncLag = MedIncLag,
           nyctMedInc = nyctMedInc) %>% 
    ggplot(aes(x= nyctMedInc, y = MedIncLag)) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    xlab("Median Income") + 
    ylab("Mean Neighbor Median Income") + 
    annotate("text", x = 45000, y = 200000, label = 
                 paste("Moran's I is", coef(lagModel)[2] %>% 
                           round(2) %>% 
                           as.character())) + 
    theme_minimal()

#You can see where neighbors might have some effect on your census tract's median
#household income.
tm_shape(nyct %>% 
             dplyr::slice(-c(612, 806, 1623)) %>% 
             dplyr::mutate(MedIncLag = MedIncLag)) + 
    tm_polygons(col = "MedIncLag", n = 5, style = "jenks",
                title = "Lagged Median Income")

#We can see that the tracts with larges negative differences are
#parks, cemeteries, or JFK airport.

tm_shape(nyct %>% 
             dplyr::slice(-c(612, 806, 1623)) %>% 
             dplyr::mutate(MedIncDiff = nyctMedInc - MedIncLag)) + 
    tm_polygons(col = "MedIncDiff", title = "Median Income Difference",
                palette = "RdBu", n = 7, midpoint = 0,
                style = "jenks")

# There is also a premade function called `moran.test` that will take in a
# numeric vector in our case the median income values from the census tracts
# and the `listw` object which is our lagged value weights we specified
# which in our case we distributed the weights equally among our neighbors.

moran.test(nyctMedInc,lw)

# The previous method was calculated analytically, but we could also calculate
# our Moran's I through Monte Carlo simulation with the `moran.mc` function.
# We feed in the same inputs but this also input how many times we want to run the 
# simulation.
# 
# In the Monte Carlo method, the median income values are randomly assigned to the
# census tracts and a moran's I value is computed for each simulation. We can then compute
# the moran's I values if we randomly placed those original values compared to
# our actual Moran's I.

MC <- moran.mc(nyctMedInc, lw, nsim = 600)
MC

## Moran's I as a function of distance
# can be done, check dnearneigh function


# So far we have been looking at the global scope or one number to determine
# how much clustering/dispersion of the whole data set. Now we will look
# at local clusters with the Local Moran's I.

## Local Moran's I


localMoran <- localmoran(nyctMedInc, lw)

nyctLocal <- nyct %>% dplyr::slice(-c(612, 806, 1623)) %>% cbind(localMoran)

#A positive value for I indicates that a feature has neighboring features with 
#similarly high or low attribute values; this feature is part of a cluster
tm_shape(nyctLocal) + 
    tm_polygons(col = "Ii", lwd = 0.1, 
                palette = "-RdBu", n = 7, 
                midpoint = 0, style = "jenks") 

# GETIS-ORD G

getisG <- localG(nyctMedInc, lw)

nyctLocal <- nyctLocal %>% dplyr::mutate(localG = as.numeric(getisG))

# The higher the z-score, the more intense the clustering of high values(hot spot)
# are being exhibited in your map. On the converse the greater the negative z-scores
# are, the more intense the clustering of low values(cold spot) is being detected.

zbreaksG <- c(min(nyctLocal$localG), -2.58, -1.96, -1.65, 1.65, 1.96, 2.58, 
              max(nyctLocal$localG))

# In our map, we can see that the Upper West side, Upper East side, Midtown, 
#most of Downtown,
# downtown Brooklyn are all high median income clusters. 
#Over in the Bronx most of them are 
# low median income clusters as many parts of Brooklyn and Queens. 

tm_shape(nyctLocal) + 
    tm_polygons(col = "localG", lwd = 0.1, palette = "-RdBu",
                breaks = zbreaksG)

tm_shape(nyctLocal) + 
    tm_polygons(col = "localG", lwd = 0.1, palette = "-RdBu",
                n = 7, midpoint = 0, style = "jenks")


## Regression

nySales <- readr::read_csv( "Data/nycSales.csv")

set.seed(1234)

condos <- nySales %>% dplyr::filter(type == "condo") %>% 
    tidyr::separate(gps_coordinates, c("Lat", "Lon"), " ") %>%
    dplyr::mutate(Lat = as.numeric(Lat),
           Lon = as.numeric(Lon)) %>% 
    dplyr::filter(Lat > 0, borough != "staten_island") %>% 
    dplyr::mutate(z = scale(price),
           zLat = scale(Lat)) %>% 
    dplyr::filter(z < 3,(zLat < -4 | zLat < 4)) %>% 
    dplyr::mutate(num_baths = as.numeric(num_baths),
           num_beds = as.numeric(num_beds),
           num_beds = dplyr::if_else(is.na(num_beds), 0, num_beds),
           borough = as.factor(borough)) %>% 
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::group_by(borough) %>% 
    dplyr::sample_frac(0.14) %>% # sampling 
    dplyr::ungroup()

# check distribution of log(price)
condos %>% ggplot(aes(log(price))) + 
    geom_histogram(fill = "light blue") + 
    theme_minimal()

# normal regression
mod1 <- lm(log(price) ~  num_beds + num_baths + num_sqft + built_date, condos)
summary(mod1)
# RMSE
sqrt(mean(mod1$residuals^2))

par(bg = 'white')
plot(mod1, which = 3)

coefplot::coefplot(mod1, sort = "mag", intercept = FALSE) + 
    theme_minimal()

# check residuals
resids1 <- residuals(mod1)

condoSF <- condos %>%
    dplyr::mutate(resids1 = resids1) %>% 
    # read in a flat file and create a sf object
    st_as_sf(coords = c("Lon", "Lat"),crs = 4326) %>% 
    st_transform(crs = 2263)

# Neighborhood tabulation areas
nycNeighb <- st_read("Shapes/nynta_19a")
nycNeighb <- st_transform(nycNeighb, crs = 2263)

# by mapping out our residuals, we can definitely see that there are other factors
# that our causing some autocorrelation. If you looks up at Harlem and in downtown.

tm_shape(nycNeighb) + tm_borders() + 
    tm_shape(condoSF) + tm_dots(col ="resids1", palette = "-RdBu",
                                n = 7, midpoint = 0, style = "fisher",
                                size =0.1, alpha = 0.5) 

## adding Borough to model

mod2 <- lm(log(price) ~  borough + num_beds + num_baths + num_sqft + built_date, condos)

# Get the summary
summary(mod2)

# compute rmse
sqrt(mean(mod2$residuals^2))

# We'll also see that Manhattan, Brooklyn, and Queens have big coefficients.
plot(mod2, which = 3)
coefplot::coefplot(mod2, sort = "mag", intercept = FALSE) + 
    theme_minimal()

# get residuals
resids2 <- residuals(mod2)
condoSF <- condoSF %>% dplyr::mutate(resids2 = resids2)

##By mapping out our residuals, we'll see that we are doing a bit better,
#but visually you can see that our residuals are still autocorrelated.
#There must be other explanatory variables that we haven't captured yet.

tm_shape(nycNeighb) + tm_borders() + 
    tm_shape(condoSF) + tm_dots(col ="resids2", palette = "-RdBu", 
                                n = 7, midpoint = 0, style = "fisher",
                                size =0.1, alpha = 0.5) 

#We can look to see if our residuals are normally distributed around our OLS regression
#line, but we can see that by mapping them out and with our new knowledge about
#spatial autocorrelation, we can see that our residuals are not spatially distributed 
#randomly.

library(spgwr)

## GWR

condoSP <- as(condoSF, "Spatial")

# first we will calibrate the bandwidth of the kernel that will 
# be used to capture the points for each regression
# (this will take a little while).
# 
# The bandwidth is hard to specify a priori and the preferred approach is
# to carry out a cross-validation, minimizing the root mean square prediction
# error.
# 
# Once the bandwidth has been found, or chosen by hand, the gwr function may be
# used to fit the model with the chosen local kernel and bandwidth.
# 
# This function creates an object that contains a dataframe with the individual coefficient estimates
# for each variable and each location. Also it can run in parallel so we can use the
# `parallel` package to spin up a cluster.
# 
# We first will need to create `condoBW` to select a suitable number of neighbors.
# You can use `gwr.sel` to either choose a bandwidth or number of neighbors. The default
# is to use cross validation, but the Akaike Information Criterion could also be used.


# Calculate the number of cores
no_cores <- parallel::detectCores() - 1 

# Initiate cluster (make sure you have the latest version of rstudio 1.3.1073 right now)
cl <-parallel:: makeCluster(no_cores)

# select band width
condoBW <- gwr.sel(log(price) ~  num_beds + num_baths + num_sqft + built_date, 
                   data=condoSP,gweight = gwr.Gauss, verbose = FALSE, adapt  = TRUE)

# run gwr model (this will take a bit)
condoGWR <-spgwr::gwr(log(price) ~  num_beds + num_baths + num_sqft + built_date, 
                      data=condoSP, adapt = condoBW,
                      gweight = gwr.Gauss, hatmatrix = TRUE, se.fit = TRUE, cl = cl)
# stop cluster
parallel::stopCluster(cl)

# check results

# we can checkout the results for our GWR. There are a lot of values and many of them can be 
# found in this http://desktop.arcgis.com/en/arcmap/10.3/tools/spatial-statistics-toolbox/interpreting-gwr-results.htm
# but the residual sum of squares, quasi-global R2a nd the statistics on the explanatory
# coefficients can be found.

condoGWR

# RMSE Scores
sqrt(mean(condoGWR$SDF$gwr.e^2))

# let's save our results from our gwr as a data.frame and add it as columns
# into our `sf` object from before so we can start exploring it.
condoGWRdf <- as.data.frame(condoGWR$SDF)
condoGWRdf %>% head()

condoComb <- condoSF %>% 
    dplyr::mutate(
        coefnum_beds = condoGWRdf$num_beds,
        coefnum_baths = condoGWRdf$num_baths,
        coefbuilt_date = condoGWRdf$built_date,
        coefnum_sqft = condoGWRdf$num_sqft,
        residuals = condoGWRdf$gwr.e)

# Let's look at the regional coefficients we got for the number of baths. You can see
# that there are areas where the coefficient for a number of bath is much greater
# than other parts of NYC

tm_shape(condoComb %>% 
             dplyr::filter(!is.na(coefnum_baths))) + 
    tm_dots(col ="coefnum_baths", 
            palette = "RdBu", n = 7, midpoint = 0,
            size =0.1, title = "Num Bath Coefficient") + 
    tm_shape(nycNeighb) + tm_borders() + 
    tm_legend(position = c("left", "top"))


# For our build date, we cans ee that some places have a negative effect on condo
# prices while some areas have a positive effect. Here is an instance where maybe
# squaring older buildings and creating a parabolic curve for built date might be
# a helpful transformation.

tm_shape(condoComb %>% 
             dplyr::filter(!is.na(coefbuilt_date))) + 
    tm_dots(col ="coefbuilt_date", 
            palette = "RdBu", n = 7, midpoint = 0,
            size =0.1, title = "Built Date Coefficient") + 
    tm_shape(nycNeighb) + tm_borders() + 
    tm_legend(position = c("left", "top"))

# We can also see for our number of beds, the pattern of coefficients are similar 
# to that of the number of beds, which could mean that there is multicollinearity.

tm_shape(condoComb %>% 
             dplyr::filter(!is.na(coefnum_beds))) + 
    tm_dots(col ="coefnum_beds", 
            palette = "RdBu", n = 7, midpoint = 0,
            size =0.1, title = "Num Beds Coefficient") + 
    tm_shape(nycNeighb) + tm_borders() + 
    tm_legend(position = c("left", "top"))

# we can again map out our residuals and see that visually it looks like we still
# have autocorrelation, but it seems to be less prominent than before.

tm_shape(condoComb %>% 
             dplyr::filter(!is.na(residuals))) + 
    tm_dots(col ="residuals", 
            palette = "RdBu", n = 7, midpoint  = 0,
            size =0.1, title = "GWR Residuals") + 
    tm_shape(nycNeighb) + tm_borders()

#We can see what our average nearest neighbor distance is for our condos for our
#neighbor ratio we got from our gwr which was 0.14. this makes our number of neighbors
#out of ~2,550 that are neighbors as ~350 closest condos. 

#The mean distance is 8,291 ft and the median distance is 6,500 feet.
library(maptools)
condoPPP <- condoComb %>% as("Spatial") %>% as("ppp")

library(spatstat)
mean(nndist(condoPPP, k = 350))
median(nndist(condoPPP, k = 350))

# we can create a neighbor list by distance with the upper
#bound being 6,600 feet
condoDist  <-  dnearneigh(condoComb, 0, 6600) 
condoDist

# we can look for condos with no links
# and remove them
noLinkCondo <- c(7, 465, 2474, 2485)
condoDist2  <-  dnearneigh(condoComb  %>% "["(-noLinkCondo,), 
                           0, 6600)  
condoDist2

par(bg = 'black')

# plotting the condo links
plot(condoDist2, st_coordinates(condoComb %>% 
                                    "["(-noLinkCondo,)), 
     points=FALSE, lwd = 0.5, col = alpha("#DC16C3", 0.05))


# we can run a moran's I again to check the autocorrelation
lw3 <- nb2listw(condoDist2, style="W", zero.policy=TRUE)

MI3  <-  moran.mc(condoComb %>% 
                      "["(-noLinkCondo,) %>% 
                      dplyr::pull(residuals), 
                  lw3, nsim=600,zero.policy=T) 
# the output is .12827 which is indicating no autocorrelation.
MI3
