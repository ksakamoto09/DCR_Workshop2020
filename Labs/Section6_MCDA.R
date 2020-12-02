library(sf)
library(tmap)
library(raster)
library(ggplot2)
library(dplyr)
library(purrr)
library(readr)

## MCDA
## supermarkets that are going to be targeted for small business support
## Criteria
## far from subway
## low income neighborhoods
## areas that have high snap benefits

nybb <- st_read("Shapes/nybb/")
nyc_supermarkets <- st_read("Shapes/nyc_supermarkets/") %>% st_transform(2263)

subway <- st_read("Shapes/SubwayLines/") %>% st_transform(2263)
subwayUnion <- subway %>%  st_union()
subwayBuffer <- subwayUnion %>% st_buffer( dist = 1320)
subwayBuffer2 <- subwayUnion %>% st_buffer( dist = 1320*2)
subwayDiff <- st_difference(subwayBuffer2,subwayBuffer)


tm_shape(subwayDiff) + 
    tm_fill()  

# subway raster

buffers <- tibble(distances = seq(1320, 5280, length.out = 4))
bufferDF <- buffers %>% mutate(buffer = map(distances, function(x)  subwayUnion %>% st_buffer(dist = x)))

for(i in 1:nrow(bufferDF)){
    if(i == 1) bufferDF[i,"dist"] <- bufferDF[i,"buffer"][[1]]
    else bufferDF[i,"dist"] <- list(st_difference(bufferDF[i,"buffer"][[1]][[1]],bufferDF[i-1,"buffer"][[1]][[1]]))
}

multiBuffer <- bufferDF %>% select(-buffer) %>% st_as_sf()
tm_shape(multiBuffer) + tm_fill(col = "distances")


raster_template = raster(extent(nybb), resolution = 500,
                         crs = st_crs(multiBuffer)$proj4string)
multiBufferRaster = rasterize(multiBuffer, raster_template, field = "distances")
crs(multiBufferRaster) <- CRS("+init=epsg:4326")
multiBufferRaster <- raster::setExtent(multiBufferRaster,ext = extent(nybb %>% st_transform(4326)))

plot(multiBufferRaster)

m <- c(0, 1320, 1,  1320, 2640, 2,  2640, 3960, 3, 3960, 5280, 4)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
subwayReclass <- reclassify(multiBufferRaster, rclmat)
plot(subwayReclass)

library(leaflet)
leaflet() %>% 
    addTiles() %>% 
    addRasterImage(subwayReclass, colors = "Spectral", opacity = .7)

## SNAP

acs <- read_csv(here::here("Data", "SNAP_MedIncome.csv")) %>% 
    mutate(Geo_FIPS = as.character(Geo_FIPS))
nyct <- st_read(here::here("Shapes" ,"nyct2010"))
nyct <- nyct %>% left_join(acs)
library(maptools)
library(spatstat)
nyctCentroid <- st_centroid(nyct)

nyctSP <- nyctCentroid %>% select(OnSNAP) %>% 
    as("Spatial")

nyctPPP <- nyctSP %>% 
    as("ppp")

## kernel density
#5280 feet  = 1 mile
snapDensity <- density(nyctPPP,weights=nyctSP@data[,1],sigma = 5280, eps = 500)
plot(snapDensity, main=NULL, las=1)

snapRaster <- raster(snapDensity, crs = crs(nyct))
plot(snapRaster)
crs(snapRaster) <- CRS("+init=epsg:4326")
snapRaster <- setExtent(snapRaster,ext = extent(nybb %>% st_transform(4326)))

values(snapRaster) <- scale(values(snapRaster))
snapVals <- seq(0, max(densityVals),length.out = 5)

#snapVals <- BAMMtools::getJenksBreaks(densityVals, 5, subset = NULL)

m2 <- c(snapVals[1], snapVals[2], 1,snapVals[2], snapVals[3], 2,
        snapVals[3], snapVals[4],3,snapVals[4], snapVals[5],4)

rclmat2 <- matrix(m2, ncol=3, byrow=TRUE)
snapReclass <- reclassify(snapRaster, rclmat2)
snapReclass <- mask(snapReclass, nybb %>% st_transform(4326))

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(snapReclass),
                    na.color = "transparent")

leaflet() %>% 
    addTiles() %>% 
    addRasterImage(subwayReclass, colors = "Spectral", opacity = .7, group = "Subway") %>% 
    addRasterImage(snapReclass, colors =pal, opacity = .7, group = "SNAP") %>% 
    addLayersControl(
        baseGroups = c("Subway", "SNAP"),
        options = layersControlOptions(collapsed = FALSE)
    )

## Low income Neighborhoods
medIncomeRaster <- fasterize::fasterize(nyct %>% st_transform(4326), field = "MedIncome", subwayReclass)
plot(medIncomeRaster)

incomeVals <- seq(0, max(values(medIncomeRaster),na.rm = TRUE),length.out = 5)

#snapVals <- BAMMtools::getJenksBreaks(densityVals, 5, subset = NULL)

m3 <- c(incomeVals[1], incomeVals[2], 1,incomeVals[2], incomeVals[3], 2,
        incomeVals[3], incomeVals[4],3,incomeVals[4], incomeVals[5],4)

rclmat3 <- matrix(m3, ncol=3, byrow=TRUE)
medIncomeReclass <- reclassify(medIncomeRaster, rclmat3)
plot(medIncomeReclass)

leaflet() %>% 
    addTiles() %>% 
    addRasterImage(subwayReclass, colors = "Spectral", opacity = .7, group = "Subway") %>% 
    addRasterImage(snapReclass, colors =pal, opacity = .7, group = "SNAP") %>% 
    addRasterImage(medIncomeReclass,colors = "Spectral", opacity = .7, group = "Income") %>% 
addLayersControl(
    baseGroups= c("Subway", "SNAP", "Income"),
    options = layersControlOptions(collapsed = FALSE)
)

## Combining layers

snapReclassResample <- resample(snapReclass, subwayReclass)

plot(snapReclassResample + subwayReclass + medIncomeReclass)

MCDA<- overlay(snapReclassResample,
               subwayReclass,
               medIncomeReclass,
                      fun=function(r1, r2, r3){return((r1*0.5)+(r2*.3)+(r3*.2))})

plot(MCDA)

leaflet() %>% 
    addTiles() %>% 
    addRasterImage(subwayReclass, colors = "Spectral", opacity = .7, group = "Subway") %>% 
    addRasterImage(snapReclass, colors =pal, opacity = .7, group = "SNAP") %>% 
    addRasterImage(medIncomeReclass,colors = "Spectral", opacity = .7, group = "Income") %>% 
    addRasterImage(MCDA,colors = "Spectral", opacity = .7, group = "MCDA") %>% 
    addLayersControl(
        baseGroups = c("Subway", "SNAP", "Income","MCDA"),
        options = layersControlOptions(collapsed = FALSE)
    )


##spatial join or extract
nyc_supermarkets

nyc_supermarkets <- nyc_supermarkets %>% mutate(MCDA = extract(MCDA, nyc_supermarkets))

palMarket <- colorNumeric(c("#14EF07", "#9F9F9F", "#FF60E2"), nyc_supermarkets$MCDA,
                    na.color = "transparent")

leaflet() %>% 
    addTiles() %>% 
    addRasterImage(subwayReclass, colors = "Spectral", opacity = .7, group = "Subway") %>% 
    addRasterImage(snapReclass, colors =pal, opacity = .7, group = "SNAP") %>% 
    addRasterImage(medIncomeReclass,colors = "Spectral", opacity = .7, group = "Income") %>% 
    addRasterImage(MCDA,colors = "Spectral", opacity = .7, group = "MCDA") %>% 
    addCircleMarkers(data = nyc_supermarkets %>% st_transform(4326), radius = .5,color = ~palMarket(nyc_supermarkets$MCDA),opacity = 1) %>% 
    addLayersControl(
        baseGroups = c("Subway", "SNAP", "Income","MCDA"),
        options = layersControlOptions(collapsed = FALSE)
    )
