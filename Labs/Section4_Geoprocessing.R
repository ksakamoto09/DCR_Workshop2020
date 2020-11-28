library(sf)

nyc_supermarkets <- st_read("Shapes/nyc_supermarkets/") %>% st_transform(2263)

## Geometric confirmations (doesn't create new geometries)
## Intersects 
st_intersects(nyc_supermarkets, nycbb)
st_intersects(nyc_supermarkets, nycbb, sparse = FALSE) %>% head()

## opposite of intersect
st_disjoint(nyc_supermarkets, nycbb, sparse = FALSE) %>% head()

## Spatial Join
marketJoin <- st_join(nyc_supermarkets, nycbb)
?st_join

library(dplyr)

## aggreate basically disolves features. 
marketJoin %>% aggregate(by = nycbb, FUN = min)
marketJoin %>% aggregate(by = nycbb, FUN = mean)

## Geometric operations creates new Geometries
nycbb_centroid <- st_centroid(nycbb)

st_distance(nyc_supermarkets[1,], nycbb_centroid)

tm_shape(nycbb) +tm_polygons(col = "white") + 
    tm_shape(nycbb_centroid) + tm_symbols(col = "blue", size = .2) + 
    tm_shape(nyc_supermarkets %>% slice(1:10)) + 
    tm_symbols(col = "red", size = .2, shape = 1)

## Simplification can save compute

nyc_simp <- st_simplify(nycbb, dTolerance = 500)
?st_simplify
plot(nyc_simp["BoroName"])
plot(nycbb["BoroName"])


#Now we can compare the space allocation

object.size(nycbb)
object.size(nyc_simp)

tm_shape(nyc_simp) +tm_polygons(col = "white") + 
    tm_shape(nycbb_centroid) + tm_symbols(col = "blue", size = .2) + 
    tm_shape(nyc_supermarkets %>% slice(1:10)) + 
    tm_symbols(col = "red", size = .2, shape = 1)

#The issue with our previous function is that topology between geometries
#are not preserved. The mapshaper version will actually preserve the
#relationship between geometries with the `keep_shapes` argument set to TRUE.

nyc_ms <- rmapshaper::ms_simplify(nycbb, keep = 0.01,
                                  keep_shapes = TRUE)

tm_shape(nyc_ms) + tm_polygons()

# Create Buffers

nycbb_buffers <- st_buffer(nyc_ms, dist = 1320)
nycbb_buffers

tm_shape(nyc_ms) + 
    tm_polygons(col = "white") +
    tm_shape(nycbb_buffers) +
    tm_polygons( col = "BoroName", alpha = 0.7) 

## Subset brooklyn
BK <- nycbb %>% filter(BoroName == "Brooklyn")

# create a new geometry
bkSelect <- st_intersection(x = nyc_supermarkets, y = BK)

# see the intersected features
tm_shape(nyc_ms) + tm_polygons() + 
    tm_shape(bkSelect) + tm_symbols(size = 0.2, shape = 1, col = "purple")

## Practice

# create a quarter mile buffer around subway lines
# Get supermarkets within subway buffer
# How many are there
# How many are outside the buffer area?
# How many in each borough are in the buffer area?


subway <- st_read("Shapes/SubwayLines/") %>% st_transform(2263)

subwayBuffer <- st_buffer(subway, dist = 1320)

tm_shape(subwayBuffer) +
    tm_polygons(alpha = 0.5)

marketSubway <- st_intersection(nyc_supermarkets, subwayBuffer)

#keep distince markets in buffer
marketSubway <- marketSubway %>% distinct(.keep_all = TRUE)

tm_shape(nyc_simp) +
    tm_borders() + 
    tm_shape(subwayBuffer) +
    tm_polygons(alpha = 0.5) +
    tm_shape(marketSubway) + 
    tm_symbols(size =.1)

# ones outside buffer
nrow(nyc_supermarkets) - nrow(marketSubway)

## join borough data
marketSubway <- st_join(marketSubway, nycbb)

marketSubway %>% group_by(BoroName) %>% count()
