
# Load library
library(sf)
library(dplyr)

# read in shapefile
nycbb <- st_read("Shapes/nybb")

# print out sf object
nycbb

#check class of sf object
class(nycbb)

# look at the geometry
class(nycbb$geometry)

# just plot out Boroname
plot(nycbb[,"BoroName"])

# you can drop geometry and just get table
nycbb %>% st_drop_geometry()

# filtering with dplyr works hurray!

nycbb %>% select(BoroName)

nycbb %>% filter(Shape_Area < 1500000000)

# methods with sf
methods(class = "sf")

