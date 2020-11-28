## save PROJ4 Strings

Area_Albers <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
Area_Equidistant <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m no_defs"
Area_Lambert <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m no_defs"
Area_NYStatePlane <- "+proj=lcc +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"

# Names them and save as a list
projections <- setNames(c(Area_Albers, Area_Lambert, Area_Equidistant, Area_NYStatePlane),
                        c("Area_Albers", "Area_Lambert","Area_Equidistant", "Area_NYStatePlane")) %>% 
    as.list() 

# read in us counties
usCounties <- st_read("Shapes/us_county",stringsAsFactors = FALSE) 

# if I wanted to do it one at a time
st_transform(usCounties, Area_Albers) %>% st_area()

# create function to reprojecting
massReproject <- function(shape, projection){
    # we have to reporject and recalulate the area
    st_transform(shape, projection) %>% st_area()
}

# map through projections

areaCalc <- purrr::map_df(projections, ~massReproject(usCounties, .x)) %>% 
    # units matter! convert from meters to feet
    mutate_at(vars(Area_Albers, Area_Equidistant, Area_Lambert), function(x) x*10.7639) %>% 
    # add back key
    mutate(GEOID = usCounties$GEOID) %>% 
    # create percent diff
    mutate(Albers_pDiff = (Area_Albers-Area_Albers)/Area_Albers,
           Lambert_pDiff = (Area_Lambert-Area_Albers)/Area_Albers,
           Equidistant_pDiff = (Area_Equidistant-Area_Albers)/Area_Albers,
           NYStatePlane_pDiff = (Area_NYStatePlane-Area_Albers)/Area_Albers)
## join data
usCounties <- usCounties %>% left_join(areaCalc %>% dplyr::select(5:9)) 
