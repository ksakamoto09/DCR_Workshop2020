library(tmap)

tm_shape(usCounties, projection = 2163, unit = "mi") + 
    tm_polygons(col = "Lambert_pDiff", lwd = 0.1,
                palette = "RdBu", n = 7, midpoint = 0, style = "fisher") +
    tm_compass(position = c("right","bottom"),
               text.color = "grey",
               color.dark = "grey",
               size = 1) +
    tm_scale_bar(position = c("center", "bottom"),
                 text.color = "grey",
                 color.dark = "grey") +
    tm_legend(position=c("left", "bottom"),
              just = "right") + 
    tm_layout(inner.margins = c(0.1,0.08,0.01,0.08))+ 
    tm_credits(text = "US Census Bureau, 2020", 
               size = 0.5,align = "right")

tm_shape(usCounties, projection = 2163, unit = "mi") + 
    tm_polygons(col = "Equidistant_pDiff", lwd = 0.1, palette = "RdBu", n = 7, midpoint = 0, style = "fisher") + 
    tm_compass(position = c("right","bottom"),
               text.color = "grey",
               color.dark = "grey",
               size = 1) +
    tm_scale_bar(position = c("center", "bottom"),
                 text.color = "grey",
                 color.dark = "grey") +
    tm_legend(position=c("left", "bottom"),
              just = "right") + 
    tm_layout(inner.margins = c(0.1,0.08,0.01,0.08))+ 
    tm_credits(text = "US Census Bureau, 2020", 
               size = 0.5,align = "right")

tm_shape(usCounties, projection = 2163, unit = "mi") + 
    tm_polygons(col = "NYStatePlane_pDiff", lwd = 0.1,palette = "RdBu", 
                n = 4, midpoint = 0, style = "fisher",
                title = "Percent Difference") + 
    tm_compass(position = c("right","bottom"),
               text.color = "grey",
               color.dark = "grey",
               size = 1) +
    tm_scale_bar(position = c("center", "bottom"),
                 text.color = "grey",
                 color.dark = "grey") +
    tm_legend(position=c("left", "bottom"),
              just = "right") + 
    tm_layout(title = "NY State Plane Areal Distortion",
              inner.margins = c(0.1,0.08,0.01,0.08)) + 
    tm_credits(text = "US Census Bureau, 2020", 
               size = 0.5,align = "right")

library(leaflet)


pal <- colorNumeric("RdBu", domain = as.numeric(usCounties$Lambert_pDiff))

leaflet(usCounties%>% st_transform(4326)) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolygons(fillColor = ~pal(as.numeric(Lambert_pDiff)), stroke = NA)


