

library(sf)
library(yach)
library(cartography)
library(leaflet)
library(htmlwidgets)
dir.create(path = "docs")
dir.create(path = "docs/img")

mat <- readRDS("data/wikimat.rds")

ctry <- readRDS("data/ctry.rds")
ocean <- readRDS("data/ocean.rds")

days <- seq(as.Date("2017-01-1"), as.Date("2017-12-31"), "days")
cols <- carto.pal("red.pal",20)
for (i in unique(ctry$iso_a2)){
  if(max(row.names(mat)==i)==1){
    png(filename = paste0("docs/img/",i,"_CH.png"), width = 650, height = 150)
    par(mar=c(0,0,0,0))
    calendarHeat(days, mat[row.names(mat)==i,], title=ctry$name[ctry$iso_a2==i],
                 colors = cols, ncolors = 40)
    dev.off()
  }
}

df <- data.frame(id = row.names(mat),v= rowSums(mat))
ctry <- merge(ctry[,c('iso_a2', "name")], df,
               by.x="iso_a2", by.y = "id", all.x=T)
ctry <- ctry[order(ctry$v, decreasing = T),]
xy <- st_coordinates(st_centroid(ctry, of_largest_polygon = T))
ctry$popup <- paste0("<img src='img/",ctry$iso_a2,"_CH.png'>")
crs <- leafletCRS(crsClass = "L.Proj.CRS", code = "ESRI:54009",
                  proj4def = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs",
                  resolutions = 2^(16:0))


rad <- sqrt(ctry$v[c(1,5,20,150)])/150

m <- leaflet(padding = 0, width = 1200, height = 600,
             options = leafletOptions(crs = crs,maxZoom = 4)) %>%
  fitBounds(lng1 = -180, lng2 = 180, lat1 = 90, lat2 = -90)%>%
  addPolygons(data=ocean, opacity = 0,  color = "#EEF5FA",
              fill = T, fillColor ="#EEF5FA", fillOpacity = 100,
              options = list(clickable = FALSE))%>%
  addGraticule(style = list(color = "#333", weight = 0.25),interval=25)%>%
  addPolygons(data = ctry, opacity = 100, color = "#FAFCFA",
              weight = 0.5,popup = NULL,
              options = list(clickable = FALSE),
              fill = T, fillColor = "#B3C4B3",
              fillOpacity = 100) %>%
  addCircleMarkers(lng = xy[,1],
                   lat = xy[,2],
                   radius = sqrt(ctry$v)/150, weight = 1,
                   stroke = T, opacity = 100,
                   fill = T, fillColor = "#C70003",
                   fillOpacity = 100,
                   popup =ctry$popup,
                   popupOptions = list(maxWidth=650, minWidth=650),
                   color = "white", dashArray = "grey") %>%
  addMarkers(lng = -179, lat = 6,
             icon = NULL,options=markerOptions(opacity=0),
             label = "Number of Views (millions)",
             labelOptions = labelOptions(noHide = T, textsize = "12px",
                                         opacity = 1, textOnly = T)) %>%
  addCircleMarkers(lng = -170 + c(0,cumsum(rad)[1:3]),
                   lat = -5,
                   radius = sqrt(ctry$v[c(1,5,20,150)])/150, weight = 1,
                   stroke = T, opacity = 100,
                   fill = T, fillColor = "#C70003",
                   fillOpacity = 100,   options = list(clickable = FALSE),
                   color = "white", dashArray = "grey") %>%
  addMarkers(lng =  -170 +c(0,cumsum(rad)[1:3]) - rad/3,
             lat = -15,
             options=markerOptions(opacity=0),
             label = c("15.76", "7.34", "4.11","1.03" ),
             labelOptions = labelOptions(noHide = T, textsize = "10px",
                                         opacity = 1, textOnly = T))%>%
  addMarkers(lng = -179, lat = 20,
             options=markerOptions(opacity=0),
             label = htmltools::HTML( "Wikipedia</br>Countries' Pages"),
             labelOptions = labelOptions(noHide = T, textsize = "18px",
                                         opacity = 1, textOnly = T))

m


# relative path does not work with saveWidget...
saveWidget(widget = m, file = "/home/tim/Documents/prj/calandarheatmap/docs/map.html",
           title = "Wikipedia Views of Countries Pages",
           selfcontained = F, background = "black")


write(append = T,file = "docs/map_files/leaflet-0.7.7/leaflet.css", x = ".leaflet-container {
    background-color:rgba(255,0,0,0.0);
}")
