library(leaflet)
library(jsonlite)
library(curl)
library(rgdal)

work_dir <- "C:/Users/dotcid034/Documents/GitHub/vzcd-shiny/quickmap-example"
setwd(work_dir)

# tutorial from https://rstudio.github.io/leaflet/json.html
hin <- rgdal::readOGR('json/High_Injury_Network.geojson', "OGRGeoJSON")
cd <- rgdal::readOGR('shp/council_districts')

# Subsets
cd1 <- cd[cd$DISTRICT == '1',]
hin_cd1 <- hin[cd1,] 

leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap = TRUE)
  ) %>%
  addPolygons(
    data = cd1,
    label = ~DISTRICT
  ) %>%
  addPolylines(
    color = '#f44242',
    weight = 3,
    opacity = 1,
    data = hin_cd1,
    label = ~paste0(STNAME, ": ", FROM_, " to ", TO_))

#####
# EVERYTHING BELOW IS FROM THE TUTORIAL
#####

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  states$name, states$density
) %>% lapply(htmltools::HTML)

leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
            position = "bottomright")