library(leaflet)
library(jsonlite)
library(curl)
library(rgdal)
library(sp)
library(rgeos)

work_dir <- "C:/Users/dotcid034/Documents/GitHub/vzcd-shiny/quickmap-example"
setwd(work_dir)

# tutorial from https://rstudio.github.io/leaflet/json.html
hin <- rgdal::readOGR('json/High_Injury_Network.geojson', "OGRGeoJSON")
cd_boundaries <- rgdal::readOGR('shp/council_districts')
lapd_collisions <- rgdal::readOGR('shp/lapd_collisions')
pc <- rgdal::readOGR('shp/prioritized_corridors')

##### clipping and Subsetting

# Subset CD layer by a specific district no
cd_boundaries.select <- cd_boundaries[cd_boundaries$DISTRICT == '6',]

# Filters
hin.filter.cd <- hin[cd_boundaries.select,] 
lapd_collisions.filter <- lapd_collisions[cd_boundaries.select,]

### Clips
# when you clip, attribute information doesn't make it over; need to fix
hin.clip.cd <- gIntersection(hin, cd_boundaries.select, byid = TRUE, drop_lower_td = TRUE)
pc.clip.cd <- gIntersection(pc, cd_boundaries.select, byid = TRUE, drop_lower_td = TRUE)

##### Create the Map

leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap = TRUE)
  ) %>%
  addPolygons(
    data = cd.select,
    #fill = FALSE,
    label = ~DISTRICT
  ) %>%
  addPolylines(
    color = '#f44242',
    weight = 3,
    opacity = 0.5,
    data = hin,
    label = ~paste0(STNAME, ": ", FROM_, " to ", TO_)
  ) %>%
  addPolylines(
    color = '#f44242',
    weight = 3,
    opacity = 1,
    data = hin.clip.cd
  ) %>%
  addPolylines(
    color = '337AFF',
    opacity = 1,
    data = pc.clip.cd
  )
#  ) %>% 
#  addMarkers(
#    data=lapd.collisions.filter
#  )


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