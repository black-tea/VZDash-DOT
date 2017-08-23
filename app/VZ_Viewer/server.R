# VZ Viewer Server Code

library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit

# Set WD
work_dir <- "C:/Users/dotcid034/Documents/GitHub/vzcd-shiny/app/VZ_Viewer"
setwd(work_dir)

# Dictionary of Column Names
cols <- c('DISTRICT','NAME_ALF','NAME')
names(cols) <- c('cd_boundaries','cpa_boundaries','nc_boundaries')

# Load Data
hin <- rgdal::readOGR('data/json/High_Injury_Network.geojson', "OGRGeoJSON")
cd_boundaries <- rgdal::readOGR('data/shp/council_districts')
lapd_collisions <- rgdal::readOGR('data/shp/lapd_collisions')
pc <- rgdal::readOGR('data/shp/prioritized_corridors')
cpa_boundaries <- rgdal::readOGR('data/shp/community_planning_areas')
nc_boundaries <- rgdal::readOGR('data/shp/neighborhood_councils')

# When we setup the postgres, this is where i will run the connection script


function(input, output, session) {

  # Geography Type select input box
  output$geography_typeSelect <- renderUI({
    
    selectInput("geography_type", "Type of Geography",
                c("Council District" = "cd_boundaries",
                  "Neighborhood Council" = "nc_boundaries",
                  "Community Plan Area" = "cpa_boundaries"
                  ),
                  selected = "cd_boundaries")
  })

 
   # Geography Name select input box
   output$geography_nameSelect <- renderUI({
 
     # Begin Empty
     if (is.null(input$geography_type))
       return()
     
     # Grab the selected geography type and associated name column
     geography_selected <- get(input$geography_type)
     print("geography_selected")
     print(geography_selected)
     column = cols[[geography_selected]]
     print(column)
     print("geography_names")
     print(geography_selected$column)
     geography_names <- sort(unique(geography_selected$column))
     print("geography_names")
     print(geography_names)
     # Add names, so that we can add all=0
     names(geography_names) <- geography_names
     geography_names <- c(All = 0, geography_names)
     
     # Generate Geography Name Input Box
     selectInput("geography_name", "Select:", choices = geography_names)
   })
   
}
   
#   # Reactive Function to Select the boundary of the Geography
#   geography <- reactive({
#     if (is.null(input$geography_name))
#       return()
#     
#     # Return all if "0" is selected
#     if (as.numeric(input$geography_name) == 0)
#       return(input$geography_type)
#     
#     # Otherwise, filter by the selected CD num
#     # MAY NEED TO MAKE 'cdnum' a string instead of number
#     geography_selected[column == input$geography_name, ]
#   })
#   
#   # Render the Leaflet Map
#   output$vzmap <- renderLeaflet({
#     
#     # Run geography function to return selected geography
#     geography <- geography()
#     
#     if (length(geography) == 0)
#       return(NULL)
#     
#     # Filter HIN by Geography
#     hin.filter <- hin[geography, ]
#     
#     map <- leaflet() %>%
#       addProviderTiles(providers$Stamen.TonerLite,
#                        options = providerTileOptions(noWrap = TRUE)
#       ) %>%
#       addPolygons(
#         data = geography
#         #fill = FALSE,
#         #label = ~DISTRICT
#       ) %>%
#       addPolylines(
#         color = '#f44242',
#         weight = 3,
#         opacity = 1,
#         data = hin.filter,
#         label = ~paste0(STNAME, ": ", FROM_, " to ", TO_)
#       )
# 
#     # Generate the map 
#     map
#     
#   })
# }

