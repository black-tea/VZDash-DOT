# VZ Viewer Server Code

library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit
library(sp)
library(rgeos)
library(sf)
library(mapview)

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

# Reformat sp objects to sf objects
hin <- st_as_sf(hin)
cd_boundaries <- st_as_sf(cd_boundaries)
pc <- st_as_sf(pc)
cpa_boundaries <- st_as_sf(cpa_boundaries)
nc_boundaries <- st_as_sf(nc_boundaries)

# When we setup the postgres, this is where i will run the connection script


function(input, output, session) {

  # Geography Type select input box
  output$geography_typeSelect <- renderUI({
    
    selectInput("geography_type", "Geography Type",
                c("Council District" = "cd_boundaries",
                  "Neighborhood Council" = "nc_boundaries",
                  "Community Plan Area" = "cpa_boundaries"
                  ),
                  selected = "cd_boundaries")
  })
  
 
   # Geography Name select input box
   # Returns the name of the selected geography
   output$geography_nameSelect <- renderUI({
     
     # Begin Empty
     if (is.null(input$geography_type))
       return()

     
     # Grab the selected geography type and associated name column
     geography_selected <- get(input$geography_type)
     
     # Grab the text version (not the object) of the selected geography
     # to look up the appropriate column value
     column = cols[[input$geography_type]]
     
     # sf (not sp) package version of the same command
     geography_names <- sort(unique(geography_selected[[column]]))
     
     # Generate Geography Name Input Box
     selectInput("geography_name", "Geography Name", choices = geography_names)
   })
   
   # Reactive Function to Filter the geography (if needed)
   geography <- reactive({
     
     # Begin Empty
     if (is.null(input$geography_name))
       return()
     
     # Grab the selected geography type and associated name column
     # the 'get' function grabs an object from a str
     geography_selected <- get(input$geography_type)
     column = cols[[input$geography_type]]
     
     # Return the specific geographical boundaries 
     print(geography_selected[(geography_selected[[column]] == input$geography_name),])
   })

  # Render the Leaflet Map
  output$vzmap <- renderLeaflet({

    # Run geography function to return selected geography
    geography <- geography()

    if (length(geography) == 0)
      return(NULL)

    # Filters
    #hin.filter <- hin[geography, ]
    #pc.filter <- pc[geography, ]
    
    # Clips
    hin.clip <- st_intersection(hin, geography)
    pc.clip <- st_intersection(pc, geography)

    map <- leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      
      # Add the boundary
      addPolygons(
        data = geography
        #fill = FALSE,
        #label = ~DISTRICT
      ) %>%
      
      # Add filtered HIN
      addPolylines(
        color = '#f44242',
        weight = 3,
        opacity = 1,
        data = hin.clip,
        label = ~paste0(STNAME, ": ", FROM_, " to ", TO_)
      ) %>%
     
     # Add filtered PC
     addPolylines(
       color = '#0E016F',
       weight = 3,
       opacity = 1,
       data = pc.clip
     )

    # Set Zoom Options
    map <- map %>% mapOptions(zoomToLimits = "always")
    
    # Getting errors; need to figure this out.
    #mapshot(map, file = '~/mapPlot.png')
    
    # Generate the map
    map

  })
  
  # Generate the Report
  output$report <- downloadHandler(
    filename = 'report.html',
    content = function(file) {
      
      src <- normalizePath('Report.Rmd')
      
      # Copy the report file to a temporary directory before processing it, in 
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'Report.Rmd', overwrite = TRUE)
      
      # Setup parameters to pas to Rmd document
      params <- list(goegraphy_type = input$geography_type,
                     geography_name = input$geography_name,
                     map = get(input$geography_type))
      
      # Knit the document, passing in the 'params' list, and eval it in a
      # chile of the global environment (this isolates the code in the doucment
      # from the code in this app)
      out <- rmarkdown::render('Report.Rmd',
                               params = params
                               )
      file.rename(out,file)

    }
  )
}

