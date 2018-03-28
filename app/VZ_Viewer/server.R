############################
# VZ Dashboard Server Code #
############################

##### Setup
library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) 
library(sp)
library(rgeos)
library(sf)
library(mapview)
library(webshot)
library(htmlwidgets)
library(units)
library(xtable)
library(gmodels)

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
lapd_collisions <- st_as_sf(lapd_collisions)

lapd_fatal <- lapd_collisions %>% filter(collision_ == '1')
lapd_si <- lapd_collisions %>% filter(collision_ == '2')

##### Combine Bike / Ped columns into one column for crosstabs
# This formula (below) replaces the 'Y' factor with the 'bike' factor
levels(lapd_collisions$bike_inv)[match('Y',levels(lapd_collisions$bike_inv))] <- "bike"
levels(lapd_collisions$bike_inv) <- c(levels(lapd_collisions$bike_inv),'ped')
# This formula (below) replaces the 'Y' factor with the 'ped' factor
levels(lapd_collisions$ped_inv)[match('Y',levels(lapd_collisions$ped_inv))] <- "ped"
levels(lapd_collisions$ped_inv) <- c(levels(lapd_collisions$ped_inv),'bike')
# Ultimately, what I need to do is 'coalesce' the two columns into one
lapd_collisions$mode <- coalesce(lapd_collisions$ped_inv, lapd_collisions$bike_inv)

# When we setup the postgres, this is where i will run the connection script


function(input, output, session) {

  ##### Geography Type select input box
  output$geography_typeSelect <- renderUI({
    
    selectInput("geography_type", "Geography Type",
                c("Council District" = "cd_boundaries",
                  "Neighborhood Council" = "nc_boundaries",
                  "Community Plan Area" = "cpa_boundaries"
                  ),
                  selected = "cd_boundaries")
  })
  
 
   ##### Geography Name select input box
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
   
   ##### Output: Stats Content below map
   output$geography_calc <- renderUI({
     
     # for multi-line text, use renderUI instead of rendertext
     geography_display <- paste(input$geography_type, ": ", paste(input$geography_name))
     str1 <- paste("Miles of HIN: ", toString(nad83_calc(geom_clip(hin))))
     str2 <- paste("Miles of PC: ", toString(nad83_calc(geom_clip(pc))))
     HTML(paste(geography_display, str1, str2, sep = '<br/>'))
     
   })
   
   ##### Reactive Function: Filter the geography (if needed)
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
  
  ##### Function: Buffer boundary by a distance in ft, return to wgs84
  geom_buff <- function(boundary, ft) {
    geom_nad83 <- st_transform(boundary, 2229) # Convert to NAD83
    geom_nad83 <- st_buffer(geom_nad83, ft) # Buffer
    geom_wgs84 <- st_transform(geom_nad83, 4326) # Convert back to wgs84
    return(geom_wgs84)
  }
    
  ##### Function: Clip to selected boundary
  geom_clip <- function(segment) {
    st_intersection(segment,geography())
  }
  
  
  ##### Function: Segment length in NAD83 Mi
  nad83_calc <- function(segment) {
    
    # NULL case
    if (length(segment$geometry) == 0)
      return(0)
    
    # Create NAD83 / California Zone 5 Version of the clipped HIN
    seg_nad83 <- st_transform(segment, 2229)
    
    # Get NAD83 length (ft), sum all geometries, convert to mi
    seg_length <- round((as.numeric(sum(st_length(seg_nad83$geometry))) * 0.000189394), digits = 2) 
    
    # Return final length
    return(seg_length)
  }
  
  ##### Reactive Function: Map object that depends on the selections
  map <- reactive({ leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite
                       #options = providerTileOptions(noWrap = TRUE)
      ) %>%
      
      # Add the boundary
      addPolygons(
        data = geography(),
        fill = FALSE
        #label = ~DISTRICT
      ) %>%
      
      # Add filtered HIN
      addPolylines(
        color = '#f44242',
        weight = 3,
        opacity = 1,
        data = st_intersection(hin, geom_buff(geography(),50)), # buffer geography by 50ft & clip
        label = ~paste0(STNAME, ": ", FROM_, " to ", TO_)
      ) %>%
      
      # Add filtered PC
      addPolylines(
        color = '#0E016F',
        weight = 3,
        opacity = 1,
        data = st_intersection(pc, geom_buff(geography(),50)) # buffer geography by 50ft & clip
      )
    
  })
    # # Set Zoom Options
    # map <- map %>% mapOptions(zoomToLimits = "always")
    # 
    # # Generate the map
    # map


  # Render the Leaflet Map (based on reactive map object)
  output$vzmap <- renderLeaflet({
    map()
    #print(hin_nad83.clip())
  })
  
  # Observe Event: Adding LAPD KSI to Map
  observeEvent(input$geography_name, {
    proxy <- leafletProxy("vzmap")
    
    # Evaluate to check for any LAPD SI in area; if so, plot
    if(nrow(lapd_si[geography(),]) > 0){
      proxy %>% 
        addCircleMarkers(
          radius = 3,
          fill = TRUE,
          color = 'orange',
          opacity = 1,
          data = lapd_si[geography(),],
          popup = ~paste0('DR#: ',dr_no, '<br>',
                          'Date: ', date_occ, '<br>',
                          'Pedestrian Inv: ', ped_inv, '<br>',
                          'Bicyclist Inv: ', bike_inv, '<br>'
          )
        )
    } 
    
    # Evaluate to check for any LAPD fatals in area; if so, plot
    if(nrow(lapd_fatal[geography(),]) > 0){
      proxy %>% 
        addCircleMarkers(
          radius = 3,
          fill = TRUE,
          color = 'red',
          opacity = 1,
          data = lapd_fatal[geography(),],
          popup = ~paste0('DR#: ',dr_no, '<br>',
                          'Date: ', date_occ, '<br>',
                          'Pedestrian Inv: ', ped_inv, '<br>',
                          'Bicyclist Inv: ', bike_inv, '<br>'
                          )
        )
    }
    
  })
  
  # later change to reactiveEvent
  # observeEvent(input$geography_name, {
  #   
  #   # Create X-Tabs Table, filter by selected geography
  #   mode_xtabs <- table(lapd_collisions[geography(),]$mode, lapd_collisions[geography(),]$collision_)
  #   xtable(mode_xtabs)
  #     
  # })

  
  output$lapd_summary <- renderTable({

    # Filter by selected geography
    # Create x-tabs frequency table
    # Use as.data.frame.matrix to solidify x-tabs structure
    as.data.frame.matrix(table(lapd_collisions[geography(),]$mode,
                               lapd_collisions[geography(),]$collision_,
                               exclude=NULL),
                         row.names = c('Ped','Bike','Other'))
    },
    spacing = 'xs',
    rownames = TRUE,
    caption = "Hello",
    caption.placement = getOption("xtable.caption.placement,","top"))

  
  ##### Generate the Report
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
      
      # Getting errors; need to figure this out.
      #webshot::install_phantomjs()
      saveWidget(map(), "temp.html", selfcontained=FALSE)
      webshot("temp.html", file = 'mapPlot.png', cliprect='viewport')
      
      
      # Setup parameters to pas to Rmd document
      params <- list(goegraphy_type = input$geography_type,
                     geography_name = input$geography_name,
                     map = geography(),
                     hin = geom_clip(hin),
                     pc = geom_clip(pc)
                     )
      
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

