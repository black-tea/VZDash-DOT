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
library(rgdal)
library(tidyr)
library(ggplot2)

# Set WD
work_dir <- "C:/Users/dotcid034/Documents/GitHub/vzcd-shiny"
setwd(work_dir)

# Dictionary of Column Names
cols <- c('DISTRICT','NAME_ALF','NAME')
names(cols) <- c('cd_boundaries','cpa_boundaries','nc_boundaries')

# Load Data
hin <- read_sf('data/High_Injury_Network.geojson')
cd_boundaries <- read_sf('data/council_districts/CnclDist_July2012_wgs84.shp')
lapd_collisions <- read_sf('data/lapd_collisions/collisions.geojson')
pc <- read_sf('data/prioritized_corridors/pc_05232017_wgs84_.shp')
cpa_boundaries <- read_sf('data/community_planning_areas/CPA_wgs84.shp')
nc_boundaries <- read_sf('data/neighborhood_councils/LACITY_NEIGHBORHOOD_COUNCILS.shp')

# Access socrata api for collisions
# limit to 1000 collisions
d = read_sf('https://data.lacity.org/resource/k8cc-2d49.geojson')

lapd_collisions$date_occ <- as.Date(lapd_collisions$date_occ)

lapd_fatal <- lapd_collisions %>% filter(severity == '1')
lapd_si <- lapd_collisions %>% filter(severity == '2')

##### Combine Bike / Ped columns into one column for crosstabs
# This formula (below) replaces the 'Y' factor with the 'bike' factor
levels(lapd_collisions$bike_inv)[match('Y',levels(lapd_collisions$bike_inv))] <- "bike"
levels(lapd_collisions$bike_inv) <- c(levels(lapd_collisions$bike_inv),'ped')
# This formula (below) replaces the 'Y' factor with the 'ped' factor
levels(lapd_collisions$ped_inv)[match('Y',levels(lapd_collisions$ped_inv))] <- "ped"
levels(lapd_collisions$ped_inv) <- c(levels(lapd_collisions$ped_inv),'bike')
# Coalesce the two columns into one
lapd_collisions$mode <- coalesce(lapd_collisions$ped_inv, lapd_collisions$bike_inv)
lapd_collisions <- lapd_collisions %>% rename(severity = severity)

# When we setup the postgres, this is where i will run the connection script

# Prep Collision Data for Dashboard
VehFatalCt <- lapd_collisions %>%
  filter(!mode %in% c("Ped","Bike"), severity == 1) %>%
  st_set_geometry(NULL) %>%
  tally()
# Fatals by Month
MonthlyFatals <- lapd_collisions %>%
  filter(severity == 1) %>%
  mutate(month = format(date_occ, "%m")) %>%
  group_by(month) %>%
  st_set_geometry(NULL) %>%
  tally()


function(input, output, session) {
  
  ### Functions 
  # Buffer boundary by a distance in ft, return to wgs84
  geom_buff <- function(boundary, ft) {
    geom_nad83 <- st_transform(boundary, 2229) # Convert to NAD83
    geom_nad83 <- st_buffer(geom_nad83, ft) # Buffer
    geom_wgs84 <- st_transform(geom_nad83, 4326) # Convert back to wgs84
    return(geom_wgs84)
  }
  
  # Clip to selected boundary
  geom_clip <- function(segment) {
    st_intersection(segment,geography())
  }
  
  ##### Citywide Dashboard Metrics Output
  # KPIs
  output$DeathsToDate <- renderValueBox({
    valueBox(
      lapd_collisions %>% filter(severity == 1) %>% st_set_geometry(NULL) %>% tally(),
      '2017 Deaths To-Date (insert date object here)',
      color = "black")  
  })
  output$PedDeaths <- renderValueBox({
    valueBox(
      lapd_collisions %>% filter(severity == 1, ped_inv == 'Y') %>% st_set_geometry(NULL) %>% tally(),
      'Pedestrian Deaths',
      icon = icon("male",lib='font-awesome'),
      color = "red")  
  })
  output$BikeDeaths <- renderValueBox({ 
    valueBox(
      lapd_collisions %>% filter(severity == 1, bike_inv == 'Y') %>% st_set_geometry(NULL) %>% tally(),
      'Bicyclist Deaths',
      icon = icon("bicycle",lib='font-awesome'),
      color = "yellow")  
  })
  output$VehDeaths <- renderValueBox({
    valueBox(
      lapd_collisions %>% filter(severity == 1, is.na(bike_inv), is.na(ped_inv)) %>% st_set_geometry(NULL) %>% tally(),
      'Passenger Deaths',
      icon = icon("car",lib='font-awesome'),
      color = "blue")   
  })
  # Yearly Timeline Plot
  output$MonthlyFatalChart <- renderPlot({
    ggplot(data = MonthlyFatals, 
           aes(x=month, y=n, group=1)) + 
      geom_line() + ylab("Fatal Collisions") + 
      xlab("Month") + theme(legend.position="bottom" 
                            ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Monthly Tracking")
  })
  
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
  
  ### Reactive Values
  # Filter geography, if needed
  geography <- reactive({
    
    # Begin Empty
    if (is.null(input$geography_name))
      return()
    
    # Grab the selected geography type and associated name column
    # the 'get' function grabs an object from a str
    geography_selected <- get(input$geography_type)
    column = cols[[input$geography_type]]
    
    # Return the specific geographical boundaries 
    return(geography_selected[(geography_selected[[column]] == input$geography_name),])
  })
  
  # Filter lapd collisions, if needed
  lapd_collisions_r <- reactive({
    # Filter if AreaFilter tab is activated
    if((input$tabs == 'AreaFilter')&(!is.null(input$geography_name))){
      # Geography Filter
      lapd_collisions <- lapd_collisions[geography(),]
      # Date Range Filter
      lapd_collisions %>% filter(date_occ >= input$dateRange[1] & date_occ <= input$dateRange[2])
    } else {
      return(lapd_collisions)
    }
  })  
  
  # HIN reactive variable
  hin_r <- reactive({
    # Clip for Area Filter
    if((input$tabs == 'AreaFilter')&(!is.null(geography()))){
      return(st_intersection(hin, geom_buff(geography(),50)))
    } else {
      return(hin)
    }
  })
  
  # PC reactive variable
  pc_r <- reactive({
    #Clip for Area Filter
    if((input$tabs == 'AreaFilter')&(!is.null(input$geography_name))){
      return(st_intersection(pc, geom_buff(geography(),50)))
    } else {
      return(pc)
    }   
  })
  
  ### Maps
  # Map Object for Project Delivery Tab
  output$projectmap <- renderLeaflet({
    
    lapd_fatal <- lapd_collisions_r() %>% filter(severity == '1')
    
    # Define color palette
    lbls = c( 'Fatal Collisions','High-Injury Network','Priority Corridors')
    pal <- colorFactor(
      palette = c('#f44242','#f44242', '#0E016F'),
      domain = lbls
      )
    
    # Create map
    map <- leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      setView(lng = -118.329327,
              lat = 34.0546143,
              zoom = 12) %>%
      # Add HIN
      addPolylines(
        #color = '#f44242',
        color = ~pal('High-Injury Network'),
        weight = 2,
        opacity = 1,
        data = hin_r(),
        group = 'VZ Streets',
        label = ~paste0(STNAME, ": ", FROM_, " to ", TO_)) %>%
      # Add PC
      addPolylines(
        #color = '#0E016F',
        color = ~pal('Priority Corridors'),
        weight = 2,
        opacity = 1,
        data = pc_r(),
        group = 'VZ Streets') %>%
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = lbls
      ) %>%
      addLayersControl(
        overlayGroups = c('VZ Streets', 'Collisions YTD'),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    # From LAPD Data, If there is at least 1 fatal, add to map
    if(nrow(lapd_fatal) > 0){
      map <- addCircleMarkers(
        map,
        radius = 1,
        fill = TRUE,
        color = ~pal('Fatal Collisions'),
        opacity = 1,
        data = lapd_fatal,
        group = 'Collisions YTD',
        popup = ~paste0('DR#: ',dr_no, '<br>',
                        'Date: ', date_occ, '<br>',
                        'Pedestrian Inv: ', ped_inv, '<br>',
                        'Bicyclist Inv: ', bike_inv, '<br>')
      )
    }
  
    map
  })
  
  # Map Object for Area Filter
  output$vzmap <- renderLeaflet({
    map <- leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite)
    
    map
  })
  
  # Area Filter Map Observer #2
  observe({
    
    geography_r <- geography()
    
    if (!is.null(input$geography_name)) {
      leafletProxy("vzmap") %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolygons(
          data = geography(),
          fill = FALSE
        ) %>%
        # Add filtered HIN
        addPolylines(
          color = '#f44242',
          weight = 3,
          opacity = 1,
          data = hin_r(),
          label = ~paste0(STNAME, ": ", FROM_, " to ", TO_)) %>%
        # Add filtered PC
        addPolylines(
          color = '#0E016F',
          weight = 3,
          opacity = 1,
          data = pc_r()
        ) %>%
        fitBounds(lng1 = as.double(st_bbox(geography_r)[1]),
                  lat1 = as.double(st_bbox(geography_r)[2]),
                  lng2 = as.double(st_bbox(geography_r)[3]),
                  lat2 = as.double(st_bbox(geography_r)[4])
        )
    }
  })
  
  
    # # Get reactive value of lapd_collisions
    # lapd_fatal <- lapd_collisions_r() %>% filter(severity == '1')
    # lapd_si <- lapd_collisions_r() %>% filter(severity == '2')
    #   
    #   
    #   
    # 
    # 
    # # From LAPD Data, If there is at least 1 Severe Injury, add to map
    # if(nrow(lapd_si) > 0){
    #   map <- addCircleMarkers(
    #     map,
    #     radius = 3,
    #     fill = TRUE,
    #     color = 'orange',
    #     opacity = 1,
    #     data = lapd_si,
    #     popup = ~paste0('DR#: ',dr_no, '<br>',
    #                     'Date: ', date_occ, '<br>',
    #                     'Pedestrian Inv: ', ped_inv, '<br>',
    #                     'Bicyclist Inv: ', bike_inv, '<br>')
    #   )
    # }
    # 
    # From LAPD Data, If there is at least 1 fatal, add to map
    # if(nrow(lapd_fatal) > 0){
    #   map <- addCircleMarkers(
    #     map,
    #     radius = 3,
    #     fill = TRUE,
    #     color = 'red',
    #     opacity = 1,
    #     data = lapd_fatal,
    #     popup = ~paste0('DR#: ',dr_no, '<br>',
    #                     'Date: ', date_occ, '<br>',
    #                     'Pedestrian Inv: ', ped_inv, '<br>',
    #                     'Bicyclist Inv: ', bike_inv, '<br>')
    #   )
    # }
    
  
  output$lapd_summary <- renderTable({

    lapd_collisions_r() %>%
      group_by(mode, severity) %>%
      tally() %>%
      st_set_geometry(NULL) %>%
      spread(severity, n)
  })
  
  # # Filter by selected geography
  # # Create x-tabs frequency table
  # # Use as.data.frame.matrix to solidify x-tabs structure
  # as.data.frame.matrix(table(lapd_collisions_r()$mode,
  #                            lapd_collisions_r()$severity,
  #                            exclude=NULL),
  #                      row.names = c('Ped','Bike','Other'))
  # },
  # spacing = 'xs',
  # rownames = TRUE,
  # caption = "Hello",
  # caption.placement = getOption("xtable.caption.placement,","top"))
  
  
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

