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

### Load Data
# Collisions
lapd_collisions <- read_sf('data/lapd_collisions/collisions.geojson')
collisions_2017 <- read_sf('data/lapd_collisions/2017collisions.geojson')
# VZ network files
hin <- read_sf('data/High_Injury_Network.geojson')
pc <- read_sf('data/prioritized_corridors/pc_05232017_wgs84_.shp')
# Boundary files
cd_boundaries <- read_sf('data/council_districts/CnclDist_July2012_wgs84.shp')
cpa_boundaries <- read_sf('data/community_planning_areas/CPA_wgs84.shp')
nc_boundaries <- read_sf('data/neighborhood_councils/LACITY_NEIGHBORHOOD_COUNCILS.shp')
city_boundary <- read_sf('data/city_boundary/city_boundary.shp')
# Infrastructure
highvis_xwalks <- read_sf('data/crosswalks/crosswalks.shp')
lpi <- read_sf('data/lpi/lpi.shp')
paddle_signs <- read_sf('data/paddle_signs/paddle_signs.shp')
pafb <- read_sf('data/pafb/pafb.shp')
ped_islands <- read_sf('data/ped_islands/ped_islands.shp')
scrambles <- read_sf('data/scrambles/scrambles.shp')
int_tight <- read_sf('data/int_tightening/int_tightening.shp')

highvis_xwalks2 <- highvis_xwalks %>% select() %>% mutate(Type = 'High-Visibility Crosswalk')
lpi2 <- lpi %>% select() %>% mutate(Type = 'Leading Pedestrian Interval')
paddle_signs2 <- paddle_signs %>% select() %>% mutate(Type = 'Paddle Sign')
pafb2 <- pafb %>% select() %>% mutate(Type = 'Pedestrian-Activated Flashing Beacon')
ped_islands2 <- ped_islands %>% select() %>% mutate(Type = 'Pedestrian Refuge Island')
scrambles2 <- scrambles %>% select() %>% mutate(Type = 'Scramble Crosswalk')
int_tight2 <- int_tight %>% select() %>% mutate(Type = 'Interim Intersection Tightening')

infrastructure <- rbind(highvis_xwalks2, lpi2, paddle_signs2, pafb2, ped_islands2, scrambles2, int_tight2)
infrastructure <- infrastructure %>% mutate(Type = as.factor(Type))
cd_boundaries$DISTRICT <- c('07','12','06','03','02','05','04','13','14','11','01','10','09','08','15')

### Other Data Preparation
lapd_fatal <- lapd_collisions %>% filter(severity == '1')
lapd_si <- lapd_collisions %>% filter(severity == '2')
# Prep for Initial Dashboard Calculations
lapd_collisions$date_occ <- as.Date(lapd_collisions$date_occ)
collisions_2017$date_occ <- as.Date(collisions_2017$date_occ)
current_date <- format(max(lapd_collisions$date_occ), format='%m-%d')
ytd_fatal_2018 <- lapd_collisions %>% filter(severity == 1) %>% st_set_geometry(NULL) %>% tally()
ytd_fatal_2017 <- collisions_2017 %>% mutate(date_occ = format(date_occ, format='%m-%d')) %>% filter(severity == 1, date_occ < current_date) %>% st_set_geometry(NULL) %>% tally()
ytd_ped_fatal_2018 <- lapd_collisions %>% filter(severity == 1, mode == 'Ped') %>% st_set_geometry(NULL) %>% tally()
ytd_ped_fatal_2017 <- collisions_2017 %>% mutate(date_occ = format(date_occ, format='%m-%d')) %>% filter(severity == 1, mode == 'Ped', date_occ < current_date) %>% st_set_geometry(NULL) %>% tally()
ytd_bike_fatal_2018 <- lapd_collisions %>% filter(severity == 1, mode == 'Bike') %>% st_set_geometry(NULL) %>% tally()
ytd_bike_fatal_2017 <- collisions_2017 %>% mutate(date_occ = format(date_occ, format='%m-%d')) %>% filter(severity == 1, mode == 'Bike', date_occ < current_date) %>% st_set_geometry(NULL) %>% tally()
ytd_veh_fatal_2018 <- lapd_collisions %>% filter(severity == 1, is.na(mode)) %>% st_set_geometry(NULL) %>% tally()
ytd_veh_fatal_2017 <- collisions_2017 %>% mutate(date_occ = format(date_occ, format='%m-%d')) %>% filter(severity == 1, is.na(mode), date_occ < current_date) %>% st_set_geometry(NULL) %>% tally()

# Fatals by Month
# MonthlyFatals <- lapd_collisions %>%
#   filter(severity == 1) %>%
#   mutate(month = format(date_occ, "%m")) %>%
#   group_by(month) %>%
#   st_set_geometry(NULL) %>%
#   tally()


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
      paste0(toString(ytd_fatal_2018),' Fatalities YTD (',toString(current_date),')'),
      paste0(toString(ytd_fatal_2017),
             ' YTD 2017 (',
             toString(round(((ytd_fatal_2018 - ytd_fatal_2017)/ytd_fatal_2017)*100),digits=2),
             '%)'),
      color = "black")  
  })
  output$PedDeaths <- renderValueBox({
    valueBox(
      paste0(toString(ytd_ped_fatal_2018),' Pedestrian'),
      paste0(toString(ytd_ped_fatal_2017),
             ' YTD 2017 (',
             toString(round(((ytd_ped_fatal_2018 - ytd_ped_fatal_2017)/ytd_ped_fatal_2017)*100),digits=2),
             '%)'),
      icon = icon("male",lib='font-awesome'),
      color = "red")  
  })
  output$BikeDeaths <- renderValueBox({ 
    valueBox(
      paste0(toString(ytd_bike_fatal_2018),' Bicyclist'),
      paste0(toString(ytd_bike_fatal_2017),
             ' YTD 2017 (',
             toString(round(((ytd_bike_fatal_2018 - ytd_bike_fatal_2017)/ytd_bike_fatal_2017)*100),digits=2),
             '%)'),
      icon = icon("bicycle",lib='font-awesome'),
      color = "yellow")  
  })
  output$VehDeaths <- renderValueBox({
    valueBox(
      paste0(toString(ytd_veh_fatal_2018),' Passenger'),
      paste0(toString(ytd_veh_fatal_2017),
             ' YTD 2017 (',
             toString(round(((ytd_veh_fatal_2018 - ytd_veh_fatal_2017)/ytd_veh_fatal_2017)*100),digits=2),
             '%)'),
      icon = icon("car",lib='font-awesome'),
      color = "blue")   
  })
  # Monthly Timeline Plot
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
  
  # Filter infrastructure, if needed
  infrastructure_r <- reactive({
    if((input$tabs == 'AreaFilter')&(!is.null(input$geography_name))){
      # Geography Filter
      infrastructure <- infrastructure[geography(),]
    } else {
      return(infrastructure)
    }
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
    lbls = c( 'Fatal Collision','High-Injury Network','High-Visibility Crosswalk','Interim Intersection Tightening','Leading Pedestrian Interval','Paddle Sign','Pedestrian-Activated Flashing Beacon','Pedestrian Refuge Island','Priority Corridor','Scramble Crosswalk')
    pal <- colorFactor(
      palette = c('#f44242','#f44242','#E11F8F','#482D8B','#79BC43','#F58220','#FFC828','#008576','#0E016F','#96C0E6'),
      domain = lbls
      )
    
    # Create map
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -118.329327,
              lat = 34.0546143,
              zoom = 12) %>%
      # Add City Boundary
      addPolylines(
        color = '#C0C0C0',
        weight = 2,
        opacity = 1,
        data = city_boundary) %>%
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
        color = ~pal('Priority Corridor'),
        weight = 2,
        opacity = 1,
        data = pc_r(),
        group = 'VZ Streets') %>%
      addCircleMarkers(
        radius = 0.2,
        fill = TRUE,
        color = ~pal('High-Visibility Crosswalk'),
        fillColor = ~pal('High-Visibility Crosswalk'),
        opacity = 1,
        data = highvis_xwalks,
        group = 'Projects - Complete'
      ) %>%
      addCircleMarkers(
        radius = 0.2,
        fill = TRUE,
        color = ~pal('Interim Intersection Tightening'),
        fillColor = ~pal('Interim Intersection Tightening'),
        opacity = 1,
        data = int_tight,
        group = 'Projects - Complete'
      ) %>%
      addCircleMarkers(
        radius = 0.2,
        fill = TRUE,
        color = ~pal('Leading Pedestrian Interval'),
        fillColor = ~pal('Leading Pedestrian Interval'),
        opacity = 1,
        data = lpi,
        group = 'Projects - Complete'
      ) %>%
      addCircleMarkers(
        radius = 0.2,
        fill = TRUE,
        color = ~pal('Paddle Sign'),
        fillColor = ~pal('Paddle Sign'),
        opacity = 1,
        data = paddle_signs,
        group = 'Projects - Complete'
      ) %>%
      addCircleMarkers(
        radius = 0.2,
        fill = TRUE,
        color = ~pal('Pedestrian-Activated Flashing Beacon'),
        fillColor = ~pal('Pedestrian-Activated Flashing Beacon'),
        opacity = 1,
        data = pafb,
        group = 'Projects - Complete'
      ) %>%
      addCircleMarkers(
        radius = 0.2,
        fill = TRUE,
        color = ~pal('Pedestrian Refuge Island'),
        fillColor = ~pal('Pedestrian Refuge Island'),
        opacity = 1,
        data = ped_islands,
        group = 'Projects - Complete'
      ) %>%
      addCircleMarkers(
        radius = 0.2,
        fill = TRUE,
        color = ~pal('Scramble Crosswalk'),
        fillColor = ~pal('Scramble Crosswalk'),
        opacity = 1,
        data = scrambles,
        group = 'Projects - Complete'
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = lbls
      ) %>%
      addLayersControl(
        overlayGroups = c('VZ Streets', 'Collisions YTD', 'Projects - Complete'),
        options = layersControlOptions(collapsed = TRUE)
      )
    
    # From LAPD Data, If there is at least 1 fatal, add to map
    if(nrow(lapd_fatal) > 0){
      map <- addCircleMarkers(
        map,
        radius = 1,
        fill = TRUE,
        color = ~pal('Fatal Collision'),
        fillColor = ~pal('Fatal Collision'),
        opacity = 1,
        data = lapd_fatal,
        group = 'Collisions YTD',
        popup = ~paste0('DR#: ',dr_no, '<br>',
                        'Date: ', date_occ, '<br>',
                        'Involved with: ', mode, '<br>')
      )
    }
  
    map
  })
  
  # Map Object for Area Filter
  output$vzmap <- renderLeaflet({
    
    lapd_fatal <- lapd_collisions_r() %>% filter(severity == '1')
    geography_r <- geography()
    infrastructure_r <- infrastructure_r()
    
    # Define color palette
    #lbls = c( 'High-Visibility Crosswalk','Interim Intersection Tightening','Leading Pedestrian Interval','Paddle Sign','Pedestrian-Activated Flashing Beacon','Pedestrian Refuge Island','Scramble Crosswalk')
    colors = c('#E11F8F','#482D8B','#79BC43','#F58220','#FFC828','#008576','#96C0E6')
    names(colors) = c( 'High-Visibility Crosswalk','Interim Intersection Tightening','Leading Pedestrian Interval','Paddle Sign','Pedestrian-Activated Flashing Beacon','Pedestrian Refuge Island','Scramble Crosswalk')
    pal <- colorFactor(
      #palette = c('#E11F8F','#482D8B','#79BC43','#F58220','#FFC828','#008576','#96C0E6'),
      domain = levels(factor(infrastructure_r$Type)),
      palette = colors[levels(factor(infrastructure_r$Type))]
    )
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    if (!is.null(input$geography_name)) {
      map <- map %>%
        addPolygons(
          data = geography_r,
          fill = FALSE) %>%
        # Add filtered HIN
        addPolylines(
          color = '#f44242',
          weight = 3,
          opacity = 1,
          data = hin_r(),
          label = ~paste0(STNAME, ": ", FROM_, " to ", TO_),
          group = 'VZ Streets') %>%
        # Add filtered PC
        addPolylines(
          color = '#0E016F',
          weight = 3,
          opacity = 1,
          data = pc_r(),
          group = 'VZ Streets')
    }
    
    # From LAPD Data, If there is at least 1 fatal, add to map
    if(nrow(lapd_fatal) > 0){
      map <- addCircleMarkers(
        map,
        radius = 1,
        fill = TRUE,
        color = '#f44242',
        fillColor = '#f44242',
        opacity = 1,
        data = lapd_fatal,
        group = 'Collisions YTD',
        popup = ~paste0('DR#: ',dr_no, '<br>',
                        'Date: ', date_occ, '<br>',
                        'Involved with: ', mode, '<br>')
      )
    }
    
    # If there is at least one piece of infrastructure, add to map
    if(nrow(infrastructure_r) > 0){
      map <- map %>%
        addCircleMarkers(
          radius = 1,
          fill = TRUE,
          data = infrastructure_r,
          color = ~pal(infrastructure_r$Type),
          fillColor = ~pal(infrastructure_r$Type),
          opacity = 1,
          group = 'Projects - Complete') %>%
        addLegend(
          position = "bottomleft",
          pal = pal,
          values = levels(factor(infrastructure_r$Type))) %>%
        addLayersControl(
          overlayGroups = c('VZ Streets', 'Collisions YTD', 'Projects - Complete'),
          options = layersControlOptions(collapsed = TRUE)
        )
    }
      
        
    map
  })
  
  # # Area Filter Map Observer #2
  # observe({
  #   
  #   geography_r <- geography()
  #   
  #   if (!is.null(input$geography_name)) {
  #     leafletProxy("vzmap") %>%
  #       clearShapes() %>%
  #       clearMarkers() %>%
  #       addPolygons(
  #         data = geography(),
  #         fill = FALSE
  #       ) %>%
  #       # Add filtered HIN
  #       addPolylines(
  #         color = '#f44242',
  #         weight = 3,
  #         opacity = 1,
  #         data = hin_r(),
  #         label = ~paste0(STNAME, ": ", FROM_, " to ", TO_)) %>%
  #       # Add filtered PC
  #       addPolylines(
  #         color = '#0E016F',
  #         weight = 3,
  #         opacity = 1,
  #         data = pc_r()
  #       ) %>%
  #       fitBounds(lng1 = as.double(st_bbox(geography_r)[1]),
  #                 lat1 = as.double(st_bbox(geography_r)[2]),
  #                 lng2 = as.double(st_bbox(geography_r)[3]),
  #                 lat2 = as.double(st_bbox(geography_r)[4])
  #       )
  #   }
  # })
  
  
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
  
  output$infrastructure_summary <- renderTable({
    
    infrastructure_r() %>%
      st_set_geometry(NULL) %>%
      group_by(Type) %>%
      summarise(Count = n()) 
  })
  
  
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

