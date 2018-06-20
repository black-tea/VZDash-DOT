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
library(slopegraph)
library(tibble)
##### Packages from IManager #####
library(RPostgreSQL)
library(shiny)
library(shinyjs)
library("googlesheets")
##### End Packages from IManager #####

##### Prep Code from IManager #####
# Mandatory fields that must be filled before a user can add data
fieldsMandatory <- c("treatment_type", "int")

# Treatment Objects
infrastructure_vars <- c(
  'Speed Feedback Sign' = 'sfs',
  'Pedestrian-Activated Flashing Beacon' = 'fb',
  'Pedestrian Refuge Island' = 'rfg'
)

sfs <- list(id = 'sfs',
            name_lbl = 'Speed Feedback Sign',
            data_flds = c('sfs_status','sfs_distance','sfs_streetside','sfs_facestraffic','sfs_serial','sfs_solar','sfs_activationdate','sfs_notes'),
            data_names = c('Status','Dist (ft.)','Street Side', 'Faces Traffic', 'Serial', 'Activation Date','Solar?', 'Notes'),
            tbl_flds = c('int_name','sfs_status','sfs_distance','sfs_streetside','sfs_facestraffic','sfs_serial','sfs_solar','sfs_activationdate','sfs_notes'),
            tbl_names = c('Intersection','Status','Dist (ft.)','Street Side', 'Faces Traffic', 'Serial', 'Activation Date', 'Solar?', 'Notes' )
            )
fb <- list(id = 'fb',
           name_lbl = 'Pedestrian-Activated Flashing Beacon',
           data_flds = c('fb_status','fb_roadside','fb_beaconstatus','fb_flashdur','fb_xwalk','fb_tcr','fb_curb','fb_notes'),
           data_names = c('Status','Roadside Only','Beacon Status','Flash Duration','X-Walk','TCR','Curb Ramps Both Approaches','Pole Notes'),
           tbl_flds = c('int_name','fb_status','fb_roadside','fb_beaconstatus','fb_flashdur','fb_xwalk','fb_tcr','fb_curb','fb_notes'),
           tbl_names = c('Intersection','Status','Roadside Only','Beacon Status','Flash Duration','X-Walk','TCR','Curb Ramps Both Approaches','Pole Notes')
           )
rfg <- list(id = 'rfg',
            name_lbl = 'Pedestrian Refuge Island',
            data_flds = c('rfg_status','rfg_hsip','rfg_designstart','rfg_designfinish','rfg_constructdate','rfg_url','rfg_notes'),
            data_names = c('Status','HSIP Survey','Design Start Date','Design Completion Date','Construction Completion Date','Design Plan URL','Notes'),
            tbl_flds = c('int_name','rfg_status','rfg_hsip','rfg_designstart','rfg_designfinish','rfg_constructdate','rfg_url','rfg_notes'),
            tbl_names = c('Intersection','Status','HSIP Survey','Design Start Date','Design Completion Date','Construction Completion Date','Design Plan URL','Notes')
            )

##### End Prep Code from IManager #####

# Dictionary of Column Names
cols <- c('DISTRICT','NAME_ALF','NAME')
names(cols) <- c('cd_boundaries','cpa_boundaries','nc_boundaries')

### Load Data
# VZ network files
hin <- read_sf('data/High_Injury_Network.geojson')
pc <- read_sf('data/prioritized_corridors/pc_05232017_wgs84_.shp')
# Boundary files
cd_boundaries <- read_sf('data/council_districts/CnclDist_July2012_wgs84.shp')
cpa_boundaries <- read_sf('data/community_planning_areas/CPA_wgs84.shp')
nc_boundaries <- read_sf('data/neighborhood_councils/LACITY_NEIGHBORHOOD_COUNCILS.shp')
city_boundary <- read_sf('data/city_boundary/city_boundary.shp')
# Infrastructure
highvis_xwalks <- read_sf('data/crosswalks/crosswalks.shp') %>% select() %>% mutate(Type = 'High-Visibility Crosswalk')
lpi <- read_sf('data/lpi/lpi.shp') %>% select() %>% mutate(Type = 'Leading Pedestrian Interval')
paddle_signs <- read_sf('data/paddle_signs/paddle_signs.shp') %>% select() %>% mutate(Type = 'Paddle Sign')
pafb <- read_sf('data/pafb/pafb.shp') %>% select() %>% mutate(Type = 'Pedestrian-Activated Flashing Beacon')
ped_islands <- read_sf('data/ped_islands/ped_islands.shp') %>% select() %>% mutate(Type = 'Pedestrian Refuge Island')
scrambles <- read_sf('data/scrambles/scrambles.shp') %>% select() %>% mutate(Type = 'Scramble Crosswalk')
int_tight <- read_sf('data/int_tightening/int_tightening.shp') %>% select() %>% mutate(Type = 'Interim Intersection Tightening')
speed_feedback_signs <- read_sf('data/speed_feedback_signs/speed_feedback_signs.shp') %>% select() %>% mutate(Type = 'Speed Feedback Signs')

infrastructure <- rbind(highvis_xwalks, lpi, paddle_signs, pafb, ped_islands, scrambles, int_tight, speed_feedback_signs)
infrastructure <- infrastructure %>% mutate(Type = as.factor(Type))
cd_boundaries$DISTRICT <- c('07','12','06','03','02','05','04','13','14','11','01','10','09','08','15')

function(input, output, session) {
  
  ##### Functions #####
  QuerySQL <- function (query, type) {
    # Query PostGIS database
    #
    # Args:
    #   query: String with db query
    #   type: Type of query. 'Spatial' returns query with geom object, table returns attribute info only
    #
    # Returns:
    #   A dataframe with or without a spatial object
    #
    # creating DB connection object 
    conn <- dbConnect(PostgreSQL(), host = "localhost", dbname = "dotdb", user="postgres", password="Feb241989", port = 5432)
    # close db connection after function call exits
    on.exit(dbDisconnect(conn))
    # Change query type depending on whether we want geometry or not
    if(type == 'table'){
      result <- dbGetQuery(conn, query)
    } else if(type == 'spatial'){
      result <- st_read(conn, query=query)
    }
    # return the dataframe
    return(result)
  }
  
  QueryTblSQL <- function (table, flds) {
    # Query a PostGIS table
    #
    # Args:
    #   table: Name of PostGIS table to query
    #   flds: Table fields to query
    #
    # Returns:
    #   Dataframe result of the db query
    #
    # Connect to the database 
    conn <- dbConnect(PostgreSQL(), host = "localhost", dbname = "dotdb", user="postgres", password="Feb241989", port = 5432)
    # close db connection after function call exits
    on.exit(dbDisconnect(conn))
    # Construct the query
    query <- sprintf(
      "SELECT %s FROM public.geom_%s",
      paste(flds, collapse = ", "),
      table
    )
    # Submit the query and disconnect
    result <- dbGetQuery(conn, query)
    return(result)
  }
  
  InsertSQL <- function(table, data) {
    # Insert a row into a PostGIS table
    #
    # Args:
    #   table: Name of PostGIS table to insert row into
    #   data: Data to insert into table
    #
    # Returns:
    #   Inserts a row into the PostGIS table
    # Connect to the database
    conn <- dbConnect(PostgreSQL(), host = "localhost", dbname = "dotdb", user="postgres", password="Feb241989", port = 5432)
    # close db connection after function call exits
    on.exit(dbDisconnect(conn))
    # Construct the update query by looping over the data fields
    query <- sprintf(
      "INSERT INTO public.geom_%s (%s) VALUES ('%s')",
      table, 
      paste(names(data), collapse = ", "),
      paste(data, collapse = "', '")
    )
    # Submit the insert query
    dbGetQuery(conn, query)
  }
  
  ChoosePt <- function(startPt, seg, segFraction) {
    # Ensure interpolation along line returns correct point
    # using PostGIS function ST_LineLocatePoint for verification
    #
    # Args:
    #   startPt: A point value at one end of the segment, used to establish origin for distance
    #   seg: A polyline segment to interpolate the distance along
    #   segFraction: Fractional value of the segment where the point is located
    #
    # Returns:
    #   The sf point that matches the distance specified in DistToPt
    #
    Pt1 <- st_line_sample(x = seg, n = 1, sample = segFraction) %>% st_cast("POINT")
    Pt2 <- st_line_sample(x = seg, n = 1, sample = (1 - segFraction)) %>% st_cast("POINT")
    
  }
  
  DistToPt <- function(startPt, seg, distFt) {
    # Generate a point along a line
    #
    # Args:
    #   startPt: A point value at one end of the segment, used to establish origin for distance
    #   seg: A polyline segment to interpolate the distance along
    #   distFt: Distance value for interpolation along the polyline segment
    #
    # Returns:
    #   A sf point at the precise distance specified along the line
    #
    # Need some error handling if distance > length of line
    seg <- seg %>%
      st_cast("LINESTRING") %>%
      st_set_crs(4326) %>%
      st_transform(2229)
    segDistFt <- st_length(seg)
    segFraction <- distFt/segDistFt
    infrastructurePt <- st_line_sample(x = seg, n = 1, sample = segFraction) %>%
      st_cast("POINT") %>%
      st_transform(4326)
    return(infrastructurePt)
    # Error handling
    #if distFt > segDist
  }
  
  UpdateGS <- function(table, flds) {
    # Update linked google sheet
    #
    # Args:
    #   table: Name of the table in PostgreSQL db to copy to excel sheet
    #   flds: List of fields to query in the table
    #
    # Returns:
    #   Copies the table data to a google sheet
    # Register test google sheet that already exists
    test_s <- gs_key('1bgkKWcvrMJXP3XOUAi_8-Z4K7k3msUYdS-yCZd_qP50')
    # Query the db for updated table information
    result <- QueryTblSQL(table, flds)
    # Update google sheet with result
    test_s <- test_s %>%
      gs_edit_cells(ws = table, input = result)
  }
  
  CreateIcon <- function(color) {
    # Create icon for mapping, using the awesomeIcons library
    #
    # Args:
    #   color: desired color for the map marker
    #
    # Returns:
    #   Map marker with the 'circle-o' icon in the desired color
    custom_icon <- awesomeIcons(
      icon = 'circle-o',
      iconColor = '#ffffff',
      library = 'fa',
      # The markercolor is from a fixed set of color choices
      markerColor = color
    )
    return(custom_icon)
  }
  
  # Formatting for Mandatory Code
  labelMandatory <- function(label) {
    tagList(
      label,
      span("*", class = "mandatory_star")
    )
  }
  
  BuffGeom <- function(boundary, ft) {
    # Buffer boundary by a distance in ft, return to wgs84
    #
    # Args:
    #   boundary: sf shape for the buffer
    #   ft: distance to buffer, in ft
    #
    # Returns:
    #   sf shape object, buffered, in wgs84
    geomNAD83 <- st_transform(boundary, 2229) # Convert to NAD83
    geomNAD83 <- st_buffer(geomNAD83, ft) # Buffer
    geomWGS84 <- st_transform(geomNAD83, 4326) # Convert back to wgs84
    return(geomWGS84)
  }
  
  SelectSegment <- function(segID) {
    # Select a polyline based on the row ID and draw on map
    #
    # Args:
    #   segID: Row ID of the line
    #
    # Returns:
    #   Adds Selected Line to the Map
    if(!is.null(xstreetR())){
      # Filter polylines based on the click
      polylineSelected <- xstreetR() %>%
        filter(rownames(.) == segID )
      # Add selected line information to RV
      locationRV$Segment <- polylineSelected
      # Add selected line on top as another color
      proxy <- leafletProxy("infrastructureManagerMap") %>%
        # Add selected line shape to map
        addPolylines(
          data = polylineSelected,
          layerId = "selected",
          color = "#0066a1",
          opacity = 1
        )
      # Once user has selected the street segment, becomes NULL
      msgRV$msg <- c(' ')
    } 
  }
  
  # Clip to selected boundary
  geomClip <- function(segment) {
    st_intersection(segment,geographyR())
  }
  
  ##### Reactive Objects #####
  # RV for intersection list
  intRV <- reactiveValues(intlist = QuerySQL("SELECT tooltip FROM intersections",type = 'table'))
  
  # RV for location objects
  locationRV <- reactiveValues(Intersection=list(), Segment=list())
  
  # RV storing UI message variable
  msgRV <- reactiveValues()
  
  # RV storing fatal collision data
  collisionsRV <- reactiveValues(fatal_5yr = QuerySQL("SELECT dr_no, date_occ, time_occ, severity, mode, wkb_geometry
                                                       FROM public.geom_lapd_collisions
                                                       WHERE
                                                       severity = '1' AND
                                                       date_occ >= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions) - interval '5' year",'spatial'),
                                  fatal_ytd = QuerySQL("SELECT dr_no, date_occ, time_occ, severity, mode, wkb_geometry
                                                       FROM public.geom_lapd_collisions
                                                       WHERE
                                                       severity = '1' AND
                                                       (date_occ >= to_char(date_trunc('year', now()),'YYYY-01-01')::DATE AND date_occ <= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions))",'spatial'),
                                  fatal_ytd_2yr = QuerySQL("SELECT dr_no, date_occ, time_occ, severity, mode, wkb_geometry
                                                           FROM public.geom_lapd_collisions
                                                           WHERE severity = '1' AND 
                                                           ((date_occ >= to_char(date_trunc('year', now() - interval '1 year'),'YYYY-01-01')::DATE AND date_occ <= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions) - interval '1 year') OR
                                                           (date_occ >= to_char(date_trunc('year', now()),'YYYY-01-01')::DATE AND date_occ <= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions)))",'spatial'),
                                  fatal_ytd_5yr = QuerySQL("SELECT dr_no, date_occ, time_occ, severity, mode, wkb_geometry FROM public.geom_lapd_collisions
                                                           WHERE severity = '1' AND 
                                                           ((date_occ >= to_char(date_trunc('year', now()),'YYYY-01-01')::DATE AND date_occ <= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions)) OR
                                                           (date_occ >= to_char(date_trunc('year', now() - interval '1 year'),'YYYY-01-01')::DATE AND date_occ <= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions) - interval '1 year') OR
                                                           (date_occ >= to_char(date_trunc('year', now() - interval '2 year'),'YYYY-01-01')::DATE AND date_occ <= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions) - interval '2 year') OR
                                                           (date_occ >= to_char(date_trunc('year', now() - interval '3 year'),'YYYY-01-01')::DATE AND date_occ <= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions) - interval '3 year') OR
                                                           (date_occ >= to_char(date_trunc('year', now() - interval '4 year'),'YYYY-01-01')::DATE AND date_occ <= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions) - interval '4 year'))",'spatial'),
                                  curr_date = QuerySQL("Select MAX(date_occ) from public.geom_lapd_collisions",'table')
                                  )
  
  # RV storing data for each table
  dbtblRV <- reactiveValues(sfs = QueryTblSQL('sfs', sfs$tbl_flds),
                             rfg = QueryTblSQL('rfg', rfg$tbl_flds),
                             fb = QueryTblSQL('fb', fb$tbl_fields)
  )
  
  # Filter geography, if needed
  geographyR <- reactive({
    # Begin Empty
    if (is.null(input$geographyName)){return()}
    # Grab the selected geography type and associated name column
    geographySelected <- get(input$geography_type)
    column = cols[[input$geography_type]]
    # Return the specific geographical boundaries 
    return(geographySelected[(geographySelected[[column]] == input$geographyName),])
  })
  
  # Capture form input values
  inputDataR <- reactive({
    fields <- get(input$treatment_type)[['data_flds']]
    formData <- sapply(fields, function(x) input[[x]])
  })
  
  # Reactive expression to grab intersection data based on user selection
  intersectionR <- reactive({
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      int_query <- paste0("SELECT * FROM intersections WHERE tooltip=","'",toString(input$int),"'")
      intersectionR <- QuerySQL(int_query, type = 'spatial')
      return(intersectionR)
    } else {return(NULL)}
  })
  
  # # Reactive expression capturing distance for SFS to nearest intersection
  # sfsDistR <- reactive({
  #   if(input$treatment_type == 'sfs') {
  #   # if(!any(sapply(list(locationRV$Intersection, locationRV$Segment, input$sfs_distance), is.null)) && input$treatment_type == 'sfs'){
  #   #   sfsPt <- DistToPt(locationRV$Intersection, locationRV$Segment, input$sfs_distance)
  #     return(input$sfs_distance)
  #   } else {
  #     return(NULL)
  #   }
  # })
  
  # # temporary observer to test sfsDistR
  # observeEvent(input$int, {
  #   print(sfsDistPtR())
  # })
  
  # Capture fields for display in tables
  tbl_fields <- reactive({
    fields <- get(input$treatment_type)[['tbl_flds']]
  })
  
  # Reactive expression to grab cross streets from selected intersection
  xstreetR <- reactive({
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      # Grab selected intersection information
      intersectionR <- intersectionR()
      # Query for streets related to the intersection
      xstreet_query <- paste0("SELECT *
                             FROM streets 
                             WHERE int_id_fro=", intersectionR$cl_node_id, " OR int_id_to=", intersectionR$cl_node_id)
      xstreet <- QuerySQL(xstreet_query, type = 'spatial')
    } else {return(NULL)}
  })
  
  # Filter infrastructure, if needed
  infrastructureR <- reactive({
    if((input$tabs == 'AreaFilter')&(!is.null(input$geographyName))){
      # Geography Filter
      infrastructure <- infrastructure[geographyR(),]
    } else {
      return(infrastructure)
    }
  })
  
  # YTD Fatals, for Citywide Map and KPI
  ytdFatalR <- reactive({
    # Filter if AreaFilter tab is activated
    if((input$tabs == 'AreaFilter')&(!is.null(input$geographyName))){
      # Geography Filter
      lapd_collisions <- collisionsRV$fatal_ytd[geographyR(),]
      # Date Range Filter (temporarily disabled)
      #lapd_collisions %>% filter(date_occ >= input$dateRange[1] & date_occ <= input$dateRange[2])
    } else {
      return(collisionsRV$fatal_ytd)
    }
  })
  
  # YTD Fatals Table, 2 year comparison 
  ytdFatal2yrTbl <- reactive({
    # Filter if AreaFilter tab is activated
    if((input$tabs == 'AreaFilter')&(!is.null(input$geographyName))){
      # Geography & Date Range Filters
      Fatals2yr <- collisionsRV$fatal_ytd_2yr[geographyR(),]
      # Date Range Filter (temporarily disabled)
      # Fatals2yr <- Fatals2yr %>% filter(date_occ >= input$dateRange[1] & date_occ <= input$dateRange[2])
    } else {
      Fatals2yr <- collisionsRV$fatal_ytd_2yr
    }
    Fatals2yr %>%
      mutate(year = lubridate::year(date_occ)) %>%
      rename(Mode = mode) %>%
      replace_na(list(Mode = 'MV Occupant')) %>%
      st_set_geometry(NULL) %>%
      group_by(Mode, year) %>%
      tally() %>%
      bind_rows(cbind(Mode = 'Total Fatalities', year = 2017, n = count(Fatals2yr %>% st_set_geometry(NULL) %>% filter(lubridate::year(date_occ) == '2017')))) %>%
      bind_rows(cbind(Mode = 'Total Fatalities', year = 2018, n = count(Fatals2yr %>% st_set_geometry(NULL) %>% filter(lubridate::year(date_occ) == '2018')))) %>%
      spread(year, n) 

  })
  
  # HIN Reactive 
  hinR <- reactive({
    # Clip for Area Filter
    if((input$tabs == 'AreaFilter')&(!is.null(geographyR()))){
      return(st_intersection(hin, BuffGeom(geographyR(),50)))
    } else {
      return(hin)
    }
  })
  
  # PC Reactive 
  pcR <- reactive({
    #Clip for Area Filter
    if((input$tabs == 'AreaFilter')&(!is.null(input$geographyName))){
      return(st_intersection(pc, BuffGeom(geographyR(), 50)))
    } else {
      return(pc)
    }   
  })
  
  # Map Reactive
  mapR <- reactive({
    
    # Grab Reactive objects
    ytdFatalR <- ytdFatalR()
    geographyR <- geographyR()
    infrastructureR <- infrastructureR()
    
    # Color palette for Infrastructure
    colors = c('#E11F8F', '#482D8B', '#79BC43', '#F58220', '#FFC828', '#008576', '#96C0E6')
    names(colors) = c('High-Visibility Crosswalk', 'Interim Intersection Tightening', 'Leading Pedestrian Interval', 'Paddle Sign', 'Pedestrian-Activated Flashing Beacon', 'Pedestrian Refuge Island', 'Scramble Crosswalk')
    pal <- colorFactor(
      domain = levels(factor(infrastructureR$Type)),
      palette = colors[levels(factor(infrastructureR$Type))]
    )
    
    # Map Object
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) 
    
    # Add City Boundary
    if (input$tabs == "ProjectDelivery") {
      map <- map %>%
        # Add city boundary
        addPolylines(
          color = '#C0C0C0',
          weight = 2,
          opacity = 1,
          data = city_boundary)
    }
    
    # Add Geography Filter
    if (input$tabs == "AreaFilter" && !is.null(input$geographyName)) {
      map <- map %>%
        addPolygons(
          data = geographyR,
          weight = 5,
          opacity = 0.3,
          color = 'grey',
          fill = FALSE)

    }
    
    # Add HIN
    map <- map %>%
      addPolylines(
        color = '#f44242',
        weight = 2.5,
        opacity = 1,
        data = hinR(),
        label = ~paste0(STNAME, ": ", FROM_, " to ", TO_),
        group = 'VZ Streets') %>%
      # Add filtered PC
      addPolylines(
        color = '#0E016F',
        weight = 2.5,
        opacity = 1,
        data = pcR(),
        group = 'VZ Streets')
    
    # From LAPD Data, If there is at least 1 fatal, add to map
    if(nrow(ytdFatalR) > 0){
      map <- addCircleMarkers(
        map,
        data = ytdFatalR,
        radius = 3,
        color = '#F44242',
        stroke = FALSE,
        fillOpacity = 1,
        group = 'Fatalities YTD',
        popup = ~paste0('DR#: ', dr_no, '<br>',
                        'Date: ', date_occ, '<br>',
                        'Time: ', time_occ, '<br>',
                        'Involved with: ', mode, '<br>')
      )
    }
    
    # If there is at least one piece of infrastructure, add to map
    if(nrow(infrastructureR) > 0){
      map <- map %>%
        addCircleMarkers(
          data = infrastructureR,
          radius = 3,
          color = ~pal(Type),
          stroke = FALSE,
          fillOpacity = 1,
          group = 'Projects - Complete',
          popup = ~paste0(Type))%>%
        addLegend(
          position = "bottomleft",
          pal = pal,
          values = levels(factor(infrastructureR$Type))) %>%
        addLayersControl(
          overlayGroups = c('VZ Streets', 'Fatalities YTD', 'Projects - Complete'),
          options = layersControlOptions(collapsed = TRUE)
        )
    }
    
    map
  })
  
  ##### Server Code from IManager #####
  ### UI Elements
  # Treatment Type Selection
  # output$treatment_type <- renderUI({
  #   
  #   # Selection Input
  #   selectInput("treatment_type",
  #               labelMandatory("Treatment"),
  #               c("",
  #                 "Leading Pedestrian Interval",
  #                 "Speed Feedback Sign", "Paddle Sign",
  #                 "Pedestrian-Activated Flashing Beacon",
  #                 "Pedestrian Refuge Island",
  #                 "High-Visibility Crosswalk",
  #                 "Scramble Crosswalk"))
  # })
  

  
  ##### Infrastructure Manger Map & Observers #####
  output$infrastructureManagerMap <- renderLeaflet({
    # Map object
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 10, maxZoom = 18)) %>%
      setView(lng = -118.329327,
              lat = 34.0546143,
              zoom = 11)
  })
  
  # Infrastructure Manager Map observer that updates based on the intersection
  observeEvent(input$int, {
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      
      # Get intersection reactive var, clear markers, clear RV
      intersectionR <- intersectionR()
      locationRV$Segment <- NULL
      proxy <- leafletProxy("infrastructureManagerMap") %>%
        clearMarkers() %>%
        clearShapes()
      
      # If there is one marker in the query, it is blue
      if(nrow(intersectionR) == 1 && length(intersectionR) > 0) {
        # Add intersection to RV object
        locationRV$Intersection <- intersectionR
        # Get cross streets
        xstreetR <- xstreetR()
        # Update message to choose a street
        msgRV$msg <- c('Select a Cross Street')
        # Add intersection marker to map
        proxy %>% addAwesomeMarkers(
          data = intersectionR,
          icon = CreateIcon('darkblue')
        )
        
        # If there is at least one related segment, add it
        if(length(xstreetR) > 0) {
          proxy %>% addPolylines(
            data = xstreetR,
            layerId = as.numeric(rownames(xstreetR)),
            color = "gray"
          )
        }
        # If there is >1 marker, gray initially
      } else if(nrow(intersectionR) > 1) {
        proxy %>% addAwesomeMarkers(
          data = intersectionR,
          icon = CreateIcon("gray")
        )
        msgRV$msg <- c('Select One Intersection Node')
        
      }
      # Update the map zoom bounds
      proxy %>% fitBounds(lng1 = as.double(st_bbox(intersectionR)[1]),
                          lat1 = as.double(st_bbox(intersectionR)[2]),
                          lng2 = as.double(st_bbox(intersectionR)[3]),
                          lat2 = as.double(st_bbox(intersectionR)[4]))
    }
  })
  
  # Infrastructure Manager Map Observer based on the polyline selection
  observeEvent(input$infrastructureManagerMap_shape_click, {
    # Add Selection to Map
    SelectSegment(input$infrastructureManagerMap_shape_click$id)
    
  })
  
  # Observe
  observe({
    # For SFS, add point
    if(!any(sapply(list(locationRV$Intersection, locationRV$Segment, input$sfs_distance), is.null)) && input$treatment_type == 'sfs'){
      infrastructurePt <- DistToPt(locationRV$Intersection, locationRV$Segment, input$sfs_distance) 
      # Update Infrastructure Manager Map
      leafletProxy("infrastructureManagerMap") %>%
        removeMarker(layerId = "infrastructurePt") %>%
        addMarkers(data = infrastructurePt, layerId = "infrastructurePt")
    }
  })
  
  # Observer controlling submission of information to database
  observeEvent(input$submit, {
    
    # Create a progress notification
    progress <- shiny::Progress$new(style = 'notification')
    progress$set(message = "Submitting...", value = NULL)
    on.exit(progress$close())
    
    # Temporarily disable submit button
    shinyjs::disable('submit')
    
    # maybe add switch variable here, (for example, whether it has a segment or not)
    
    # Int AssetID, Int Cl_Node_ID, Int Name, Seg AssetID, Form Data, Int Geom 
    data <- c(int_assetid = locationRV$Intersection$assetid,
              int_clnodeid = locationRV$Intersection$cl_node_id,
              int_name = locationRV$Intersection$tooltip,
              seg_assetid = locationRV$Segment$assetid,
              inputDataR(),
              geom_4326 = st_as_text(locationRV$Intersection$geom))
    
    # Add to DB, update progress bar
    InsertSQL(input$treatment_type, data)
    
    # Update linked spreadsheet, update progress bar
    progress$set(detail = "Updating linked Google Sheets.")
    UpdateGS(input$treatment_type, tbl_fields())
    
    # Reset form & map objects, map view back to LA
    shinyjs::reset("form")
    locationRV$Segment <- NULL
    proxy <- leafletProxy("infrastructureManagerMap") %>%
      clearMarkers() %>%
      clearShapes() %>%
      setView(lng = -118.329327,
              lat = 34.0546143,
              zoom = 11)
    
    # Update DT
    dbtblRV[[input$treatment_type]] <- QueryTblSQL(input$treatment_type, tbl_fields())
    
  })
  
  ### Maps
  # Map Object for Project Delivery Tab
  output$projectmap <- renderLeaflet({
    
    mapR() %>%
      setView(lng = -118.329327,
              lat = 34.0546143,
              zoom = 12) 
    
  })
  
  # Map Object for Area Filter
  output$filterMap <- renderLeaflet({

    mapR()

  })
    
  ##### Non-Map Output #####
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(dbtblRV$sfs, file)
    }
  )

  output$treatment_type <- renderUI({
    
    # Selection Input
    selectInput("treatment_type",
                labelMandatory("Treatment"),
                c(infrastructure_vars))
  })
  
  # Intersection Selection
  output$intSelect <- renderUI({
    
    # Selection Input
    selectizeInput(inputId = "int",
                   labelMandatory('Intersection'),
                   choices = intRV$intlist$tooltip,
                   selected = NULL,
                   multiple = FALSE)
  })
  
  # Planned Status Selection
  output$treatment_status <- renderUI({
    if(!is.null(input$treatment_type)){
      if(input$treatment_type == 'rfg'){
        selectInput(inputId = "rfg_status", label = "Status", choices = list('Planned', 'Completed'))
      } else if(input$treatment_type == 'sfs'){
        selectInput(inputId = "sfs_status", label = "Status", choices = list('Planned', 'Completed'))
      } else if(input$treatment_type == 'fb'){
        selectInput(inputId = "fb_status", label = "Status", choices = list('Planned', 'Completed'))
      }
    }
  })
  
  # Second UI Bin
  output$treatment_info1 <- renderUI({
    if(!is.null(input$treatment_type)){
      if(input$treatment_type == 'Leading Pedestrian Interval'){
        sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE)
      } else if(input$treatment_type == 'rfg'){
        tagList(
          selectInput("rfg_hsip", label = "HSIP Survey", choices = list("","Yes", "No")),
          dateInput("rfg_designstart", label = "Design Start Date", value = ""),
          dateInput("rfg_designfinish", label = "Design Completion Date (Plan Sent to BSS)", value = ""),
          dateInput("rfg_constructdate", label = "Construction Completion Date", value = "")
        )
      } else if (input$treatment_type == 'fb'){
        tagList(
          selectInput("fb_roadside", label = "Roadside-Only RRFB Candidate", choices = c('','No','Yes','Yes + Mast Arm','Yes + Mast Arm or Median')),
          selectInput("fb_beaconstatus", label = "Beacon Status", choices = c('','Field Assessment Completed', 'RRFB Activated','RRFB Activated - Mast Arm still needed')),
          textInput("fb_flashdur", label = "Flash Duration"),
          selectInput("fb_xwalk", label = "Existing or Proposed Crosswalk", choices = c('','Existing','Proposed'))
        )
      } else if (input$treatment_type == 'sfs'){
        tagList(
          numericInput("sfs_distance", label = "Distance from Intersection (ft)", value = 0),
          selectInput("sfs_streetside", label = "Side of Street", choices = c('','N','S','E','W')),
          selectInput("sfs_facestraffic", label = "Faces Traffic", choices = c('','N','S','E','W'))
        )
      }
    }
  })
  
  # Third UI Bin
  output$treatment_info2 <- renderUI({
    if(!is.null(input$treatment_type)){
      if(input$treatment_type == 'Leading Pedestrian Interval'){
        sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE)
      } else if(input$treatment_type == 'rfg'){
        tagList(
          dateInput("rfg_url", label = "URL to Design Plan", value = ""),
          textAreaInput("rfg_notes", "Notes")
        ) 
      } else if(input$treatment_type == 'fb'){
        tagList(
          selectInput("fb_tcr", label = "TCR", choices = c('','No',"Yes")),
          selectInput("fb_curb", label = "Curb Ramps Both Approaches", choices = c('','TBD','Yes')),
          textAreaInput("fb_notes", label = "Poles")
        )
      } else if(input$treatment_type == 'sfs'){
        tagList(
          textInput("sfs_serial","Serial No."),
          selectInput('sfs_solar', 'Solar?', choices = c('','Yes','No')),
          dateInput('sfs_activationdate', 'Activation Date', value = ''),
          textAreaInput('sfs_notes', label = 'Comments')
        )
      }
    }
  })
  
  # Message Object
  output$message <- renderText({msgRV$msg})
  
  # DT
  output$sfs <- renderDT(dbtblRV$sfs, colnames = sfs$tbl_names)
  output$rfg <- renderDT(dbtblRV$rfg, colnames = rfg$tbl_names)
  output$fb <- renderDT(dbtblRV$fb, colnames = fb$tbl_names)
  
  # Observer focused on the input form
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled, length(locationRV$Segment) > 0)

    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  # Citywide infrastructure output table
  output$citywideInfrastructureSummary <- renderTable({
    
    infrastructure %>%
      st_set_geometry(NULL) %>%
      count(Type) %>%
      bind_rows(cbind(Type = 'Total Improvements Installed', n = count(infrastructure %>% st_set_geometry(NULL)))) %>%
      mutate(Count = n,
             n = NULL)
  })
  
  # Geography Type select input box
  output$geographyTypeSelect <- renderUI({
    
    selectInput("geography_type", "Geography Type",
                c("Council District" = "cd_boundaries",
                  "Neighborhood Council" = "nc_boundaries",
                  "Community Plan Area" = "cpa_boundaries"
                ),
                selected = "cd_boundaries")
  })
  
  
  # Geography Name select input box
  output$geographyNameSelect <- renderUI({
    
    # Begin Empty
    if (is.null(input$geography_type))
      return()
    
    # Grab the selected geography type and associated name column
    geographySelected <- get(input$geography_type)
    
    # Grab the text version (not the object) of the selected geography
    # to look up the appropriate column value
    column = cols[[input$geography_type]]
    
    # sf (not sp) package version of the same command
    geographyNames <- sort(unique(geographySelected[[column]]))
    
    # Generate Geography Name Input Box
    selectInput("geographyName", "Geography Name", choices = geographyNames)
  })
  
  # Fatal 2Yr Summary Table for 'Key Indicators' Tab
  output$fatalSummaryKPI <- renderTable({
    ytdFatal2yrTbl()
  })
  
  # Fatal 2Yr Summary Table for 'Area Filter' Tab
  output$fatalSummaryFilter <- renderTable({
    ytdFatal2yrTbl()
  })
  
  output$collision_title <- renderText({
    paste0('YTD Fatalities, as of ',format(collisionsRV$curr_date, format='%m-%d'))
  })
  
  output$infrastructureSummary <- renderTable({
    
    infrastructureR() %>%
      st_set_geometry(NULL) %>%
      count(Type) %>%
      bind_rows(cbind(Type = 'Total Improvements Installed', n = count(infrastructureR() %>% st_set_geometry(NULL)))) %>%
      mutate(Count = n,
             n = NULL)
  })
  
  # Slopegraph with YTD Numbers
  output$slopegraph <- renderPlot({
    
    slopetbl <- collisionsRV$fatal_ytd_5yr %>%
      st_set_geometry(NULL) %>%
      replace_na(list(mode = 'MV Occupant')) %>%
      mutate(year = lubridate::year(date_occ)) %>%
      group_by(mode, year) %>%
      tally() %>%
      spread(year, n) %>%
      ungroup() %>%
      as.data.frame() %>%
      remove_rownames %>%
      column_to_rownames(var = 'mode')

    slopegraph(slopetbl, col.lines = 'gray', col.lab = "black", 
               xlim = c(-1,7), 
               cex.lab = 0.8, cex.num = 0.9,
               family = 'Helvitca',
               xlabels = c('2014','2015','2016','2017','2018')
               )
  })
  
  ##### Reports #####
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
                     geographyName = input$geographyName,
                     map = geographyR(),
                     hin = geomClip(hin),
                     pc = geomClip(pc)
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

