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

##### Packages from IManager #####
library(RPostgreSQL)
library(shiny)
library(shinyjs)
library(sf) # Don't use 'sf' column for queries that don't have 'geom' column
library("googlesheets")
##### End Packages from IManager #####

##### Functions from IManager #####
# PostGIS // NEED TO ADD FILE WITH CREDENTIALS
# Function to run specific query on db
sqlQuery <- function (query, type) {
  
  # creating DB connection object 
  conn <- dbConnect(PostgreSQL(), host = "localhost", dbname = "dotdb", user="postgres", password="Feb241989", port = 5432)
  
  # close db connection after function call exits
  on.exit(dbDisconnect(conn))
  
  # Change query type depending on whether we want geometry or not
  if(type == 'table'){
    result <- dbGetQuery(conn, query)
  } else if(type == 'spatial'){
    result <- st_read_db(conn, query=query)
  }
  
  # return the dataframe
  return(result)
}

sqlQueryTblData <- function (table, flds) {
  
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

# Insert row into PostGIS table
sqlInsert <- function(table, data) {
  
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

# Function to update google sheets after making any changes to the DB
gsUpdate <- function(table, flds) {
  
  # Register test google sheet that already exists
  test_s <- gs_key('1bgkKWcvrMJXP3XOUAi_8-Z4K7k3msUYdS-yCZd_qP50')
  
  # Query the db for updated table information
  result <- sqlQueryTblData(table, flds)
  
  # Update google sheet with result
  test_s <- test_s %>%
    gs_edit_cells(ws = table, input = result)
}

# Function to create Icons for map
createIcon <- function(color) {
  
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
##### End Functions from IManager #####

##### Prep Code from IManager #####
# Load intersections
intersections <- sqlQuery("SELECT assetid, cl_node_id, tooltip FROM intersections",type = 'table')
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

# Set WD
#work_dir <- "C:/Users/dotcid034/Documents/GitHub/vzcd-shiny"
#setwd(work_dir)

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
#lapd_fatal <- lapd_collisions %>% filter(severity == '1')
#lapd_si <- lapd_collisions %>% filter(severity == '2')
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

# infrastructure_r() %>%
#   st_set_geometry(NULL) %>%
#   group_by(Type) %>%
#   summarise(Count = n()) 
# collisions_tbl <- lapd_collisions %>%
#   bind_rows(collisions_2017) %>%
#   st_set_geometry(NULL) %>%
#   filter(severity == '1') %>%
#   group_by(mode) %>%
#   summarise(Count = n())

# Collisions
lapd_collisions_2 <- lapd_collisions %>% mutate(year = 2018) %>% st_set_geometry(NULL)
collisions_2017_2 <- collisions_2017 %>% mutate(year = 2017) %>% st_set_geometry(NULL)

# Collisions
collisions_tbl <- lapd_collisions_2 %>%
  bind_rows(collisions_2017_2) %>%
  replace_na(list(mode = 'Veh')) %>%
  filter(severity == '1') %>%
  group_by(mode, year) %>%
  tally() %>%
  spread(year, n) %>%
  ungroup %>%
  as.data.frame() %>%
  remove_rownames %>%
  column_to_rownames(var = 'mode')

# Fatals by Month
# MonthlyFatals <- lapd_collisions %>%
#   filter(severity == 1) %>%
#   mutate(month = format(date_occ, "%m")) %>%
#   group_by(month) %>%
#   st_set_geometry(NULL) %>%
#   tally()


function(input, output, session) {
  
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
  
  output$treatment_type <- renderUI({
    
    # Selection Input
    selectInput("treatment_type",
                labelMandatory("Treatment"),
                c(infrastructure_vars))
  })
  
  # Intersection Selection
  output$int_select <- renderUI({
    
    # Selection Input
    selectizeInput(inputId = "int",
                   labelMandatory('Intersection'),
                   choices = intersections$tooltip,
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
          numericInput("sfs_distance", label = "Distance from Intersection (ft)", value=0),
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
  output$message <- renderText({rv_msg$msg})
  
  # DT
  output$sfs <- renderDT(rv_dbtbl$sfs, colnames = sfs$tbl_names)
  output$rfg <- renderDT(rv_dbtbl$rfg, colnames = rfg$tbl_names)
  output$fb <- renderDT(rv_dbtbl$fb, colnames = fb$tbl_names)
  
  ### Observer focused on the input form
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled, length(rv_location$Segment) > 0)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  ### Reactive Objects
  # RV for location objects
  rv_location <- reactiveValues(Intersection=list(), Segment=list())
  
  # RV storing UI message variable
  rv_msg <- reactiveValues()
  
  # RV storing fatal collision data
  rv_collisions <- reactiveValues(fatal_5yr = sqlQuery("SELECT dr_no, date_occ, time_occ, severity, mode, wkb_geometry
                                                        FROM public.geom_lapd_collisions
                                                        WHERE
                                                          severity = '1' AND
                                                          date_occ >= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions) - interval '5' year",'spatial'),
                                  fatal_ytd = sqlQuery("SELECT dr_no, date_occ, time_occ, severity, mode, wkb_geometry
                                                        FROM public.geom_lapd_collisions
                                                        WHERE
                                                          severity = '1' AND
                                                          (date_occ >= to_char(date_trunc('year', now()),'YYYY-01-01')::DATE AND date_occ <= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions))",'spatial'),
                                  fatal_ytd_2yr = sqlQuery("SELECT dr_no, date_occ, time_occ, severity, mode, wkb_geometry
                                                            FROM public.geom_lapd_collisions
                                                            WHERE severity = '1' AND 
                                                              ((date_occ >= to_char(date_trunc('year', now() - interval '1 year'),'YYYY-01-01')::DATE AND date_occ <= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions) - interval '1 year') OR
                                                              (date_occ >= to_char(date_trunc('year', now()),'YYYY-01-01')::DATE AND date_occ <= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions)))",'spatial'),
                                  fatal_ytd_5yr = sqlQuery("SELECT dr_no, date_occ, time_occ, severity, mode, wkb_geometry FROM public.geom_lapd_collisions
                                                            WHERE severity = '1' AND 
                                                              ((date_occ >= to_char(date_trunc('year', now()),'YYYY-01-01')::DATE AND date_occ <= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions)) OR
                                                              (date_occ >= to_char(date_trunc('year', now() - interval '1 year'),'YYYY-01-01')::DATE AND date_occ <= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions) - interval '1 year') OR
                                                              (date_occ >= to_char(date_trunc('year', now() - interval '2 year'),'YYYY-01-01')::DATE AND date_occ <= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions) - interval '2 year') OR
                                                              (date_occ >= to_char(date_trunc('year', now() - interval '3 year'),'YYYY-01-01')::DATE AND date_occ <= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions) - interval '3 year') OR
                                                              (date_occ >= to_char(date_trunc('year', now() - interval '4 year'),'YYYY-01-01')::DATE AND date_occ <= (SELECT MAX(date_occ) FROM public.geom_lapd_collisions) - interval '4 year'))",'spatial'),
                                  curr_date = sqlQuery("Select MAX(date_occ) from public.geom_lapd_collisions",'table')
                                  )
  
  # RV storing data for each table
  rv_dbtbl <- reactiveValues(sfs = sqlQueryTblData('sfs', sfs$tbl_flds),
                             rfg = sqlQueryTblData('rfg', rfg$tbl_flds),
                             fb = sqlQueryTblData('fb', fb$tbl_fields)
                             )
  
  # Capture form input values
  input_data <- reactive({
    fields <- get(input$treatment_type)[['data_flds']]
    formData <- sapply(fields, function(x) input[[x]])
  })
  
  # Capture fields for display in tables
  tbl_fields <- reactive({
    fields <- get(input$treatment_type)[['tbl_flds']]
  })
  
  # Reactive expression to grab intersection data based on user selection
  intersection_r <- reactive({
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      int_query <- paste0("SELECT * FROM intersections WHERE tooltip=","'",toString(input$int),"'")
      intersection_r <- sqlQuery(int_query, type = 'spatial')
    } else {return(NULL)}
  })
  
  # Reactive expression to grab cross streets from selected intersection
  xstreet_r <- reactive({
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      # Grab selected intersection information
      intersection_r <- intersection_r()
      # Query for streets related to the intersection
      xstreet_query = paste0("SELECT *
                             FROM streets 
                             WHERE int_id_fro=",intersection_r$cl_node_id," OR int_id_to=",intersection_r$cl_node_id)
      xstreet <- sqlQuery(xstreet_query, type='spatial')
    } else {return(NULL)}
  })
  
  ### Map
  output$map <- renderLeaflet({
    # Map object
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 10, maxZoom = 18)) %>%
      setView(lng = -118.329327,
              lat = 34.0546143,
              zoom = 11) 
  })
  
  # Map observer that updates based on the intersection
  observeEvent(input$int, {
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      
      # Get intersection reactive var, clear markers, clear RV
      intersection_r <- intersection_r()
      rv_location$Segment <- NULL
      proxy <- leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes()
      
      # If there is one marker in the query, it is blue
      if(nrow(intersection_r) == 1 && length(intersection_r) > 0) {
        # Add intersection to RV object
        rv_location$Intersection <- intersection_r
        # Get cross streets
        xstreet_r <- xstreet_r()
        # Update message to choose a street
        rv_msg$msg <- c('Select a Cross Street')
        # Add intersection marker to map
        proxy %>% addAwesomeMarkers(
          data = intersection_r,
          icon = createIcon('darkblue')
        )
        
        # If there is at least one related segment, add it
        if(length(xstreet_r) > 0) {
          proxy %>% addPolylines(
            data = xstreet_r,
            layerId = as.numeric(rownames(xstreet_r)),
            color = "gray"
          )
        }
        # If there is >1 marker, gray initially
      } else if(nrow(intersection_r) > 1) {
        proxy %>% addAwesomeMarkers(
          data = intersection_r,
          icon = createIcon("gray")
        )
        rv_msg$msg <- c('Select One Intersection Node')
        
      }
      # Update the map zoom bounds
      proxy %>% fitBounds(lng1 = as.double(st_bbox(intersection_r)[1]),
                          lat1 = as.double(st_bbox(intersection_r)[2]),
                          lng2 = as.double(st_bbox(intersection_r)[3]),
                          lat2 = as.double(st_bbox(intersection_r)[4]))
    }
  })
  
  # Map Observer based on the polyline selection
  observeEvent(input$map_shape_click, {
    if(!is.null(xstreet_r())){
      # Grab ID of the shape that was clicked
      click_id <- input$map_shape_click$id
      # Filter polylines based on the click
      polyline_s <- xstreet_r() %>%
        filter(rownames(.) == click_id )
      rv_location$Segment <- polyline_s
      # Add selected line on top as another color
      proxy <- leafletProxy("map") %>%
        # Add selected line shape to map
        addPolylines(
          data = polyline_s,
          layerId = "selected",
          color = "#0066a1",
          opacity = 1
        )
      # Once user has selected the street segment, becomes NULL
      rv_msg$msg <- c(' ')
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
    data <- c(int_assetid = rv_location$Intersection$assetid,
              int_clnodeid = rv_location$Intersection$cl_node_id,
              int_name = rv_location$Intersection$tooltip,
              seg_assetid = rv_location$Segment$assetid,
              input_data(),
              geom_4326 = st_as_text(rv_location$Intersection$geom))
    
    # Add to DB, update progress bar
    sqlInsert(input$treatment_type, data)
    
    # Update linked spreadsheet, update progress bar
    progress$set(detail = "Updating linked Google Sheets.")
    gsUpdate(input$treatment_type, tbl_fields())
    
    # Reset form & map objects, map view back to LA
    shinyjs::reset("form")
    rv_location$Segment <- NULL
    proxy <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      setView(lng = -118.329327,
              lat = 34.0546143,
              zoom = 11)
    
    # Update DT
    rv_dbtbl[[input$treatment_type]] <- sqlQueryTblData(input$treatment_type, tbl_fields())
    
  })
  
  #### Download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(rv_dbtbl$sfs, file)
    }
  )
  ##### End Server Code from IManager #####
  
  
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
  
  # Citywide infrastructure output table
  output$citywide_infrastructure_summary <- renderTable({
    
    infrastructure %>%
      st_set_geometry(NULL) %>%
      count(Type) %>%
      bind_rows(cbind(Type = 'Total Improvements Installed', n = count(infrastructure %>% st_set_geometry(NULL)))) %>%
      mutate(Count = n,
             n = NULL)
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
  
  # # Filter lapd collisions, if needed
  # ytd_fatal_collisions_r <- reactive({
  #   # Filter if AreaFilter tab is activated
  #   if((input$tabs == 'AreaFilter')&(!is.null(input$geography_name))){
  #     # Geography Filter
  #     lapd_collisions <- lapd_collisions[geography(),]
  #     # Date Range Filter
  #     lapd_collisions %>% filter(date_occ >= input$dateRange[1] & date_occ <= input$dateRange[2])
  #   } else {
  #     return(lapd_collisions)
  #   }
  # })
  
  # Filter lapd collisions, if needed
  ytd_fatal_collisions_r <- reactive({
    # Filter if AreaFilter tab is activated
    if((input$tabs == 'AreaFilter')&(!is.null(input$geography_name))){
      # Geography Filter
      lapd_collisions <- rv_collisions$fatal_ytd[geography(),]
      # Date Range Filter
      lapd_collisions %>% filter(date_occ >= input$dateRange[1] & date_occ <= input$dateRange[2])
    } else {
      return(rv_collisions$fatal_ytd)
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
    
    lapd_fatal <- ytd_fatal_collisions_r() %>% filter(severity == '1')
    
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
    
    lapd_fatal <- ytd_fatal_collisions_r() %>% filter(severity == '1')
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
    # lapd_fatal <- ytd_fatal_collisions_r() %>% filter(severity == '1')
    # lapd_si <- ytd_fatal_collisions_r() %>% filter(severity == '2')
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
    
  
  output$lapd_summary_current <- renderTable({

    ytd_fatal_collisions_r() %>%
      group_by(mode, severity) %>%
      tally() %>%
      st_set_geometry(NULL) %>%
      spread(severity, n)
  })
  
  output$lapd_summary_2yr <- renderTable({
    
    rv_collisions$fatal_ytd_2yr %>%
      mutate(year = lubridate::year(date_occ)) %>%
      group_by(mode, year) %>%
      tally() %>%
      st_set_geometry(NULL) %>%
      spread(year, n)
  })
  
  output$collision_title <- renderText({
    paste0('YTD Fatals, ',format(rv_collisions$curr_date, format='%m-%d'))
  })
  
  output$infrastructure_summary <- renderTable({
    
    infrastructure_r() %>%
      st_set_geometry(NULL) %>%
      count(Type) %>%
      bind_rows(cbind(Type = 'Total Improvements Installed', n = count(infrastructure_r() %>% st_set_geometry(NULL)))) %>%
      mutate(Count = n,
             n = NULL)
  })
  
  # Slopegraph with YTD Numbers
  output$slopegraph <- renderPlot({
    
    slopetbl <- rv_collisions$fatal_ytd_5yr %>%
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
               #xlim = c(-.5,5.5), 
               #cex.lab = 0.5, cex.num = 0.5, 
               family = 'Helvitca',
               xlabels = c('2014','2015','2016','2017','2018'))
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

