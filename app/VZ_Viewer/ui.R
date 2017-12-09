# VZ Viewer UI

library(shinydashboard)
library(leaflet)

## Header
header <- dashboardHeader(
  title = "VZ View"
)

## Sidebar Content
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Citywide Collisions", tabName="citywide", icon = icon("bar-chart")),
    menuItem("Area / Corridor Collisions", tabName="AreaFilter", icon = icon("map")),
    menuItem("Speed Survey Status", tabName="Surveys", icon = icon("pencil-square-o")),
    menuItem("BSS PCI Status", tabName="BSS", icon=icon("truck"))
    
  )
)

## Body Layout
body <- dashboardBody(
  fluidRow(
    
    # Left-hand column
    column(width = 9,
           
           # Map Viewer
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("vzmap", height = 500)
               ),
           
           # Output crash table
           box(width = NULL,
               title = "Collision Summary",
               tableOutput('lapd_summary'))
           #     ),
           # 
           # # Basic Output Statistics
           # box(width = NULL,
           #     htmlOutput("geography_calc"))

    ),
    
    # Right-hand column
    column(width = 3, wellPanel(
      
      # Select Geography Type
      uiOutput("geography_typeSelect"),
      
      # Select Geography Name
      uiOutput("geography_nameSelect"),
      
      # Date Range Filter
      dateRangeInput(inputId = 'dateRange',
                     label = 'Date Range',
                     start = '2017-01-01',
                     end = Sys.Date(),
                     separator = ' - ',
                     format = "mm/dd/yy")
    ),
    
    # Generate a Report
    downloadButton("report","Generate Report")
    )
    
  )
)

dashboardPage(
  header,
  sidebar,
  body,
  skin='red'
)
