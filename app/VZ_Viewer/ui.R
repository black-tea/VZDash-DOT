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
               tableOutput('lapd_summary')
               ),
           
           # Basic Output Statistics
           box(width = NULL,
               htmlOutput("geography_calc"))

    ),
    
    # Right-hand column
    column(width = 3,
           box(width = NULL, #status = "warning",
               uiOutput("geography_typeSelect")
              ),
           box(width = NULL, #status = "warning",
               uiOutput("geography_nameSelect")
              ),
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
