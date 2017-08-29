# VZ Viewer UI

library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
  title = "VZ View"
)

body <- dashboardBody(
  fluidRow(
    
    # Left-hand column
    column(width = 9,
           
           # Map Viewer
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("vzmap", height = 500)
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
  dashboardSidebar(disable = TRUE),
  body
)
