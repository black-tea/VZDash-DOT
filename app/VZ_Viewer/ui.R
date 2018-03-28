########################
# VZ Dashboard UI Code #
########################

library(shinydashboard)
library(leaflet)

## Header
header <- dashboardHeader(
<<<<<<< HEAD
  title = "Vision Zero"
=======
  title = "VZ LA Viewer"
>>>>>>> origin/master
)

## Sidebar Content
sidebar <- dashboardSidebar(
  sidebarMenu(
<<<<<<< HEAD
    menuItem("Citywide Stats", tabName="citywide", icon = icon("bar-chart")),
    menuItem("Project Delivery", tabName="ProjectDelivery", icon = icon("bar-chart")),
    menuItem("By Council District", tabName="AreaFilter", icon = icon("map"))#,
    #menuItem("Speed Survey Status", tabName="Surveys", icon = icon("pencil-square-o")),
    #menuItem("BSS PCI Status", tabName="BSS", icon=icon("truck"))
=======
    menuItem("Citywide Dashboard", tabName="citywide", icon = icon("bar-chart")),
    menuItem("Area / Corridor Filter", tabName="AreaFilter", icon = icon("map")),
    menuItem("Speed Survey Status", tabName="Surveys", icon = icon("pencil-square-o")),
    menuItem("BSS PCI Status", tabName="BSS", icon=icon("truck"))
>>>>>>> origin/master
    
  )
)

## Body Layout
body <- dashboardBody(
  tabItems(
    
    # Citywide Dashboard Content
    tabItem(tabName = "citywide",
      fluidRow(
        valueBoxOutput("DeathsToDate")
      ),
      fluidRow(
        valueBoxOutput("PedDeaths"),
        valueBoxOutput("BikeDeaths"),
        valueBoxOutput("VehDeaths")
      ),
      fluidRow( 
        box(
          #title = "Monthly Fatals"
          #,status = "primary"
          #,solidHeader = TRUE 
          #,collapsible = TRUE, 
          plotOutput("MonthlyFatalChart", height = "400px")
        )
      )
    ),
    
    # Area Filter Content
    tabItem(tabName = "AreaFilter",
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
  )
)

dashboardPage(
  header,
  sidebar,
  body,
  skin='red'
)
