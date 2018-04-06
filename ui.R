########################
# VZ Dashboard UI Code #
########################

library(shinydashboard)
library(leaflet)

## Header
header <- dashboardHeader(
  # Use horizontal VZ logo for title
  title = tags$a(href='http://visionzero.lacity.org',
                 tags$img(src='vz_horiz.png', height='38px'))
)

## Sidebar Content
sidebar <- dashboardSidebar(

  # Sidebar Menu
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("Key Indicators", tabName="kpi", icon = icon("bar-chart")),
    menuItem("Project Delivery", tabName="ProjectDelivery", icon = icon("road")),
    menuItem("Council Districts", tabName="AreaFilter", icon = icon("map"))#,
    #menuItem("Speed Survey Status", tabName="Surveys", icon = icon("pencil-square-o")),
    #menuItem("BSS PCI Status", tabName="BSS", icon=icon("truck"))
  )
)

## Body Layout
body <- dashboardBody(
  tabItems(
    
    # Key Indicator Content
    tabItem(tabName = "kpi",
            fluidRow(
              valueBoxOutput("DeathsToDate", width = 6),
              valueBoxOutput("PedDeaths", width = 6)
            ),
            fluidRow(
              valueBoxOutput("BikeDeaths", width = 6),
              valueBoxOutput("VehDeaths", width = 6))
            # ),
            # fluidRow(
            #   box(
            #     #title = "Monthly Fatals"
            #     #,status = "primary"
            #     #,solidHeader = TRUE
            #     #,collapsible = TRUE,
            #     plotOutput("MonthlyFatalChart", height = "400px")
            #   )
            # )
    ),

    # Key Indicator Content
    tabItem(tabName = "ProjectDelivery",
            fluidRow(
              box(#status = "warning",
                  width = 12,
                  leafletOutput("projectmap", height = 700)
                  #"boxcontent"
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
                     
                     tabBox(
                       width = NULL,
                       tabPanel("Collisions",tableOutput('lapd_summary')),
                       tabPanel("Improvements", tableOutput('infrastructure_summary'))
                     )
                     
                     # Output crash table
                     # box(width = NULL,
                     #     title = "Collision Summary",
                     #     tableOutput('lapd_summary'))
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
              )#,
              
              # Generate a Report
              #downloadButton("report","Generate Report")
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
