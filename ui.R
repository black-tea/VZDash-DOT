########################
# VZ Dashboard UI Code #
########################

library(shinydashboard)
library(leaflet)

##### Packages from IManager #####
library(shiny)
library(DT)
library(leaflet)
library(shinyjs)
library(sf)
library(dplyr)
appCSS <- ".mandatory_star { color: red; }"
##### End Packages from IManager #####

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
    menuItem("Map", icon = icon("map"), startExpanded = FALSE,
             menuSubItem("Citywide", tabName = "ProjectDelivery"),
             menuSubItem("Area Filter", tabName =  'AreaFilter'))
    # ,
    # menuItem("Infrastructure", icon = icon("road"), startExpanded = FALSE,
    #          menuSubItem("Add", tabName = "AddI"),
    #          menuSubItem("View", tabName = "ManageI"))
  )
)

## Body Layout
body <- dashboardBody(
  tags$style(HTML(".datepicker {z-index:99999 !important;}")),
  tabItems(
    
    # Key Indicator Content
    tabItem(tabName = "kpi",
            tabBox(
              width = 6,
              title = textOutput('collision_title'),
              tabPanel("Table",tableOutput("fatalSummaryKPI")),
              tabPanel("Graph",plotOutput("slopegraph"))
            ),
            
            box(
              width = 6,
              title = "Citywide Improvements",
              tableOutput("citywideInfrastructureSummary")
            ),
            h4("Beta")

    ),

    # Key Indicator Content
    tabItem(tabName = "ProjectDelivery",
            fluidRow(
              box(#status = "warning",
                  width = 12,
                  leafletOutput("projectmap", height = 700))
                  #"boxcontent"
                  # ),
    #           # Box with download information
    #           box(width = 6,
    #               title = "Download Treatment Data",
    #               downloadButton('downloadData', 'Download')))
    )),

    # Area Filter Content
    tabItem(tabName = "AreaFilter",
            fluidRow(
              
              # Left-hand column
              column(width = 9,
                     
                     # Map Viewer
                     box(width = NULL, solidHeader = TRUE,
                         leafletOutput("filterMap", height = 500)),
                     
                     tabBox(
                       width = NULL,
                       tabPanel("YTD Fatalities",tableOutput('fatalSummaryFilter')),
                       tabPanel("Improvements", tableOutput('infrastructureSummary')))
                     
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
                uiOutput("geographyTypeSelect"),
                
                # Select Geography Name
                uiOutput("geographyNameSelect")#,
                
              #   # Date Range Filter
              #   dateRangeInput(inputId = 'dateRange',
              #                  label = 'Date Range',
              #                  start = '2017-01-01',
              #                  end = Sys.Date(),
              #                  separator = ' - ',
              #                  format = "mm/dd/yy")
              )#,
              
              # Generate a Report
              #downloadButton("report","Generate Report")
              )
              
            )
    ),
    
    # Add Infrastructure Page
    tabItem(tabName = "AddI",
            box(width = 12,
              fluidPage(
                # Enable javascript
                shinyjs::useShinyjs(),
                shinyjs::inlineCSS(appCSS),
  
                div(
                  id = "form",
                  fluidRow(
  
                    # First UI Bin
                    column(4,
                           uiOutput("treatment_type"),
                           uiOutput("intSelect"),
                           uiOutput("treatment_status")),
  
                    # Second UI Bin
                    column(4,
                           uiOutput("treatment_info1")),
  
                    # Third UI Bin
                    column(4,
                           uiOutput("treatment_info2"),
                           conditionalPanel(condition = "input.treatment_type != null && input.treatment_type.length > 0",
                                            actionButton("submit", "Submit", class = "btn-default"))
                           )
                  ),
                  #hr(),
                  h6(uiOutput("message"), align="center"),
                  hr(),
  
                  # Map Output
                  leafletOutput("infrastructureManagerMap")
                )
            
              )
            )
            ),
    # Add Infrastructure Page
    tabItem(tabName = "ManageI",
            tabBox(
              width = 12,
              tabPanel("SFS",DTOutput('sfs')),
              tabPanel("FB",DTOutput('fb')),
              tabPanel("RFG",DTOutput('rfg'))))
  )
)

dashboardPage(
  #tags$style(HTML(".datepicker {z-index:99999 !important;}")),
  header,
  sidebar,
  body,
  skin='red'
)
