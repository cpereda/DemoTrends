library(shiny)
library(leaflet)



ui <- fluidPage(
  
  titlePanel("US County Health Indicators"),
  
  # Custom CSS to modify the sidebar
  tags$head(
    tags$style(HTML("
      
      .leaflet-map-container {
        height: 100%; 
      }
      
      .leaflet-container {
        height: 100%; 
        width: 100%; 
      }
      
      .sidebar-panel {
      position: absolute;
      top: 100px;
      right: 10px;
      width: 300px;
      background-color: rgba(255, 255, 255, 0.6);
      padding: 10px;
      z-index: 1000;
      }
      
      
      .dataTables_wrapper {
      width: calc(100% - 310px) !important;
      overflow-x: auto !important;
      }
    "))
  ),
  
  # Include sidebar with indicators of interests
  jqui_draggable(
  div(
    class = "sidebar-panel",
    selectInput("State", "Select a state",
                choices = c("Select a state" = "", unique(joineddf3_sf$State)),
                selected = NULL),
    selectInput("County", "Select a county", 
                choices = c("Select a county" = ""),
                selected = NULL),
    
    h4("Race/Ethnicity Indicators"),
    checkboxGroupInput("ethnicity", "Select indicators",
                       choices = c("Black", "White", "Asian", "Native American" = "Native_American", "Hispanic")),
    h4("Socioeconomic Indicators"),
    checkboxGroupInput("other", "Select indicators",
                       choices = c("Uninsured", "Poverty", "No College Degree" = "Uneducated")),
    h4("Health Indicators"),
    checkboxGroupInput("health", "Select indicators",
                       choices = c("Diabetes", "Hypertension", "Obesity"))
  )),
  
  # Add tabsetPanel to create tabs
  tabsetPanel(
    tabPanel("Interactive Map",
             leafletOutput("map", width = "100%", height = "800px"),
             style = "height: calc(100vh - 110px);"
    ),
    tabPanel("Data", dataTableOutput("data")),
    tabPanel("State Comparisons",
             plotOutput("state_map"),
             plotOutput("diabetes_boxplot"),
             plotOutput("hypertension_boxplot"),
             plotOutput("obesity_boxplot")),
    tabPanel("Obesity Comorbidities",
             plotOutput("diabetes_correlation"),
             plotOutput("hypertension_correlation"),
             plotOutput("density"))
  )
  
)