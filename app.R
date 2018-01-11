#################
## Jim Fairman ##
######################################
## Shiny App For Financial Analysis ##
######################################


###########################
# START LOADING LIBRARIES #
###########################
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(plotly)
library(googlesheets)
library(DT)
########################
# END LOADING LIBARIES #
########################



#####################
# START UI FUNCTION #
#####################
ui <- dashboardPage(
  
  # Define Dashboard Header
  dashboardHeader(
    title = "Fairman Finances",
    titleWidth = 225
    ),
  
  # Define Dashboard Sidebar
  dashboardSidebar(
    width = 225,
    
    # Input for Year
    uiOutput("yearInput"),
    
    # Input for Month
    selectInput(
      inputId = "monthInput",
      label = "Select A Month:",
      choices = c("All Year" = 99,
                  "Jan" = 1,
                  "Feb" = 2,
                  "Mar" = 3,
                  "Apr" = 4,
                  "May" = 5,
                  "Jun" = 6,
                  "Jul" = 7,
                  "Aug" = 8,
                  "Sep" = 9,
                  "Oct" = 10,
                  "Nov" = 11,
                  "Dec" = 12),
      selected = "All Year",
      width = "100%"
    )
  ),
  
  # Define Dashbard Body
  dashboardBody(
    
    # Output Data Table
    DT::dataTableOutput('tbl')
    
  )
  
)
###################
# END UI FUNCTION #
###################



#########################
# START SERVER FUNCTION #
#########################
server <- function(input, output, session) {

  # Authenticate With Google Using Pre-Stored Token
  gs_auth(token="sheets_token.rds")
  
  # Getting Financial Data Sheet
  sheet <- gs_title("Financial Dataframe")
  
  # Read Data From Sheets
  credit_data <- sheet %>% gs_read(ws="Credit Cards")
  other_expenses <- sheet %>% gs_read(ws="Other Expenses")
  
  # Convert Date Columns
  credit_data$TransactionDate <- as.Date(credit_data$TransactionDate, format="%m/%d/%Y")
  other_expenses$TransactionDate <- as.Date(other_expenses$TransactionDate, format="%m/%d/%Y")
  
  # Bind Together Dataframes
  finances <- bind_rows(credit_data, other_expenses)
  finances$year <- lubridate::year(finances$TransactionDate)
  finances$month <- lubridate::month(finances$TransactionDate)
  finances$day <- lubridate::day(finances$TransactionDate)
  
  # Wait For Finances Dataframe
  observeEvent(finances,{
    
    # Make Year Input Tool
    output$yearInput <- renderUI({
      selectInput(
        inputId = "yearInput",
        label = "Select a Year:",
        choices = sort(unique(finances$year)),
        width = "100%"
      )
    })
    
    # Reactively Filter Data Based On Inputs
    dataset <- reactive({
      filtered_data <- finances %>% filter(year == input$yearInput) %>% filter(month == input$monthInput)
    })
    
    # Create Data Table Output
    output$tbl = DT::renderDataTable(
      dataset()
    )
    
    
  })
  
}
#######################
# END SERVER FUNCTION #
#######################



#################
# START RUNNING #
#################
shinyApp(ui = ui, server = server)
###############
# END OF FILE #
###############
