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
      label = "Select Month(s):",
      choices = c("Jan" = 1,
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
      selected = c(1,2,3,4,5,6,7,8,9,10,11,12),
      width = "100%",
      multiple = TRUE
    ),
    
    # Account Selector
    selectInput(
      inputId = "accountInput",
      label = "Select Account(s):",
      choices = c("Barclaycard" = "Barclaycard",
                  "Chase Sapphire" = "ChaseSapphire",
                  "USAA" = "USAAChecking"),
      selected = c("Barclaycard","ChaseSapphire","USAAChecking"),
      width = "100%",
      multiple = TRUE
    )
    
  ),
  
  # Define Dashbard Body
  dashboardBody(
    
    # Set Up Tabs
    tabsetPanel(
      id = "tabs",
      
      # First Tab
      tabPanel(
        title="Time vs. Expenses",
        value="page1",
        
        # Output Line Graph
        withSpinner(
          size = 3,
          plotlyOutput(
            outputId= 'line',
            width="100%",
            height="800px"
        ))
        
      ),
      # End of First Tab
      
      # Second Tab
      tabPanel(
        title = "Graphs",
        value = "page2",
        
        # Output Bar Graph
        withSpinner(
          size = 3,
          plotlyOutput(
            outputId = 'bar',
            width="100%",
            height="975px"
        ))
        
      ),
      # End Second Tab
      
      # Third Tab
      tabPanel(
        title = "Data Table",
        value = "page3",
        
        # Output Data Table
        withSpinner(
          size = 3,
          DT::dataTableOutput(
            outputId = 'tbl',
            width="100%",
            height="100%"
        ))
        
      )
      # End Third Tab
   
    )
    
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
  gs_auth(token="../googlesheets_token.rds")
  
  # Getting Financial Data Sheet
  sheet <- gs_title("Financial Dataframe")
  
  # Read Data From Sheets
  barclaydata <- sheet %>% gs_read(ws = "Barclaycard")
  Sys.sleep(2)
  chasedata <- sheet %>% gs_read(ws = "Chase_Sapphire")
  Sys.sleep(2)
  usaadata <- sheet %>% gs_read(ws = "USAA_Checking")
  
  # Convert Date Columns
  barclaydata$TransactionDate <- as.Date(barclaydata$TransactionDate, format="%m/%d/%Y")
  chasedata$TransactionDate <- as.Date(chasedata$TransactionDate, format="%m/%d/%Y")
  usaadata$TransactionDate <- as.Date(usaadata$TransactionDate, format='%m/%d/%Y')
  
  # Clean USAA Account Data To Remove Payments to Chase and Barclaycard
  usaadata <- usaadata %>% filter(!PaidTo %in% c("Chase","Barclaycard"))
  
  # Bind Together Dataframes
  finances <- bind_rows(barclaydata)
  finances <- bind_rows(finances, chasedata)
  finances <- bind_rows(finances, usaadata)
  finances$year <- lubridate::year(finances$TransactionDate)
  finances$month <- lubridate::month(finances$TransactionDate)
  finances$day <- lubridate::day(finances$TransactionDate)
  
  # Make Year Input Tool
  output$yearInput <- renderUI({
    
    req(finances)
    
    selectInput(
      inputId = "yearInput",
      label = "Select a Year:",
      choices = sort(unique(finances$year)),
      selected = 2018,
      width = "100%"
    )
    
  })
  
  # Reactively Filter Data Based on Year, Month, and Account Input
  reacData <- reactive({
    
    req(finances)
    req(input$monthInput)
    req(input$accountInput)
    
    df <- finances %>% filter(year == input$yearInput) %>%
      filter(month %in% input$monthInput) %>%
      filter(Account %in% input$accountInput)
    
  })
  
  # Create Month vs Spending Line Graph(s)
  output$line <- renderPlotly({
    
    test <- reacData() %>% group_by(month, Category) %>%
      summarise(total = sum(Amount))
    
    cycleplot <- plot_ly(data = test,
                         x = ~month,
                         y = ~total,
                         color = ~Category,
                         type = "scatter",
                         mode = "lines+markers",
                         marker = list(size=12),
                         line = list(width=5)
    ) %>%
      layout(title = ~paste("Monthly Spending"),
             titlefont = list(size=15),
             yaxis = list(title = "Amount ($)",
                          titlefont = list(size=20)),
             xaxis = list(title = "Month of the Year",
                          titlefont = list(size=20))
      )
    
  })
  
  # Create Bar Graphs Based on Year And Month Filter
  output$bar <- renderPlotly({
    
    
    
  })
  
  # Create Data Table Output
  output$tbl <- DT::renderDataTable({
    
    # Turn Reactive Dataframe Into Datatable
    df <- DT::datatable(reacData(),
                        filter = 'top',
                        class = 'cell-border stripe',
                        extensions = c("Scroller","ColReorder","KeyTable"),
                        options = list(pageLength = 20,
                                       autoWidth = TRUE,
                                       paging = TRUE,
                                       searching = TRUE,
                                       scroller = TRUE,
                                       ordering = TRUE,
                                       searchHighlight = TRUE,
                                       scrollY = 700,
                                       scrollX = TRUE,
                                       colReorder = TRUE,
                                       keys = TRUE)
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
