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
    
    # Set Up Tabs
    tabsetPanel(
      id = "tabs",
      
      # First Tab
      tabPanel(
        title="Time vs. Expenses",
        value="page1",
        
        # Output Line Graph
        withSpinner(plotlyOutput(
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
        withSpinner(plotlyOutput(
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
        withSpinner(DT::dataTableOutput(
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
  gs_auth(token="sheets_token.rds")
  
  # Getting Financial Data Sheet
  sheet <- gs_title("Financial Dataframe")
  
  # Read Data From Sheets
  barclaydata <- sheet %>% gs_read(ws = "Barclaycard")
  chasedata <- sheet %>% gs_read(ws = "Chase_Sapphire")
  usaadata <- sheet %>% gs_read(ws = "USAA_Checking")
  
  # Convert Date Columns
  barclaydata$TransactionDate <- as.Date(barclaydata$TransactionDate, format="%m/%d/%Y")
  chasedata$TransactionDate <- as.Date(chasedata$TransactionDate, format="%m/%d/%Y")
  usaadata$TransactionDate <- as.Date(usaadata$TransactionDate, format='%m/%d/%Y')

    # Bind Together Dataframes
  finances <- bind_rows(barclaydata)
  finances <- bind_rows(finances, chasedata)
  finances <- bind_rows(finances, usaadata)
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
        selected = 2018,
        width = "100%"
      )
    })
    
    # Reactively Filter Data Based on Year Input
    yeardata <- reactive({
      filtered_data <- finances %>% filter(year == input$yearInput)
    })
    
    # Reactively Filter Data Based On Year And Month Inputs
    dataset <- reactive({
      if (input$monthInput == 99) {
        filtered_data <- finances %>% filter(year == input$yearInput)
      } else {
        filtered_data <- finances %>% filter(year == input$yearInput) %>% filter(month == input$monthInput)
      }
    })

    # Create Month vs Spending Line Graph(s)
    output$line <- renderPlotly({
      a <- ggplot(yeardata(), aes(x=month, y=Amount, color=Category)) +
        #geom_line(stat="identity") +
        geom_line() +
        xlab("Month") +
        ylab("Total Amount ($)") +
        scale_x_continuous(limits=c(1,12), breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
        theme_minimal()
      a <- ggplotly(a)
      a
      
    })
    
    # Create Bar Graphs Based on Year And Month Filter
    output$bar <- renderPlotly({
      b <- ggplot(dataset(), aes(x=Category, y=Amount, fill=Category),show.legend=F) +
        geom_bar(stat="identity") +
        xlab("") +
        ylab("Total Amount ($)") +
        theme_minimal() +
        theme_bw(base_size=15) +
        theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1),
              plot.margin = unit(c(1.25,1.25,1.25,1.25), "cm"))
      b <- ggplotly(b)
      b

    })
    
    # Create Data Table Output
    output$tbl <- DT::renderDataTable(
      
      # Turn Reactive Dataframe Into Datatable
      df <- DT::datatable(dataset(),
                          options = list(pageLength = 30)
                          )
      
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
