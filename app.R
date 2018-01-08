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
    width = 225
  ),
  
  # Define Dashbard Body
  dashboardBody(
    
  )
  
)
###################
# END UI FUNCTION #
###################



#########################
# START SERVER FUNCTION #
#########################
server <- function(input, output, session) {

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
