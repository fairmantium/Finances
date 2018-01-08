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
########################
# END LOADING LIBARIES #
########################



#####################
# START UI FUNCTION #
#####################
ui <- dashboardPage(

		    )
###################
# END UI FUNCTION #
###################



#########################
# START SERVER FUNCTION #
#########################
server <- funcion(input, output, session) {

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
