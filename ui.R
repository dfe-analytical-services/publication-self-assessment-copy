shinyUI(
  fluidPage(
  #theme = "acalat_theme.css",
  
  useShinyalert(),
  
  # tags$head(
  #   tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
  # ),
  # 
  
  titlePanel("Publication tracker"),
  
  ### This is to adjust the width of pop up "showmodal()" for DT modify table 
  tags$head(tags$style(HTML('.modal-lg {width: 1400px;}'))),
  
  helpText("Note: Remember to save any updates!"),
  
  br(),
  
  
  selectInput("publication_choice",
              label = p(strong("Choose a publication")),
              choices = unique(start_data$Publication)),
  
  
  ### tags$head() is to customize the download button
  tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
  
  downloadButton("Trich_csv", "Download in CSV", class="butt"),
  
#  useShinyalert(), # Set up shinyalert
  
  uiOutput("MainBody_trich"),

  br(),

  actionButton(inputId = "Updated_trich",label = "Save")
  
)
)



#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# install.packages("remotes")
# devtools::install_github("AnalytixWare/ShinySky")
#library(shinysky)

# library(shiny)
# library(shinyjs)
# #library(shinysky)
# library(DT)
# library(data.table)
# library(lubridate)
# library(shinyalert)
# useShinyalert()
# Define UI for application that draws a histogram
# shinyUI(fluidPage(
#   
#   # Application title
#   titlePanel("DT Editor Minimal Example"),
#   ### This is to adjust the width of pop up "showmodal()" for DT modify table 
#   tags$head(tags$style(HTML('
#                             .modal-lg {
#                             width: 1200px;
#                             }
#                             '))),
#   helpText("Note: Remember to save any updates!"),
#   br(),
#   ### tags$head() is to customize the download button
#   tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
#   downloadButton("Trich_csv", "Download in CSV", class="butt"),
#   useShinyalert(), # Set up shinyalert
#   uiOutput("MainBody_trich"),actionButton(inputId = "Updated_trich",label = "Save")
# ))