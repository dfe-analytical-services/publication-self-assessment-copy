library(shiny)
library(shinyjs)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)
library(shinyWidgets)
library(dplyr)

start_data <- readRDS("new_tracker_data.rds")

rag_it <- function(label_text, input_id, row_num, data_t, help_text){
  
  div(
    class = "row",
    div(
      class = "col-sm-4",
      label_text,
    ),
    div(
      class = "col-sm-3",
      style = "align-content: center",
      prettyRadioButtons(
        inputId = input_id,
        label = NULL,
        choices = c("Yes", "No", "Working on it"),
        selected = t(data_t)[row_num,ncol(t(data_t))], 
        status = "primary",
        shape = "curve",
        animation = "pulse",
        bigger = TRUE,
        inline = TRUE
      )),
    div(
      class = "col-sm-5",
      help_text
    )
  )
  
}