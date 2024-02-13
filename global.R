library(shiny)
library(shinyjs)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)
library(shinyWidgets)
library(plyr)
library(dplyr)
library(janitor)
library(tidyr)
library(DBI)
library(dbplyr)
library(stringr)
library(config)
library(ggplot2)
library(plotly)
library(reactable)
library(markdown)
library(knitr)
library(readr)
library(tidyverse)
library(kableExtra)
library(fontawesome)

# Pulling credentials for server access from config file ----

config <- config::get("db_connection")

connection <- dbConnect(odbc::odbc(),
                        Driver = "SQL Server Native Client 11.0",
                        Server = "T1PRANMSQL\\SQLPROD,60125",
                        Database = "MA_SDT_NS_DATA",
                        Trusted_Connection = "yes")

environment <- if_else(Sys.getenv("SDT_SAT_ENV") == "", "Local", stringr::str_remove(Sys.getenv("SDT_SAT_ENV"), "\\s.*$")) # some string faff as the variables weren't quite saved as I'd intended for them to be
environment <- 'Production'

start_data <- connection %>% tbl(paste0("publicationTracking", environment)) %>% collect()

# Formatting radio button inputs for form ----

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

# Formatting the split tables for publication progress page ----

format_split_table <- function(table,df) {
  
  
  table  %>%
    formatStyle(2:ncol(df),
                color = '#c8c8c8',
                background = '#363b40', # background colour for app is '#363b40'
                target = 'row') %>%
    formatStyle(2:ncol(df)-1,
                backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                             c('#34373b', '#5e8742', '#c96c28'))) %>% # red - b05353
    formatStyle(ncol(df):ncol(df),
                backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                             c('#454b51', '#70ad47', '#e87421'))) %>% # red - d45859
    formatStyle(2:ncol(df), `text-align` = 'center') %>%
    formatStyle(2:ncol(df), border = '1px solid #4d5154') %>%
    formatStyle(2:ncol(df), width='200px')

  
  

  
}



