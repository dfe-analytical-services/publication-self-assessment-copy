library(shiny)
library(shinyjs)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)
library(shinyWidgets)
library(dplyr)
library(janitor)
#library(googlesheets4)
library(rdrop2)


Sys.setenv(no_proxy="*")

#start_data <- drop_read_csv("csv-data.csv") #readRDS("new_tracker_data.rds")


#install.packages('rdrop2')




# token <- drop_auth()
# saveRDS(token, "droptoken.rds")
# # Upload droptoken to your server
# # ******** WARNING ********
# # Losing this file will give anyone 
# # complete control of your Dropbox account
# # You can then revoke the rdrop2 app from your
# # dropbox account and start over.
# # ******** WARNING ********
# # read it back with readRDS
# token <- readRDS("droptoken.rds")
# # Then pass the token to each drop_ function
# drop_acc(dtoken = token)
# 
# 
# 
# new_iris <- drop_read_csv("iris.csv")
# 
# write.csv(iris, file = "iris.csv")
# drop_upload("iris.csv")

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

format_split_table <- function(dataframe, which_rows) {
  
  datatable(data.frame(t(dataframe)[which_rows,]),
            selection = 'single',
            escape = F,
            class = list(stripe = FALSE),
            
            
            extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
            options = list(
              dom = 't',
              #headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}"), # removes header
              # deferRender = TRUE,
              searching = TRUE,
              autoWidth = TRUE,
              columnDefs = list(list(width = '200px', targets = "_all")),
              # scrollCollapse = TRUE,
              rownames = FALSE,
              scroller = TRUE,
              scrollX = TRUE,
              fixedHeader = TRUE,
              class = 'cell-border stripe',
              fixedColumns = list(leftColumns = 1),
              pageLength = 30
            )
            
            
            
            
    # options = list(
    #   dom = 't', # simple table output (add other letters for search, filter etc)
    #   headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}"), # removes header
    #   
    #   searching = TRUE,
    #   scroller = TRUE,
    #   
    #   #extensions = c('Buttons', 'Scroller'),
    #   scrollX = TRUE,
    #   # fixedColumns = list(leftColumns = 2),
    #   fixedHeader=TRUE,
    #   fixedColumns = TRUE,
    #   fixedColumns = list(leftColumns = 1, heightMatch = 'none'),
    #   # buttons = list('excel',
    #   #                list(extend = 'colvis', targets = 0, visible = FALSE)),
    #   autoWidth = TRUE,
    #   columnDefs = list(list(width = '200px', targets = "_all")),
    #   #columnDefs = list(list(width = '200px', targets = "_all")),
    #   pageLength = 25
    # )
    
    ) %>%
  
  
  
    formatStyle(' ', #rownames col (replace with V1, V2 etc for others)
                backgroundColor = '#363b40')  %>% 
    formatStyle(1:ncol(t(dataframe)), 
                color = '#c8c8c8',
                background = '#363b40', # background colour for app is '#363b40'
                target = 'row') %>%
    formatStyle(1:ncol(t(dataframe))-1,
                backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                             c('#34373b', '#5e8742', '#c96c28'))) %>% # red - b05353
    formatStyle(ncol(t(dataframe)):ncol(t(dataframe)),
                backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                             c('#454b51', '#70ad47', '#e87421'))) %>% # red - d45859
    formatStyle(1:ncol(t(dataframe)), `text-align` = 'center') %>%
    formatStyle(1:ncol(t(dataframe)), border = '1px solid #4d5154') %>% 
    formatStyle(1:ncol(t(dataframe)), width='200px')
  
}



