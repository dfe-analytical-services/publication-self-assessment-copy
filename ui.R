#shinyUI(
fluidPage(
  theme = "acalat_theme.css",
  
  useShinyalert(),
  useShinyjs(),
  
  titlePanel("Publication tracker"),
  
  HTML("<a href=\"mailto:explore.statistics@education.gov.uk\">explore.statistics@education.gov.uk</a>"),
  
  br(),
  
  shinyjs::hidden(div(
    id = "home_page",
    br(),
    "Some intro stuff for what the app is and why it exists",
    br(),
    br(),
    "Action button for updating a publication's progress",
    br(),
    br(),
    actionButton("add_pub_status_page", "Update a publication", width = "30%"),
    br(),
    br(),
    "Action button for seeing latest for all publications",
    br(),
    br(),
    actionButton("see_overview_page", "Publication overview", width = "30%"),
  )),
  
  shinyjs::hidden(div(
    id = "progress_page",
    fluidRow(
      column(8,
             selectInput("publication_choice",
                         label = p(strong("Choose a publication")),
                         choices = unique(start_data$publication),
                         width = "80%"),
             fluidRow(
               column(4,uiOutput("add_g6")),
               column(4,uiOutput("add_g7")))
      ),
      column(4,
             downloadButton("all_data_csv", "Download in CSV"),#, class="butt"),
             br(),
             actionButton(inputId = "Updated_trich",label = "Save"),
             actionButton("go_to_homepage", "Homepage", width = "30%"),
             uiOutput("add_button"))),
    hr(),
    
    uiOutput("publication_table")
    
  )),
  
  shinyjs::hidden(div(
    id = "overview_page",
    "This is where we'll show a high level summary of the latest info on each publication",
    br(),
    actionButton("go_to_homepage2", "Homepage", width = "30%"),
    column(12,dataTableOutput("overview_table"))
    
  ))
  
)

