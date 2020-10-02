#shinyUI(
fluidPage(
  theme = "acalat_theme.css",
  
  useShinyalert(),
  useShinyjs(),
  
  # Title ----
  
  titlePanel("Publication tracker"),
  
  HTML("<a href=\"mailto:explore.statistics@education.gov.uk\">explore.statistics@education.gov.uk</a>"),
  
  br(),
  
  # Homepage ----
  
  shinyjs::hidden(div(
    id = "home_page",
    br(),
    h3("What is this app for?"),
    "Supporting improvement of statistics products",
    br(),
    "Navigation tool for finding relevant guidance",
    h3("How it might be useful"),
    "To self-assess against best practice",
    br(),
    "To help guide improvement work",
    br(),
    "Directs to relevant guidance",
    br(),
    h3("How to use the app"),
    "Production teams should come to the app to self assess their publications against each of the [criteria] and then use any gaps highlighted to structure their improvement work.",
    br(),
    "Teams should come back to update their publications regularly so they can see how far they've come and how well work is progressing over time.", 
    br(),
    br(),
    actionButton("add_pub_status_page", "View information for a specific publication and/or add a new update", width = "30%"),
    br(),
    br(),
    actionButton("see_overview_page", "View an overview of all publications", width = "30%"),
    br(),
    h3("Support available"),
    "CSSU and the Statistics Production Team are available to support on meeting the [criteria] via email or through the Partnership Programmme.",
    br(),
    "Guidance is available at the guidance link",
    br(),
    "Information gathered through this app will be used to inform training events and further guidance as needed",
  )),

  # Publication progress page ----

  ## Modal width
  tags$head(tags$style(HTML('.modal-lg {width: 1200px;}'))),

  shinyjs::hidden(div(
    id = "progress_page",
    fluidRow(
      hr(),
      column(7,
             div(class = "row",
                 div(class = "col-sm-2", style = "margin-top: 10px", "Choose a publication"),
                 div(class = "col-sm-10", selectInput("publication_choice",
                                                      label = NULL,
                                                      choices = unique(start_data$publication),
                                                      width = "100%"))),
             fluidRow(
               column(6,uiOutput("add_g6")),
               column(6,uiOutput("add_g7")))
      ),
      column(4, offset = 1,
             actionButton("go_to_homepage", "Homepage", width = "74%"),
             br(),
             div(class = "row",
                 div(class = "col-sm-4", actionButton(inputId = "Add_row_head",label = "Add", width = "100%")), #  uiOutput("add_button")),
                 div(class = "col-sm-1",""),
                 div(class = "col-sm-4", actionButton(inputId = "save_data",label = "Save", width = "100%")),
                 div(class = "col-sm-3",""))
            )
      ),
    hr(),

    #div(style = "margin-left: 15px", dataTableOutput("main_pub_table", width = "100%")),
    
    div(style = "margin-left: 15px", dataTableOutput("main_pub_table1", width = "100%")),
    h3("RAP levels - Good"),
    div(style = "margin-left: 15px", dataTableOutput("main_pub_table2", width = "100%")),
    h3("RAP levels - Great"),
    div(style = "margin-left: 15px", dataTableOutput("main_pub_table3", width = "100%")),
    h3("RAP levels - Best"),
    div(style = "margin-left: 15px", dataTableOutput("main_pub_table4", width = "100%")),
    h3("Continuous improvement"),
    div(style = "margin-left: 15px", dataTableOutput("main_pub_table5", width = "100%")),

    br(),
    fluidRow(align = "right", downloadButton("all_data_csv", "Download in CSV", width = "80%"))

  )),

  # Publication overview page ----

  shinyjs::hidden(div(
    id = "overview_page",
    "This is where we'll show a high level summary of the latest info on each publication",
    br(),
    br(),
    actionButton("go_to_homepage2", "Homepage", width = "30%"),
    hr(),
    br(),
    column(12,dataTableOutput("overview_table"))

  ))
  
)

