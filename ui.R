#shinyUI(
fluidPage(
  theme = "acalat_theme.css", 
  
  useShinyalert(),
  useShinyjs(),
  
  navbarPage("DfE publication self-assessment tool",
             
             # Homepage ----
             
             tabPanel("Using the app",
                      
                      fluidRow(
                        column(7,
                               h3("What is this app for?"),
                               "This app allows you to assess different aspects of your Official Statistics production processes against best practice.",
                               br(),
                               br(),
                               "You can use the app to:",
                               br(),
                               tags$li("Self-assess against best practice for producing and publishing statistics"),
                               tags$li("Help direct your own improvement work"),
                               tags$li("Find the relevant guidance relating to each bit of best practice"),
                               tags$li("View how improvements have been implemented over time"),
                               br(),
                               h3("How to use the app"),
                               "You should self-assess your publication against each of the criteria within the tool regularly and use the results to help structure your improvement work.",
                               br(),
                               br(),
                               "If you fill it in regularly, you will be able to use the tool to see how work is progressing over time.", 
                               br(),
                               br(),
                               h3("Support available"),
                               "CSSU and the Statistics Development Team are available to support on meeting the best practice described within the tool via email or through the Partnership Programmme.",
                               br(),
                               br(),
                               "Further guidance is also available on the ", a(href = "https://rsconnect/rsc/stats-production-guidance","statistics production guidance website.",target = "_blank"),
                               br(),
                               br(),
                               "Information gathered through this app will be used to inform training events and further guidance as needed.",
                               br(),
                               br(),
                               "For any questions or feedback please contact us - ",
                               a(href = "mailto:statistics.development@education.gov.uk", "statistics.development@education.gov.uk.", target = "_blank")
                        ),
                        column(5,
                               br(),
                               wellPanel(
                                 verticalLayout(
                                   h3("Best practice for publications includes:"),
                                   br(),
                                   "1. Being published via Explore Education Statistics",
                                   "2. Including the maximum time series possible",
                                   "3. Being produced in line with Reproducible Analytical Pipelines (RAP):",
                                   br(),
                                   img(src='hex-diagram.png', align = "left", width="100%"),
                                   br(),
                                   "4. Written content produced in line with the content guidance",
                                   "5. Written content is peer reviewed regularly and feedback acted upon",
                                   "6. Users are actively engaged for feedback"
                                 ))
                        )
                      )
             ),
             
             # Publication progress page ----
             
             tabPanel("Publication self-assessment",
                      
                      ## Modal width
                      tags$head(tags$style(HTML('.modal-lg {width: 1200px;}'))),
                      
                      # center col headers
                      tags$head(
                        tags$style(type = "text/css",
                                   HTML("th { text-align: center; }")
                        )),
                      
                      fluidRow(
                        column(7,
                               div(class = "row",
                                   div(class = "col-sm-3", style = "margin-top: 10px", "Choose publication:"),
                                   div(class = "col-sm-9",   selectizeInput("publication_choice",
                                                                            label = NULL,
                                                                            choices = sort(unique(start_data$publication)),
                                                                            options = list(
                                                                              placeholder = 'Please select a publication',
                                                                              onInitialize = I('function() { this.setValue(""); }')
                                                                            ),
                                                                            width = "100%")))
                        ),
                        column(5, 
                               fluidRow(
                                 column(4, 
                                        actionButton(inputId = "Add_row_head",label = "Add new response", icon = icon("fas fa-plus"),width = "100%")),
                                 column(4, 
                                        actionButton(inputId = "Delete_row_head",label = "Delete response", icon = icon("fas fa-minus"),width = "100%")),
                                 column(4, 
                                        actionButton(inputId = "Add_publication_head",label = "Create new publication", icon = icon("fas fa-file-medical"),width = "100%"))
                               )
                        )),
                      hr(),
                      
                      conditionalPanel("input.publication_choice != ''",
                                       dataTableOutput("main_pub_table1"),
                                       h4(strong("RAP levels - Good")),
                                       dataTableOutput("main_pub_table2"),
                                       h4(strong("RAP levels - Great")),
                                       div(style = "margin-left: 0px", dataTableOutput("main_pub_table3", width = "100%")),
                                       h4(strong("RAP levels - Best")),
                                       div(style = "margin-left: 0px", dataTableOutput("main_pub_table4", width = "100%")),
                                       h4(strong("Continuous improvement")),
                                       div(style = "margin-left: 0px", dataTableOutput("main_pub_table5", width = "100%")),
                                       br(),
                                       fluidRow(align = "right", downloadButton("publication_data_csv", "Download data", width = "80%"))
                      )),
             
             # Overview page ----
             
             tabPanel("Overview of latest responses",
                      dateInput(inputId = "overviewDate", label = "Responses as at:"),
                      fluidRow(
                        column(4,
                               htmlOutput("summary_lines"),
                               br(),
                               br(),
                               h4("RAP steps breakdown:"),
                               textOutput("steps_kpi"),
                               br(),
                               radioButtons("summary_choice", label=NULL, c("Number", "Percentage"), inline = TRUE),
                               tableOutput("summary_rap_practice")),
                        column(8,
                               h4("RAP level breakdown:"),
                               plotOutput("summary_plot_level"))
                      ),
                      hr(),
                      h4("The latest status for each publication:"),
                      column(12,dataTableOutput("overview_table",width = "100%")),
                      fluidRow(align = "right", downloadButton("all_data_csv", "Download data", width = "80%"))
             )
  ),
  
  HTML("<script>var parent = document.getElementsByClassName('navbar-nav');
parent[0].insertAdjacentHTML( 'afterend', '<ul class=\"nav navbar-nav navbar-right\"><li><a href=\"mailto:statistics.development@education.gov.uk\">statistics.development@education.gov.uk</a></li></ul>' );</script>")
  
)

