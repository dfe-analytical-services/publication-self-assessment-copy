#shinyUI(
fluidPage(
  theme = "acalat_theme.css",
  
  useShinyalert(),
  useShinyjs(),
  
  navbarPage("DfE publication self assessment tool",
             
             # Homepage ----
             
             tabPanel("How to use the app",
                      
                      fluidRow(
                        column(7,
                               h3("What is this app for?"),
                               "This app allows you to assess different aspects of your Official Statistics production processes against best practice.",
                               br(),
                               br(),
                               "You can use the app to –",
                               br(),
                               "- Self-assess against best practice for producing and publishing statistics",
                               br(),
                               "- Help direct your own improvement work",
                               br(),
                               "- Find the relevant guidance relating to each bit of best practice",
                               br(),
                               "- View how improvements have been implemented over time",
                               br(),
                               h3("How to use the app"),
                               "You should self assess your publication against each of the criteria within the tool regularly and use the results to help structure your improvement work.",
                               br(),
                               "If you fill it in regularly, you will be able to use the tool to see how work is progressing over time.", 
                               h3("Support available"),
                               "CSSU and the Statistics Development Team are available to support on meeting the best practice described within the tool via email or through the Partnership Programmme.",
                               br(),
                               "Further guidance is also available at the ", a(href = "https://rsconnect/rsc/stats-production-guidance","guidance website.",target = "_blank"),
                               br(),
                               "Information gathered through this app will be used to inform training events and further guidance as needed.",
                               br(),
                               "For any questions or feedback please contact us - ",
                               a(href = "mailto:explore.statistics@education.gov.uk", "explore.statistics@education.gov.uk.", target = "_blank")
                        ),
                        column(5,
                               br(),
                               wellPanel(
                                 verticalLayout(
                                   h3("What is best practice?"),
                                   "Best practice for Official Statistics publications includes:",
                                   br(),
                                   "1. Are published on Explore Education Statistics",
                                   "2. Include the maximum time series possible",
                                   "3. Are produced in line with Reproducible Analytical Pipeline (RAP) principles:",
                                   br(),
                                   img(src='hex-diagram.png', align = "left", width="100%"),
                                   br(),
                                   "4. Have content produced in line with the content checklist",
                                   "5. Have content that is peer reviewed regularly and feedback acted upon",
                                   "6. Have active user engagement activities taking place"
                                 ))
                        )
                      )
             ),
             
             # Publication progress page ----
             
             tabPanel("Publication page",
                      
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
                                   div(class = "col-sm-9", selectInput("publication_choice",
                                                                       label = NULL,
                                                                       choices = unique(start_data$publication),
                                                                       width = "100%")))
                        ),
                        column(4, offset = 1,
                               fluidRow(
                                 column(5, 
                                        actionButton(inputId = "Add_row_head",label = "Add column", width = "100%")),
                                 column(5,
                                        ''
                                 )
                               )
                        )
                      ),
                      hr(),
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
             ),
             
             # Overview page ----
             
             tabPanel("Overview page",
                      br(),
                      h4(strong("The latest status for each publication:")),
                      column(12,dataTableOutput("overview_table")),
                      fluidRow(align = "right", downloadButton("all_data_csv", "Download data", width = "80%"))
             )
             
             
             
  ),
  
  tags$script(HTML("var header = $('.navbar> .container-fluid');
                       header.append('<div style=\"float:right\"><h4>explore.statistics@education.gov.uk</h4></div>');
                       console.log(header)"))
)

