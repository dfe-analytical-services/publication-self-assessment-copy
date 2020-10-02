server <- function(input, output, session){
  
  # Main dataset ----
  
  all_data<-reactiveValues()
  all_data$Data<-readRDS("new_tracker_data.rds") 
  
  # Publication progress table ---- 
  
  
  output$test_table <- renderDataTable({
    
    DT = all_data$Data
  })  
    
  output$main_pub_table1 <- renderDataTable({
    
    DT = all_data$Data %>% dplyr::filter(publication == input$publication_choice)
    
    datatable(data.frame(t(DT)[c(1,5:6),]),
              selection = 'single',
              escape = F,
              class = list(stripe = FALSE),
              options = list(
                dom = 't', # simple table output (add other letters for search, filter etc)
                headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}"), # removes header
                # autoWidth = TRUE,
                # columnDefs = list(list(width = '200px', targets = "_all")),
                pageLength = 25
              )) %>% 
      formatStyle(' ', #rownames col (replace with V1, V2 etc for others)
                  backgroundColor = '#363b40')  %>% 
      formatStyle(1:ncol(t(DT)), 
                  color = '#c8c8c8',
                  background = '#363b40', # background colour for app is '#363b40'
                  target = 'row') %>%
      formatStyle(1:ncol(t(DT))-1,
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#b05353', '#5e8742', '#c96c28'))) %>% 
      formatStyle(ncol(t(DT)):ncol(t(DT)),
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#d45859', '#70ad47', '#e87421'))) %>% 
      formatStyle(1:ncol(t(DT)), `text-align` = 'center') %>%
      formatStyle(1:ncol(t(DT)), border = '1px solid #4d5154') %>% 
      formatStyle(1:ncol(t(DT)), width='200px')
    
  })
  
  output$main_pub_table2 <- renderDataTable({
    
    DT = all_data$Data %>% dplyr::filter(publication == input$publication_choice)
    
    datatable(data.frame(t(DT)[c(7:13),]),
              selection = 'single',
              escape = F,
              class = list(stripe = FALSE),
              options = list(
                dom = 't', # simple table output (add other letters for search, filter etc)
                headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}"), # removes header
                # autoWidth = TRUE,
                # columnDefs = list(list(width = '200px', targets = "_all")),
                pageLength = 25
              )) %>% 
      formatStyle(' ', #rownames col (replace with V1, V2 etc for others)
                  backgroundColor = '#363b40')  %>% 
      formatStyle(1:ncol(t(DT)), 
                  color = '#c8c8c8',
                  background = '#363b40', # background colour for app is '#363b40'
                  target = 'row') %>%
      formatStyle(1:ncol(t(DT))-1,
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#b05353', '#5e8742', '#c96c28'))) %>% 
      formatStyle(ncol(t(DT)):ncol(t(DT)),
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#d45859', '#70ad47', '#e87421'))) %>% 
      formatStyle(1:ncol(t(DT)), `text-align` = 'center') %>%
      formatStyle(1:ncol(t(DT)), border = '1px solid #4d5154') %>% 
      formatStyle(1:ncol(t(DT)), width='200px')
    
  })
  
  output$main_pub_table3 <- renderDataTable({
    
    DT = all_data$Data %>% dplyr::filter(publication == input$publication_choice)
    
    datatable(data.frame(t(DT)[c(14:19),]),
              selection = 'single',
              escape = F,
              class = list(stripe = FALSE),
              options = list(
                dom = 't', # simple table output (add other letters for search, filter etc)
                headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}"), # removes header
                # autoWidth = TRUE,
                # columnDefs = list(list(width = '200px', targets = "_all")),
                pageLength = 25
              )) %>% 
      formatStyle(' ', #rownames col (replace with V1, V2 etc for others)
                  backgroundColor = '#363b40')  %>% 
      formatStyle(1:ncol(t(DT)), 
                  color = '#c8c8c8',
                  background = '#363b40', # background colour for app is '#363b40'
                  target = 'row') %>%
      formatStyle(1:ncol(t(DT))-1,
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#b05353', '#5e8742', '#c96c28'))) %>% 
      formatStyle(ncol(t(DT)):ncol(t(DT)),
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#d45859', '#70ad47', '#e87421'))) %>% 
      formatStyle(1:ncol(t(DT)), `text-align` = 'center') %>%
      formatStyle(1:ncol(t(DT)), border = '1px solid #4d5154') %>% 
      formatStyle(1:ncol(t(DT)), width='200px')
    
  })
  
  output$main_pub_table4 <- renderDataTable({
    
    DT = all_data$Data %>% dplyr::filter(publication == input$publication_choice)
    
    datatable(data.frame(t(DT)[c(20:25),]),
              selection = 'single',
              escape = F,
              class = list(stripe = FALSE),
              options = list(
                dom = 't', # simple table output (add other letters for search, filter etc)
                headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}"), # removes header
                # autoWidth = TRUE,
                # columnDefs = list(list(width = '200px', targets = "_all")),
                pageLength = 25
              )) %>% 
      formatStyle(' ', #rownames col (replace with V1, V2 etc for others)
                  backgroundColor = '#363b40')  %>% 
      formatStyle(1:ncol(t(DT)), 
                  color = '#c8c8c8',
                  background = '#363b40', # background colour for app is '#363b40'
                  target = 'row') %>%
      formatStyle(1:ncol(t(DT))-1,
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#b05353', '#5e8742', '#c96c28'))) %>% 
      formatStyle(ncol(t(DT)):ncol(t(DT)),
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#d45859', '#70ad47', '#e87421'))) %>% 
      formatStyle(1:ncol(t(DT)), `text-align` = 'center') %>%
      formatStyle(1:ncol(t(DT)), border = '1px solid #4d5154') %>% 
      formatStyle(1:ncol(t(DT)), width='200px')
    
  })
  
  output$main_pub_table5 <- renderDataTable({
    
    DT = all_data$Data %>% dplyr::filter(publication == input$publication_choice)
    
    datatable(data.frame(t(DT)[c(26:28),]),
              selection = 'single',
              escape = F,
              class = list(stripe = FALSE),
              options = list(
                dom = 't', # simple table output (add other letters for search, filter etc)
                headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}"), # removes header
                # autoWidth = TRUE,
                # columnDefs = list(list(width = '200px', targets = "_all")),
                pageLength = 25
              )) %>% 
      formatStyle(' ', #rownames col (replace with V1, V2 etc for others)
                  backgroundColor = '#363b40')  %>% 
      formatStyle(1:ncol(t(DT)), 
                  color = '#c8c8c8',
                  background = '#363b40', # background colour for app is '#363b40'
                  target = 'row') %>%
      formatStyle(1:ncol(t(DT))-1,
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#b05353', '#5e8742', '#c96c28'))) %>% 
      formatStyle(ncol(t(DT)):ncol(t(DT)),
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#d45859', '#70ad47', '#e87421'))) %>% 
      formatStyle(1:ncol(t(DT)), `text-align` = 'center') %>%
      formatStyle(1:ncol(t(DT)), border = '1px solid #4d5154') %>% 
      formatStyle(1:ncol(t(DT)), width='200px')
    
  })
  
  # Overview table ----
  
  output$overview_table <- renderDataTable({
  
    table <- all_data$Data %>%
      group_by(publication ) %>% 
      summarise_all(last)
    
    datatable(table,
              selection = 'single',
              escape = F,
              class = list(stripe = FALSE),
              options = list(
                dom = 't', # simple table output (add other letters for search, filter etc)
                headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}"), # removes header
                pageLength = 100
              )) %>% 
      formatStyle(#columns=colnames(t(DT)),
        1:ncol(table),
        color = '#c8c8c8',
        background = '#363b40', # background colour for app is '#363b40'
        target = 'row') %>%
      formatStyle(1:ncol(table),
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#d45859', '#70ad47', '#e87421'))) %>% 
      formatStyle(1:ncol(table), `text-align` = 'center') %>%
      formatStyle(1:ncol(table), border = '1px solid #4d5154')
    
  })
  
  # G6 text box ----
  
  output$add_g6<-renderUI({
    
    #DT <- all_data$Data %>% dplyr::filter(publication == input$publication_choice)
    
    div(class = "row",
        div(class = "col-sm-1", style = "margin-top: 10px", "G6:"),
        div(class = "col-sm-11", textInput("T2_add",label = NULL, value = t(all_data$Data %>% dplyr::filter(publication == input$publication_choice))[2,ncol(t(all_data$Data %>% dplyr::filter(publication == input$publication_choice)))], width = "100%")))
        #div(class = "col-sm-11", textInput("T2_add",label = NULL, width = "100%")))
    
  })
  
  # G7 text box ----
  
  output$add_g7<-renderUI({
    
    DT <- all_data$Data %>% dplyr::filter(publication == input$publication_choice)
    
    div(class = "row",
        div(class = "col-sm-1", style = "margin-top: 10px","G7:"),
        div(class = "col-sm-11", textInput("T3_add",label = NULL, value = t(DT)[3,ncol(t(DT))], width = "100%")))
    
  })
  
  # Add latest progress modal 
  
  observeEvent(input$Add_row_head, {
    
    DT = all_data$Data %>% dplyr::filter(publication == input$publication_choice)
    
    showModal(modalDialog(title = "Add a new row",
                          rag_it(
                            "Is the publication published on Explore Education Statistics??",
                            "T5_add",
                            5,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/ees.html",
                              "How to publish on EES",
                              target = "_blank"
                            )
                          ),   #Publishing on EES
                          div(class = "row",
                              div(class = "col-sm-4","Average time series length"),
                              div(class = "col-sm-3", textInput("T6_add",label = NULL, value = t(DT)[6,ncol(t(DT))])),
                              div(class = "col-sm-5", a(href = "https://rsconnect/rsc/stats-production-guidance/pub.html","How much data should be published",target = "_blank" ))),
                          hr(),
                          strong("RAP levels - Good"),
                          br(),
                          br(),
                          rag_it(
                            "Is all data processing done with code script(s)?",
                            "T7_add",
                            7,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "What processing with code scripts looks like",
                              target = "_blank"
                            )
                          ),   #Processing is done with code
                          rag_it(
                            "Is there a sensible folder/file structure?",
                            "T8_add",
                            8,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "How to set up a sensible folder/file structure",
                              target = "_blank"
                            )
                          ), #Sensible folder /  file structure
                          rag_it(
                            "Are approporiate tools being used to create data files?",
                            "T9_add",
                            9,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/ud.html#how_to_check_against_the_standards",
                              "What the approriate tools look like",
                              target = "_blank"
                            )
                          ), #Use approporiate tools
                          rag_it(
                            "Is all source data stored in single database?",
                            "T10_add",
                            10,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "How to set up and import data into a SQL database",
                              target = "_blank"
                            )
                          ), #All source data stored in single database
                          rag_it(
                            "Is there suitable documentation?",
                            "T11_add",
                            11,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "What suitable documentation looks like",
                              target = "_blank"
                            )
                          ), #Documentation
                          rag_it(
                            "Do all data files meet the data standards?",
                            "T12_add",
                            12,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "How to check if data files meet the standards",
                              target = "_blank"
                            )
                          ), #Files meet data standards
                          rag_it(
                            "Are there basic automated QA checks?",
                            "T13_add",
                            13,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "What basic automated QA checks look like",
                              target = "_blank"
                            )
                          ), #Basic automated QA
                          hr(),
                          strong("RAP levels - Great"),
                          br(),
                          br(),
                          rag_it(
                            "Is the code recyclable for future use?",
                            "T14_add",
                            14,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "How to know if code is recyclable",
                              target = "_blank"
                            )
                          ), #Recyclable code for future use
                          rag_it(
                            "Is each data file produced with a single code script?",
                            "T15_add",
                            15,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "What producing a file with a single code script looks like",
                              target = "_blank"
                            )
                          ), #Single production script(s)
                          rag_it(
                            "Are final code script(s) saved in a version controlled repository?",
                            "T16_add",
                            16,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "How to set up and use a version controlled repository",
                              target = "_blank"
                            )
                          ), #Version controlled final versions of code
                          rag_it(
                            "Are automated summaries generated for insight?",
                            "T17_add",
                            17,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "What generating automated summaries to provide insight looks like",
                              target = "_blank"
                            )
                          ), #Automated summaries generated for insight
                          rag_it(
                            "Has all code been peer reviewed within team?",
                            "T18_add",
                            18,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "How to peer review code",
                              target = "_blank"
                            )
                          ), #Peer review of code within team
                          rag_it(
                            "Are there publication specifc automated QA checks?",
                            "T19_add",
                            19,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "What publication specific automated QA checks look like",
                              target = "_blank"
                            )
                          ), #Publication specifc automated QA
                          hr(),
                          strong("RAP levels - Best"),
                          br(),
                          br(),
                          rag_it(
                            "Is code collaboratively developed using git?",
                            "T20_add",
                            20,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "What using git looks like",
                              target = "_blank"
                            )
                          ), #Collab and develop code using git
                          rag_it(
                            "Are publication specific automated summaries generated for insight?",
                            "T21_add",
                            21,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "How to generate publication specific summaries to provide insight",
                              target = "_blank"
                            )
                          ), #Subject specific automated insights
                          rag_it(
                            "Are data files produced via single code script(s) with integrated QA?",
                            "T22_add",
                            22,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "What producing a file with integrated QA via a single code script looks like",
                              target = "_blank"
                            )
                          ), #Subject specific automated insights
                          rag_it(
                            "Can the publication be reproduced using a single code script?",
                            "T23_add",
                            23,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "How to produce a publication using a single run script",
                              target = "_blank"
                            )
                          ), #Single publication script
                          rag_it(
                            "Are final code script(s) cleanly formatted?",
                            "T24_add",
                            24,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "What cleanly formatted code scripts look like",
                              target = "_blank"
                            )
                          ), #Clean final code
                          rag_it(
                            "Has all code been peer reviewed from outside the team?",
                            "T25_add",
                            25,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html",
                              "Where to go to request an external peer review",
                              target = "_blank"
                            )
                          ), #Peer review of code from outside the team
                          hr(),
                          strong("Continuous improvement"),
                          br(),
                          br(),
                          # Content peer review
                          div(class = "row",
                              div(class = "col-sm-4","Has the latest publication content been peer reviewed?"),
                              div(class = "col-sm-3", textInput("T26_add",label = NULL, value = t(DT)[26,ncol(t(DT))])),
                              div(class = "col-sm-5", a(href = "https://rsconnect/rsc/stats-production-guidance/pub.html","Where to go to request an external peer review",target = "_blank" ))),
                          # Targetted user research activities
                          div(class = "row",
                              div(class = "col-sm-4","What targetted user research activites are taking place?"),
                              div(class = "col-sm-3", textInput("T27_add",label = NULL, value = t(DT)[27,ncol(t(DT))])),
                              div(class = "col-sm-5", a(href = "https://rsconnect/rsc/stats-production-guidance/pub.html","What targetted user research looks like",target = "_blank" ))),
                          # L&D
                          div(class = "row",
                              div(class = "col-sm-4","Any L&D requests or needs"),
                              div(class = "col-sm-3", textInput("T28_add",label = NULL, value = t(DT)[28,ncol(t(DT))])),
                              div(class = "col-sm-5", a(href = "https://rsconnect/rsc/stats-production-guidance/l+d.html","L&D resources",target = "_blank" ))),

                          actionButton("go", "Add item"),
                          
                          easyClose = TRUE, footer = NULL
                          , size = "l" 
                          ))
    
    
  })
  
  # Adding new data to main data file ----
  
  observeEvent(input$go, {
    
    new_row=data.frame(
      
      ï..date = as.character(Sys.Date()), 
      g6 = input[["T2_add"]],
      tl = input[["T3_add"]],                                           
      publication = input$publication_choice, 
      published_on_ees = input[["T5_add"]],
      time_series_length = input[["T6_add"]],
      processing_with_code = input[["T7_add"]],
      sensible_folder_file_structure = input[["T8_add"]],
      approporiate_tools = input[["T9_add"]],             
      single_database = input[["T10_add"]],
      documentation = input[["T11_add"]],
      files_meet_data_standards = input[["T12_add"]],
      basic_automated_qa = input[["T13_add"]],
      recyclable_code = input[["T14_add"]],
      single_data_production_scripts = input[["T15_add"]],
      final_code_in_repo = input[["T16_add"]],
      automated_insight_summaries = input[["T17_add"]],
      peer_review_within_team = input[["T18_add"]],
      publication_specifc_automated_qa = input[["T19_add"]],
      collab_develop_using_git = input[["T20_add"]],
      pub_specific_automated_insight_summaries = input[["T21_add"]],
      single_data_production_scripts_with_qa = input[["T22_add"]],
      single_publication_script = input[["T23_add"]],
      clean_final_code = input[["T24_add"]],
      peer_review_outside_team = input[["T25_add"]],
      content_peer_review = input[["T26_add"]],
      targetted_user_research = input[["T27_add"]],
      l_and_d_requests = input[["T28_add"]]
    )
    
    all_data$Data<-rbind(all_data$Data,new_row )
    removeModal()
  })
   
  # Update rds file ----
   
  observeEvent(input$save_data,{
    saveRDS(all_data$Data, "new_tracker_data.rds")
    shinyalert(title = "Saved!", type = "success")
  })
  
  # UI shenanigans ----
  
  shinyjs::showElement(id = "home_page")
  
  observeEvent(input$add_pub_status_page, {
    shinyjs::hideElement(id = "home_page")
    shinyjs::showElement(id = "progress_page")
    shinyjs::hideElement(id = "overview_page")
  })
  
  observeEvent(input$go_to_homepage, {
    shinyjs::showElement(id = "home_page")
    shinyjs::hideElement(id = "progress_page")
    shinyjs::hideElement(id = "overview_page")
  })
  
  observeEvent(input$see_overview_page, {
    shinyjs::showElement(id = "overview_page")
    shinyjs::hideElement(id = "home_page")
    shinyjs::hideElement(id = "progress_page")
  })
  
  observeEvent(input$go_to_homepage2, {
    shinyjs::showElement(id = "home_page")
    shinyjs::hideElement(id = "progress_page")
    shinyjs::hideElement(id = "overview_page")
  })
  
  # Download data ----
  
  output$all_data_csv<- downloadHandler(
    filename = function() {
      paste("All data", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(all_data$Data), file, row.names = F)
    }
  )
  
  # Stop app ----
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}