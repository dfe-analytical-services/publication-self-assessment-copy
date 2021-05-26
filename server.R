server <- function(input, output, session){

  # Main dataset ----
  
  all_data <- reactiveValues()
  
  # Pulling from SQL ----

  all_data$Data <- connection %>% tbl(paste0("publicationTracking", environment)) %>% collect() %>% arrange(date) 
  
  table_data <- reactive({
    
    y <- all_data$Data %>% mutate(date = as.Date(date)) %>% dplyr::filter(publication == input$publication_choice) %>% t(.) %>% row_to_names(row_number = 1)
    
    x <- tibble::rownames_to_column(as.data.frame(y))
    
    x$rowname <- case_when(
      x$rowname == "g6" ~ "Grade 6",
      x$rowname == "tl" ~ "Grade 7",
      x$rowname == "publication" ~ "Publication",
      x$rowname == "published_on_ees" ~ "Publication is published on EES",
      x$rowname == "time_series_length" ~ "Maximum time series published",
      x$rowname == "processing_with_code" ~ "Processing is done with code",
      x$rowname == "sensible_folder_file_structure" ~ "Sensible folder and file structure",
      x$rowname == "approporiate_tools" ~ "Use approporiate tools",
      x$rowname == "single_database" ~ "All source data stored in single database",
      x$rowname == "documentation" ~ "Documentation",
      x$rowname == "files_meet_data_standards" ~ "Files meet data standards",
      x$rowname == "basic_automated_qa" ~ "Basic automated QA",
      x$rowname == "recyclable_code" ~ "Recyclable code for future use",
      x$rowname == "single_data_production_scripts" ~ "Single production scripts",
      x$rowname == "final_code_in_repo" ~ "Version controlled final code scripts",
      x$rowname == "automated_insight_summaries" ~ "Automated summaries",
      x$rowname == "peer_review_within_team" ~ "Peer review of code within team",
      x$rowname == "publication_specifc_automated_qa" ~ "Publication specifc automated QA",
      x$rowname == "collab_develop_using_git" ~ "Collaboratively develop code using git",
      x$rowname == "pub_specific_automated_insight_summaries" ~ "Publication specific automated summaries",
      x$rowname == "single_data_production_scripts_with_qa" ~ "Single production scripts with integrated QA",
      x$rowname == "single_publication_script" ~ "Single publication production script",
      x$rowname == "clean_final_code" ~ "Clean final code",
      x$rowname == "peer_review_outside_team" ~ "Peer review of code from outside the team",
      x$rowname == "content_checklist" ~ "Content checklist",
      x$rowname == "content_peer_review" ~ "Content peer review",
      x$rowname == "targetted_user_research" ~ "Targetted user research activities",
      x$rowname == "l_and_d_requests" ~ "L&D requests"
    )
    
    names(x)[names(x) == 'rowname'] <- ''
    
    return(x)

  })

  # Publication progress tables ---- 
  
  output$main_pub_table1 <- renderDataTable({ 
    
    x <- table_data() 
    
    datatable(x[4:5,],
              rownames = FALSE,
              class = list(stripe = FALSE),
              selection = 'none',
              options = list(
                dom = 't', # simple table output (add other letters for search, filter etc)
                bSort=FALSE,
                pageLength = 30,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'color': '#c8c8c8'});",
                  "}")
              ) 
    )  %>%
      formatStyle(2:ncol(x),
                  color = '#c8c8c8',
                  background = '#363b40', # background colour for app is '#363b40'
                  target = 'row') %>%
      formatStyle(2:ncol(x)-1,
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#34373b', '#5e8742', '#c96c28'))) %>% # red - b05353
      formatStyle(ncol(x):ncol(x),
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#454b51', '#70ad47', '#e87421'))) %>% # red - d45859
      formatStyle(2:ncol(x), `text-align` = 'center') %>%
      formatStyle(2:ncol(x), border = '1px solid #4d5154') %>%
      formatStyle(2:ncol(x), width='200px')
    #             
  })
  
  output$main_pub_table2 <- renderDataTable({ 
    
    x <- table_data()
    
    datatable(x[6:12,],
              rownames = FALSE,
              class = list(stripe = FALSE),
              selection = 'none',
              options = list(
                dom = 't', # simple table output (add other letters for search, filter etc)
                headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}"), # removes header
                bSort=FALSE,
                pageLength = 30
              ) 
    )  %>%
      formatStyle(2:ncol(x),
                  color = '#c8c8c8',
                  background = '#363b40', # background colour for app is '#363b40'
                  target = 'row') %>%
      formatStyle(2:ncol(x)-1,
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#34373b', '#5e8742', '#c96c28'))) %>% # red - b05353
      formatStyle(ncol(x):ncol(x),
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#454b51', '#70ad47', '#e87421'))) %>% # red - d45859
      formatStyle(2:ncol(x), `text-align` = 'center') %>%
      formatStyle(2:ncol(x), border = '1px solid #4d5154') %>%
      formatStyle(2:ncol(x), width='200px')
    #             
  })
  
  output$main_pub_table3 <- renderDataTable({ 
    
    x <- table_data()
    
    datatable(x[13:18,],
              rownames = FALSE,
              class = list(stripe = FALSE),
              selection = 'none',
              options = list(
                dom = 't', # simple table output (add other letters for search, filter etc)
                headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}"), # removes header
                bSort=FALSE,
                pageLength = 30
              ) 
    )  %>%
      formatStyle(2:ncol(x),
                  color = '#c8c8c8',
                  background = '#363b40', # background colour for app is '#363b40'
                  target = 'row') %>%
      formatStyle(2:ncol(x)-1,
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#34373b', '#5e8742', '#c96c28'))) %>% # red - b05353
      formatStyle(ncol(x):ncol(x),
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#454b51', '#70ad47', '#e87421'))) %>% # red - d45859
      formatStyle(2:ncol(x), `text-align` = 'center') %>%
      formatStyle(2:ncol(x), border = '1px solid #4d5154') %>%
      formatStyle(2:ncol(x), width='200px')
    #             
  })
  
  output$main_pub_table4 <- renderDataTable({ 
    
    x <- table_data()
    
    datatable(x[19:24,],
              rownames = FALSE,
              class = list(stripe = FALSE),
              selection = 'none',
              options = list(
                dom = 't', # simple table output (add other letters for search, filter etc)
                headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}"), # removes header
                bSort=FALSE,
                pageLength = 30
              ) 
    )  %>%
      formatStyle(2:ncol(x),
                  color = '#c8c8c8',
                  background = '#363b40', # background colour for app is '#363b40'
                  target = 'row') %>%
      formatStyle(2:ncol(x)-1,
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#34373b', '#5e8742', '#c96c28'))) %>% # red - b05353
      formatStyle(ncol(x):ncol(x),
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#454b51', '#70ad47', '#e87421'))) %>% # red - d45859
      formatStyle(2:ncol(x), `text-align` = 'center') %>%
      formatStyle(2:ncol(x), border = '1px solid #4d5154') %>%
      formatStyle(2:ncol(x), width='200px')
    #             
  })
  
  
  output$main_pub_table5 <- renderDataTable({ 
    
    x <- table_data()
    
    datatable(x[25:29,],
              rownames = FALSE,
              class = list(stripe = FALSE),
              selection = 'none',
              options = list(
                dom = 't', # simple table output (add other letters for search, filter etc)
                headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}"), # removes header
                bSort=FALSE,
                pageLength = 30
              ) 
    )  %>%
      formatStyle(2:ncol(x),
                  color = '#c8c8c8',
                  background = '#363b40', # background colour for app is '#363b40'
                  target = 'row') %>%
      formatStyle(2:ncol(x)-1,
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#34373b', '#5e8742', '#c96c28'))) %>% # red - b05353
      formatStyle(ncol(x):ncol(x),
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#454b51', '#70ad47', '#e87421'))) %>% # red - d45859
      formatStyle(2:ncol(x), `text-align` = 'center') %>%
      formatStyle(2:ncol(x), border = '1px solid #4d5154') %>%
      formatStyle(2:ncol(x), width='200px')
    #             
  })
  
  
  # Overview table ----
  
  output$overview_table <- renderDataTable({
  
    table <- all_data$Data %>%
      group_by(publication) %>% 
      summarise_all(last)
    
    datatable(table[,c(1,5:29)],
              selection = 'none',
              escape = F,
              class = list(stripe = FALSE),
              rownames = FALSE,
              options = list(
                dom = 't', # simple table output (add other letters for search, filter etc)
                headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}"), # removes header
                pageLength = 100
              )) %>% 
      formatStyle(1:ncol(table),
        color = '#c8c8c8',
        background = '#363b40', # background colour for app is '#363b40'
        target = 'row') %>%
      formatStyle(1:ncol(table),
                  backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                               c('#454b51', '#70ad47', '#e87421'))) %>% 
      formatStyle(2:ncol(table), `text-align` = 'center') %>%
      formatStyle(1:ncol(table), border = '1px solid #4d5154')
    
  })
  
  # Add latest publication progress form 
  
  observeEvent(input$Add_row_head, {
    
    DT <- all_data$Data %>% dplyr::filter(publication == input$publication_choice)
    
    showModal(modalDialog(title = NULL,#"Add a new row",
                          div(class = "row",
                              div(class = "col-sm-4","G6:"),
                              div(class = "col-sm-3", textInput("T2_add",label = NULL, value = t(DT)[2,ncol(t(DT))])),
                              div(class = "col-sm-5", "")),
                          div(class = "row",
                              div(class = "col-sm-4","G7:"),
                              div(class = "col-sm-3", textInput("T3_add",label = NULL, value = t(DT)[3,ncol(t(DT))])),
                              div(class = "col-sm-5", "")),
                          hr(),
                          rag_it(
                            "Is the publication published on Explore Education Statistics?",
                            "T5_add",
                            5,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/ees.html",
                              "How to publish on EES",
                              target = "_blank"
                            )
                          ),   #Publishing on EES
                          
                          rag_it(
                            "Is the maximum time series published?",
                            "T6_add",
                            6,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/ud.html#how-much-data-to-publish",
                              "How much data should be published",
                              target = "_blank"
                            )
                          ),   #Time series length
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
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#processing-is-done-with-code",
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
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#sensible-folder-and-file-structure",
                              "How to set up a sensible folder/file structure",
                              target = "_blank"
                            )
                          ), #Sensible folder /  file structure
                          rag_it(
                            "Are appropriate tools being used to create data files?",
                            "T9_add",
                            9,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/ud.html#appropriate-tools",
                              "What the appropriate tools look like",
                              target = "_blank"
                            )
                          ), #Use appropriate tools
                          rag_it(
                            "Is all source data stored in a single database?",
                            "T10_add",
                            10,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#all-source-data-stored-in-single-database",
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
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#documentation",
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
                              href = "https://rsconnect/rsc/stats-production-guidance/ud.html#how_to_check_against_the_standards",
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
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#basic-automated-qa",
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
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#recyclable-code-for-future-use",
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
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#single-production-scripts",
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
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#version-controlled-final-code-scripts",
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
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#automated-summaries",
                              "What generating automated summaries to provide insight looks like",
                              target = "_blank"
                            )
                          ), #Automated summaries generated for insight
                          rag_it(
                            "Has all code been peer reviewed within the team?",
                            "T18_add",
                            18,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#review-of-code-within-team",
                              "How to peer review code",
                              target = "_blank"
                            )
                          ), #Peer review of code within team
                          rag_it(
                            "Are there publication specific automated QA checks?",
                            "T19_add",
                            19,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#publication-specific-automated-qa",
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
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#collaboratively-develop-code-using-git",
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
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#publication-specific-automated-summaries",
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
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#single-production-scripts-with-integrated-qa",
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
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#single-publication-production-script",
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
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#clean-final-code",
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
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#review-of-code-from-outside-the-team",
                              "Where to go to request an external peer review",
                              target = "_blank"
                            )
                          ), #Peer review of code from outside the team
                          hr(),
                          strong("Continuous improvement"),
                          br(),
                          br(),
                          # Content checklist
                          rag_it(
                            "Was the content checklist used during the writing of the latest publication?",
                            "T26_add",
                            26,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/cd.html#content-checklist ",
                              "How to use the content checklist when writing a publication",
                              target = "_blank"
                            )
                          ),
                          # Content peer review
                          rag_it(
                            "Has the latest publication content been peer reviewed?",
                            "T27_add",
                            27,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/cd.html#peer-review",
                              "Where to go to request a content peer review",
                              target = "_blank"
                            )
                          ), 
                         # # Targetted user research activities
                          div(class = "row",
                              div(class = "col-sm-4","What targetted user research activites are taking place?"),
                              div(class = "col-sm-3", textInput("T28_add",label = NULL, value = t(DT)[28,ncol(t(DT))])),
                              div(class = "col-sm-5", a(href = "https://rsconnect/rsc/stats-production-guidance/pub.html#user-engagement","What targetted user research looks like",target = "_blank" ))),
                          # L&D
                          div(class = "row",
                              div(class = "col-sm-4","Any L&D requests or needs"),
                              div(class = "col-sm-3", textInput("T29_add",label = NULL, value = t(DT)[29,ncol(t(DT))])),
                              div(class = "col-sm-5", a(href = "https://rsconnect/rsc/stats-production-guidance/l+d.html","L&D resources",target = "_blank" ))),

                          actionButton("go", "Add item"),
                          
                          easyClose = TRUE, footer = NULL
                          , size = "l" 
                          ))
    
    
  })
  
  # Delete a column----
  
  observeEvent(input$Delete_row_head, {
    
    DT <- all_data$Data %>% dplyr::filter(publication == input$publication_choice)
    
    
    
    showModal(modalDialog(title = "Delete a column",#"Add a new row",
                          div(class = "row",
                              div(class = "col-sm-4","Select column to delete:"),
                              div(class = "col-sm-3", selectInput("col_choice",
                                                                  label = NULL,
                                                                  choices = sort(unique(DT$date)),
                                                                  width = "100%")),
                              div(class = "col-sm-5", "")),

                          div(class = "row",
                              div(class = "col-sm-4","Preview column to delete:"),
                              div(class = "col-sm-3", renderDataTable(datatable((DT %>% 
                                                                        filter(date == as_datetime(input$col_choice)) %>% 
                                                                        t()),
                                                                        rownames = TRUE,
                                                                        class = list(stripe = FALSE),
                                                                        selection = 'none',
                                                                        options = list(
                                                                          dom = 't', # simple table output (add other letters for search, filter etc)
                                                                          bSort=FALSE,
                                                                          pageLength = 30,
                                                                          headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}"), # removes header
                                                                          initComplete = JS(
                                                                            "function(settings, json) {",
                                                                            "$(this.api().table().header()).css({'color': '#c8c8c8'});",
                                                                            "}")
                                                                        ) 
                              ) %>% 
                                formatStyle(1:1,
                                            color = '#c8c8c8',
                                            background = '#363b40', # background colour for app is '#363b40'
                                            target = 'row') %>%
                                formatStyle(1:1,
                                            backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
                                                                         c('#454b51', '#70ad47', '#e87421'))) %>% 
                                #formatStyle(2:ncol(table), `text-align` = 'center') %>%
                                formatStyle(1:1, border = '1px solid #4d5154')
                                                                        
                                                                        )),
                              div(class = "col-sm-5", "")),
                          actionButton("go_delete", "Delete item"),
                          
                          easyClose = TRUE, footer = NULL
                          , size = "l" 
    ))
    
    
  })
  
  ## Pop-up to get confirmation
  observeEvent(input$go_delete,{
    confirmSweetAlert(
      session = session,
      inputId = "myconfirmation",
      type = "warning",
      title = "Are you sure you want to permanently delete this column?",
      btn_labels = c("No", "Yes")
    )
  })
  
  
  observeEvent(input$myconfirmation,{
    
    if (isTRUE(input$myconfirmation)){
    # Update SQL database
   #statement <- paste0("WITH CTE AS (SELECT *, ROW_NUMBER() OVER (ORDER BY date DESC) rn FROM ","publicationTracking", environment," WHERE publication = 'Test')",
    #                     " DELETE FROM CTE where rn = 6")
    # 
    statement <- paste0("DELETE FROM publicationTracking", environment," WHERE publication = '",input$publication_choice,"' and DATE = '", input$col_choice,"'")
    
    dbSendStatement(connection, statement)
    
    # Update the main data
    all_data$Data <- connection %>% tbl(paste0("publicationTracking", environment)) %>% collect()
    
    removeModal()

    shinyalert(title = "Deleted!", type = "success")
    
    }
    
  })
  
  # Adding new data to main data file ----
  
  observeEvent(input$go, {
    
    new_row <- data.frame(
      
      date = as.character(Sys.time()), 
      #ï..date = as.character(Sys.Date()), 
      g6 = str_replace_all(input[["T2_add"]],"'","''"),
      tl = str_replace_all(input[["T3_add"]],"'","''"),                                           
      publication = str_replace_all(input$publication_choice,"'","''"), 
      published_on_ees = input[["T5_add"]],
      time_series_length = input[["T6_add"]],
      processing_with_code = input[["T7_add"]],
      sensible_folder_file_structure = input[["T8_add"]],
      approporiate_tools = input[["T9_add"]], # Leaving this typo in as it is the column name in the database now (also referred to in line 594, the SQL query)             
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
      content_checklist = input[["T26_add"]],
      content_peer_review = input[["T27_add"]],
      targetted_user_research = str_replace_all(input[["T28_add"]],"'","''"),
      l_and_d_requests = str_replace_all(input[["T29_add"]],"'","''")
    )
    
    # Update SQL database
    statement <- paste0("INSERT INTO ", "publicationTracking", environment, 
                        " ([date], [g6], [tl], [publication], [published_on_ees], [time_series_length], [processing_with_code], [sensible_folder_file_structure], [approporiate_tools], [single_database], [documentation], [files_meet_data_standards], [basic_automated_qa], [recyclable_code], [single_data_production_scripts], [final_code_in_repo], [automated_insight_summaries], [peer_review_within_team], [publication_specifc_automated_qa], [collab_develop_using_git], [pub_specific_automated_insight_summaries], [single_data_production_scripts_with_qa], [single_publication_script], [clean_final_code], [peer_review_outside_team], [content_checklist], [content_peer_review], [targetted_user_research], [l_and_d_requests])
                        VALUES ('", paste0(as.vector(new_row), collapse = "', '"), "')")

    dbSendStatement(connection, statement)
  
    # Remove any test rows
    
    DT <- all_data$Data %>% dplyr::filter(publication == input$publication_choice)
    
    if(any(DT$date == "2019-09-28")) {
      clean_statement <- paste0("DELETE FROM publicationTracking", environment, " WHERE [publication] = '", str_replace_all(input$publication_choice,"'","''"), "' AND [date] = '2019-09-28';")
      dbSendStatement(connection, clean_statement)
    }

    # Update the main data
    all_data$Data <- connection %>% tbl(paste0("publicationTracking", environment)) %>% collect()
    
    removeModal()
  
    shinyalert(title = "Saved!", type = "success")
    
  })
   
  
  # Download data ----
  
  output$publication_data_csv<- downloadHandler(
    filename <- function() {
      paste("Publication data", Sys.Date(), ".csv", sep="")
    },
    content <- function(file) {
      write.csv(table_data(), file, row.names = F)
    }
  )
  
  output$all_data_csv<- downloadHandler(
    filename <- function() {
      paste("All data", Sys.Date(), ".csv", sep="")
    },
    content <- function(file) {
      write.csv(data.frame(all_data$Data), file, row.names = F)
    }
  )
  
  # Stop app ----
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}