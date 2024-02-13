server <- function(input, output, session){
  
  # Main dataset ----
  
  all_data <- reactiveValues()
  
  # Pulling from SQL ----
  
  all_data$Data <- connection %>% tbl(paste0("publicationTracking", environment)) %>% collect() %>% arrange(date) %>%
    mutate(date = as_datetime(date))
  
  observeEvent(all_data,updateSelectInput(session, "G6_dropdown", choices=c('All publications',unique(all_data$Data %>%
                                                                                                        filter(date < input$overviewDate) %>% 
                                                                                                        group_by(publication) %>% 
                                                                                                        arrange(date) %>% 
                                                                                                        summarise_all(last) %>%
                                                                                                        select(g6) %>%
                                                                                                        arrange(g6)))))
  
  table_data <- reactive({
    
    y <- all_data$Data %>%  mutate(date = as.Date(date)) %>% dplyr::filter(publication == input$publication_choice) %>% arrange(date) %>% t(.) %>% row_to_names(row_number = 1)
    
    x <- tibble::rownames_to_column(as.data.frame(y))
    x <- x %>%
      filter(rowname != 'single_data_production_scripts_with_qa')
    
    x$rowname <- case_when(
      x$rowname == "g6" ~ "Grade 6",
      x$rowname == "tl" ~ "Grade 7",
      x$rowname == "publication" ~ "Publication",
      x$rowname == "published_on_ees" ~ "Publication is published on EES",
      x$rowname == "time_series_length" ~ "Maximum time series published",
      x$rowname == "processing_with_code" ~ "Processing is done with code",
      x$rowname == "sensible_folder_file_structure" ~ "Sensible folder and file structure",
      x$rowname == "approporiate_tools" ~ "Use appropriate tools",
      x$rowname == "single_database" ~ "All source data stored in a database",
      x$rowname == "documentation" ~ "Documentation",
      x$rowname == "files_meet_data_standards" ~ "Files meet data standards",
      x$rowname == "basic_automated_qa" ~ "Basic automated QA",
      x$rowname == "recyclable_code" ~ "Recyclable code for future use",
      x$rowname == "single_data_production_scripts" ~ "Dataset production scripts",
      x$rowname == "final_code_in_repo" ~ "Version controlled final code scripts",
      x$rowname == "automated_insight_summaries" ~ "Automated summaries",
      x$rowname == "peer_review_within_team" ~ "Peer review of code within team",
      x$rowname == "publication_specifc_automated_qa" ~ "Publication specifc automated QA",
      x$rowname == "collab_develop_using_git" ~ "Collaboratively develop code using git",
      x$rowname == "pub_specific_automated_insight_summaries" ~ "Publication specific automated summaries",
     # x$rowname == "single_data_production_scripts_with_qa" ~ "Single production scripts with integrated QA",
      x$rowname == "single_publication_script" ~ "Whole publication production script",
      x$rowname == "clean_final_code" ~ "Clean final code",
      x$rowname == "open_source_repo" ~ "Open source repository",
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
      format_split_table(df=x)
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
      format_split_table(df=x) 
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
      format_split_table(df=x)             
  })
  
  output$main_pub_table4 <- renderDataTable({ 
    
    x <- table_data()
    
    datatable(x[c(19:22,28,23),],
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
      format_split_table(df=x)           
  })
  
  output$main_pub_table5 <- renderDataTable({ 
    
    x <- table_data()
    
    datatable(x[c(24,25,26,27),],
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
      format_split_table(df=x)             
  })
  

  # Weekly reporting tab ----
  
  output$wr_markdown <- renderUI({
    HTML(mark_html(knit('weekly_reporting/weekly-report.rmd', quiet = TRUE) , output = NULL, template = FALSE))
  })
  
  # Overview tab ----
  
  ## RAP level table ----
  
  rap_level_summary_data <- reactive({
    
    if(input$G6_dropdown == 'All publications'){
    all_data$Data[, c(1:21,23,24,30,25)] %>%
      filter(date < input$overviewDate) %>% 
      group_by(publication) %>% 
      arrange(date) %>% 
      summarise_all(last) %>% 
      pivot_longer(!c(publication, date, g6, tl),
                   names_to = "rap_level",
                   values_to = "done") %>% 
      group_by(rap_level) %>%  
      mutate(rap_practice = case_when( 
        rap_level %in% c("processing_with_code",
                         "sensible_folder_file_structure",
                         "approporiate_tools",
                         "single_database",
                         "documentation",
                         "files_meet_data_standards",
                         "basic_automated_qa") ~ "Good",
        rap_level %in% c("recyclable_code",
                         "single_data_production_scripts",
                         "final_code_in_repo",
                         "automated_insight_summaries",
                         "peer_review_within_team",
                         "publication_specifc_automated_qa") ~ "Great",
        rap_level %in% c("collab_develop_using_git",
                         "pub_specific_automated_insight_summaries",
                         #"single_data_production_scripts_with_qa",
                         "single_publication_script",
                         "clean_final_code",
                         "open_source_repo",
                         "peer_review_outside_team") ~ "Best",
        rap_level %in% c("published_on_ees",
                         "time_series_length") ~ "EES",
        TRUE ~ "Other")) %>%
      mutate(rap_practice = factor(rap_practice, levels=c('EES','Good','Great','Best'))) %>%
      mutate(rap_level_label = case_when(
        rap_level == "published_on_ees" ~ "Publication is published on EES",
        rap_level == "time_series_length" ~ "Maximum time series published",
        rap_level == "processing_with_code" ~ "Processing is done with code",
        rap_level == "sensible_folder_file_structure" ~ "Sensible folder and file structure",
        rap_level == "approporiate_tools" ~ "Use appropriate tools",
        rap_level == "single_database" ~ "All source data stored in a database",
        rap_level == "documentation" ~ "Documentation",
        rap_level == "files_meet_data_standards" ~ "Files meet data standards",
        rap_level == "basic_automated_qa" ~ "Basic automated QA",
        rap_level == "recyclable_code" ~ "Recyclable code for future use",
        rap_level == "single_data_production_scripts" ~ "Dataset production scripts",
        rap_level == "final_code_in_repo" ~ "Version controlled final code scripts",
        rap_level == "automated_insight_summaries" ~ "Automated summaries",
        rap_level == "peer_review_within_team" ~ "Peer review of code within team",
        rap_level == "publication_specifc_automated_qa" ~ "Publication specifc automated QA",
        rap_level == "collab_develop_using_git" ~ "Collaboratively develop code using git",
        rap_level == "pub_specific_automated_insight_summaries" ~ "Publication specific automated summaries",
        #rap_level == "single_data_production_scripts_with_qa" ~ "Single production scripts with integrated QA",
        rap_level == "single_publication_script" ~ "Whole publication production script",
        rap_level == "clean_final_code" ~ "Clean final code",
        rap_level == "open_source_repo" ~ "Open source repository",
        rap_level == "peer_review_outside_team" ~ "Peer review of code from outside the team"
      )) %>% 
      mutate(rap_level_label = factor(rap_level_label, levels = c(
        #EES
         "Maximum time series published","Publication is published on EES"
        #GOOD
        ,"Basic automated QA","Files meet data standards","Documentation","All source data stored in a database",
        "Use appropriate tools","Sensible folder and file structure","Processing is done with code"
        #GREAT
        ,"Publication specifc automated QA","Peer review of code within team", "Automated summaries",
        "Version controlled final code scripts","Dataset production scripts","Recyclable code for future use"
        #BEST
        ,"Peer review of code from outside the team", "Clean final code", "Open source repository", "Whole publication production script", 
        "Publication specific automated summaries", "Collaboratively develop code using git"
        
        
      ))) %>% 
      mutate(done = if_else(date==as_datetime("2019-09-28"), "No response", as.character(done)),
             done = factor(done, levels=c("No response","No","Working on it","Yes"))) 
    } else {
      all_data$Data[, c(1:21,23,24,30,25)] %>%
        filter(date < input$overviewDate) %>% 
        group_by(publication) %>% 
        arrange(date) %>% 
        summarise_all(last) %>%
        filter(g6 == input$G6_dropdown) %>%
        pivot_longer(!c(publication, date, g6, tl),
                     names_to = "rap_level",
                     values_to = "done") %>% 
        group_by(rap_level) %>%  
        mutate(rap_practice = case_when( 
          rap_level %in% c("processing_with_code",
                           "sensible_folder_file_structure",
                           "approporiate_tools",
                           "single_database",
                           "documentation",
                           "files_meet_data_standards",
                           "basic_automated_qa") ~ "Good",
          rap_level %in% c("recyclable_code",
                           "single_data_production_scripts",
                           "final_code_in_repo",
                           "automated_insight_summaries",
                           "peer_review_within_team",
                           "publication_specifc_automated_qa") ~ "Great",
          rap_level %in% c("collab_develop_using_git",
                           "pub_specific_automated_insight_summaries",
                           #"single_data_production_scripts_with_qa",
                           "single_publication_script",
                           "clean_final_code",
                           "open_source_repo",
                           "peer_review_outside_team") ~ "Best",
          rap_level %in% c("published_on_ees",
                           "time_series_length") ~ "EES",
          TRUE ~ "Other")) %>%
        mutate(rap_practice = factor(rap_practice, levels=c('EES','Good','Great','Best'))) %>%
        mutate(rap_level_label = case_when(
          rap_level == "published_on_ees" ~ "Publication is published on EES",
          rap_level == "time_series_length" ~ "Maximum time series published",
          rap_level == "processing_with_code" ~ "Processing is done with code",
          rap_level == "sensible_folder_file_structure" ~ "Sensible folder and file structure",
          rap_level == "approporiate_tools" ~ "Use appropriate tools",
          rap_level == "single_database" ~ "All source data stored in a database",
          rap_level == "documentation" ~ "Documentation",
          rap_level == "files_meet_data_standards" ~ "Files meet data standards",
          rap_level == "basic_automated_qa" ~ "Basic automated QA",
          rap_level == "recyclable_code" ~ "Recyclable code for future use",
          rap_level == "single_data_production_scripts" ~ "Dataset production scripts",
          rap_level == "final_code_in_repo" ~ "Version controlled final code scripts",
          rap_level == "automated_insight_summaries" ~ "Automated summaries",
          rap_level == "peer_review_within_team" ~ "Peer review of code within team",
          rap_level == "publication_specifc_automated_qa" ~ "Publication specifc automated QA",
          rap_level == "collab_develop_using_git" ~ "Collaboratively develop code using git",
          rap_level == "pub_specific_automated_insight_summaries" ~ "Publication specific automated summaries",
          #rap_level == "single_data_production_scripts_with_qa" ~ "Single production scripts with integrated QA",
          rap_level == "single_publication_script" ~ "Whole publication production script",
          rap_level == "clean_final_code" ~ "Clean final code",
          rap_level == "open_source_repo" ~ "Open source repository",
          rap_level == "peer_review_outside_team" ~ "Peer review of code from outside the team"
        )) %>% 
        mutate(rap_level_label = factor(rap_level_label, levels = c(
          #EES
          "Maximum time series published","Publication is published on EES"
          #GOOD
          ,"Basic automated QA","Files meet data standards","Documentation","All source data stored in a database",
          "Use appropriate tools","Sensible folder and file structure","Processing is done with code"
          #GREAT
          ,"Publication specifc automated QA","Peer review of code within team", "Automated summaries",
          "Version controlled final code scripts","Dataset production scripts","Recyclable code for future use"
          #BEST
          ,"Peer review of code from outside the team", "Clean final code", "Open source repository", "Whole publication production script", 
          "Single production scripts with integrated QA", "Publication specific automated summaries", "Collaboratively develop code using git"
          
          
        ))) %>% 
        mutate(done = if_else(date==as_datetime("2019-09-28"), "No response", as.character(done)),
               done = factor(done, levels=c("No response","No","Working on it","Yes")))
    }
    
  })

  
  ## Publication table ----
  
  output$overview_table <- renderDataTable({
    
    if(input$G6_dropdown == 'All publications'){
    table <- all_data$Data %>%
      filter(date < input$overviewDate) %>% 
      group_by(publication) %>% 
      arrange(date) %>% 
      summarise_all(last)
    } else {
      table <- all_data$Data %>%
        filter(date < input$overviewDate) %>% 
        group_by(publication) %>% 
        arrange(date) %>% 
        summarise_all(last) %>%
        filter(g6 == input$G6_dropdown)
    }
      
    table$date <- format(table$date, format = '%Y/%m/%d')
    
    #Create table container to populate
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan = 2, 'Publication'),
          th(colspan = 1, '% Good'),
          th(colspan = 1, '% Great'),
          th(colspan = 1, '% Best'),
          th(colspan = 2, 'EES checks'),
          th(colspan = 7, 'Good'),
          th(colspan = 6, 'Great'),
          th(colspan = 6, 'Best'),
          th(colspan = 4, 'Continuous improvement')
        ),
        tr(
          lapply(rep(c(''), 27), th)
        )
      )
    ))
    
    # Using JS for adding CSS, i.e., coloring your heading
    # Get the corresponding table header (th) from a table cell (td) and apply color to it
    headjs <- "function(thead, data, start, end, display) {
  $(thead).closest('thead').find('th').eq(0).css({'background-color': '#363b40', 'color': '#c8c8c8'}); 
   $(thead).closest('thead').find('th').eq(1).css({'background-color': '#363b40', 'color': '#c8c8c8'});
    $(thead).closest('thead').find('th').eq(2).css({'background-color': '#4472c4', 'color': '#c8c8c8'});
     $(thead).closest('thead').find('th').eq(3).css({'background-color': '#70ad47', 'color': '#c8c8c8'});
      $(thead).closest('thead').find('th').eq(4).css({'background-color': '#ec7d37', 'color': '#c8c8c8'});
       $(thead).closest('thead').find('th').eq(5).css({'background-color': '#363b40', 'color': '#c8c8c8'});
         $(thead).closest('thead').find('th').eq(6).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(7).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(8).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(9).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(10).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(11).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(12).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(13).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(14).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(15).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(16).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(17).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(18).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(19).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(20).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(21).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(22).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(23).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(24).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(25).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(26).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(27).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(28).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(29).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(30).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(31).css({'background-color': '#363b40'});
    
}" #this is really ugly! Must be a way to iterate this but works for now
    
    datatable(table[,c(1,2,5:21,23,24,30,25:29)],
              container = sketch,
              selection = 'none',
              escape = F,
              #class = "compact row-border",#list(stripe = FALSE),
              rownames = FALSE,
              colnames = NULL,
              options = list(
                dom = 't', # simple table output (add other letters for search, filter etc)
                scrollY = "600px",
                scrollX = TRUE,
                headerCallback = JS(headjs),
                #headerCallback = JS("function(thead, data, start, end, display){","  $(thead).css({'background-color': '#363b40'});","}"), # removes header
                #fixedHeader = TRUE,
                ordering=T,
                pageLength = 100
              )#,
              #extensions = "FixedHeader") 
    )%>% 
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
  
  ## Grade 6 table ----
  
  output$G6_overview_table <- renderDataTable({
    goodgreat_steps <- all_data$Data %>%
      filter(date < input$overviewDate) %>% 
      group_by(publication) %>%
      slice(which.max(date)) %>% 
      group_by(g6) %>% 
      ungroup() %>% 
      select(g6,
             processing_with_code,
             sensible_folder_file_structure,
             approporiate_tools,
             single_database,
             documentation,
             files_meet_data_standards,
             basic_automated_qa,
             recyclable_code,
             single_data_production_scripts,
             final_code_in_repo,
             automated_insight_summaries,
             peer_review_within_team,
             publication_specifc_automated_qa)
    
    test <- goodgreat_steps %>%
      group_by(g6) %>%
      dplyr::summarise(number_of_pubs=n(),
                       percent=scales::percent((sum(length(which(processing_with_code=='Yes')),
                                        length(which(sensible_folder_file_structure=='Yes')),
                                        length(which(approporiate_tools=='Yes')),
                                        length(which(single_database=='Yes')),
                                        length(which(documentation=='Yes')),
                                        length(which(files_meet_data_standards=='Yes')),
                                        length(which(basic_automated_qa=='Yes')),
                                        length(which(recyclable_code=='Yes')),
                                        length(which(single_data_production_scripts=='Yes')),
                                        length(which(final_code_in_repo=='Yes')),
                                        length(which(automated_insight_summaries=='Yes')),
                                        length(which(peer_review_within_team=='Yes')),
                                        length(which(publication_specifc_automated_qa=='Yes')))/(n()*(ncol(.)-1))), accuracy = 0.01))
    
#     #Create table container to populate
#     sketch = htmltools::withTags(table(
#       class = 'display',
#       thead(
#         tr(
#           th(colspan = 1, 'Grade 6'),
#           th(colspan = 3, 'EES checks'),
#           th(colspan = 3, 'Good'),
#           th(colspan = 3, 'Great'),
#           th(colspan = 3, 'Best')
#         ),
#         tr(th(colsan = 1, ''),
#           lapply(rep(c('Yes', 'Working on it', 'No'), 4), th)
#         )
#       )
#     ))
#     
    # Using JS for adding CSS, i.e., coloring your heading
    # Get the corresponding table header (th) from a table cell (td) and apply color to it
    headjs <- "function(thead, data, start, end, display) {
  $(thead).closest('thead').find('th').eq(0).css({'background-color': '#363b40', 'color': '#c8c8c8'});
   $(thead).closest('thead').find('th').eq(1).css({'background-color': '#363b40', 'color': '#c8c8c8'});
    $(thead).closest('thead').find('th').eq(2).css({'background-color': '#363b40', 'color': '#c8c8c8'});
     $(thead).closest('thead').find('th').eq(3).css({'background-color': '#363b40', 'color': '#c8c8c8'});
      $(thead).closest('thead').find('th').eq(4).css({'background-color': '#363b40', 'color': '#c8c8c8'});
       $(thead).closest('thead').find('th').eq(5).css({'background-color': '#363b40', 'color': '#c8c8c8'});
         $(thead).closest('thead').find('th').eq(6).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(7).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(8).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(9).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(10).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(11).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(12).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(13).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(14).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(15).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(16).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(17).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(18).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(19).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(20).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(21).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(22).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(23).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(24).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(25).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(26).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(27).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(28).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(29).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(30).css({'background-color': '#363b40'});
         $(thead).closest('thead').find('th').eq(31).css({'background-color': '#363b40'});

}" #this is really ugly! Must be a way to iterate this but works for now
    
    # Hacky code alert: styleEqual below doesn't deal well with ampersands, so
    # I'm adding a cleaned g6 name column with any ampersands removed (i.e. where 
    # there's a job-share). The extra clean_g6 column is hidden by datatable by 
    # the line:
    # columnDefs = list(list(visible=FALSE, targets=3))
    # Which assumes it's the 4th column of the table.
    test <- test %>% mutate(clean_g6 = gsub("&","-",g6))
    
    datatable(test,
              #container = sketch,
              selection = 'none',
              escape = F,
              #class = "compact row-border",#list(stripe = FALSE),
              rownames = FALSE,
              colnames = c("Grade 6", "Number of publications", "Proportion of Good & Great achieved"),
              options = list(
                dom = 't', # simple table output (add other letters for search, filter etc)
                #scrollY = "600px",
                #scrollX = TRUE,
                headerCallback = JS(headjs),
                headerCallback = JS("function(thead, data, start, end, display){","  $(thead).css({'background-color': '#363b40'});","}"), # removes header
                fixedHeader = TRUE,
                ordering=T,
                pageLength = 100,
                columnDefs = list(list(visible=FALSE, targets=3)) # Warning this is hiding the 4th column!
              )#,
              #extensions = "FixedHeader") 
     )%>% 
      formatStyle(1:ncol(test),
                  color = '#c8c8c8',
                  background = '#363b40', # background colour for app is '#363b40'
                  target = 'row') %>%
      formatStyle(1:ncol(test),
                  backgroundColor = styleEqual(c(100, gsub("&","-",input$G6_dropdown)),
                                               c('#70ad47', '#e87421')),
                  target = 'row') %>%
      formatStyle(2:ncol(test), `text-align` = 'center') %>%
      formatStyle(1:ncol(test), border = '1px solid #4d5154')

  })
  
  ## Summary stats ----
  
  output$summary_lines <- renderUI({
    
    if(input$G6_dropdown == 'All publications'){
      table <- all_data$Data %>%
        filter(date < input$overviewDate) %>% 
        group_by(publication) %>% 
        arrange(date) %>% 
        summarise_all(last)
    } else {
      table <- all_data$Data %>%
        filter(date < input$overviewDate) %>% 
        group_by(publication) %>% 
        arrange(date) %>% 
        summarise_all(last) %>%
        filter(g6 == input$G6_dropdown)
    }
    
    count_pubs <- table %>%  select(publication) %>% nrow()
    
    count_good <- table %>% filter(processing_with_code == "Yes",
                                   sensible_folder_file_structure =="Yes",
                                   approporiate_tools == "Yes",
                                   single_database == "Yes",
                                   documentation == "Yes",
                                   files_meet_data_standards == "Yes",
                                   basic_automated_qa =="Yes") %>% nrow()
    count_great <- table %>% filter(recyclable_code == "Yes",
                                    single_data_production_scripts =="Yes",
                                    final_code_in_repo == "Yes",
                                    automated_insight_summaries == "Yes",
                                    peer_review_within_team == "Yes",
                                    publication_specifc_automated_qa == "Yes") %>% nrow()
    count_good_great <- table %>% filter(processing_with_code == "Yes",
                                         sensible_folder_file_structure =="Yes",
                                         approporiate_tools == "Yes",
                                         single_database == "Yes",
                                         documentation == "Yes",
                                         files_meet_data_standards == "Yes",
                                         basic_automated_qa =="Yes",
                                         recyclable_code == "Yes",
                                    single_data_production_scripts =="Yes",
                                    final_code_in_repo == "Yes",
                                    automated_insight_summaries == "Yes",
                                    peer_review_within_team == "Yes",
                                    publication_specifc_automated_qa == "Yes") %>% nrow()
    count_best <- table %>% filter(collab_develop_using_git == "Yes",
                                   pub_specific_automated_insight_summaries =="Yes",
                                   #single_data_production_scripts_with_qa == "Yes",
                                   single_publication_script == "Yes",
                                   clean_final_code == "Yes",
                                   open_source_repo == "Yes",
                                   peer_review_outside_team == "Yes") %>% nrow()
    no_response <- table %>% 
      dplyr::mutate(date = as.character(date)) %>% 
      dplyr::filter(date == "2019-09-28 00:00:00") %>%
      dplyr::distinct(publication) %>% 
      nrow()
    
    HTML(paste0("<h4>So far, out of all ", count_pubs, " publications: </h4>","<br/> • <b>",
                no_response, "</b> publications have not completed a self-assessment.<br/> • <b>",
                count_good , "</b> publications are meeting all elements of ","<img src = 'good.svg'>","<br/> • <b>",
                count_great, "</b> publications are meeting all elements of ","<img src = 'great.svg'>","<br/> • <b>",
                count_good_great, "</b> publications are meeting all elements of both ","<img src = 'good.svg'>","&","<img src = 'great.svg'>","<br/> • <b>",
                count_best, "</b> publications are meeting all elements of ","<img src = 'best.svg'>"
    ))
    
  })
  
  output$summary_rap_practice <- renderTable({
    if (input$summary_choice == "Number") {
      
      rap_level_summary_data() %>%
        group_by(rap_practice) %>%
        dplyr::count(done) %>%pivot_wider(names_from = rap_practice, values_from = n) %>%
        filter(is.na(done) == FALSE)
      
    } else if (input$summary_choice == "Percentage") {
      
      rap_level_summary_data() %>%
        group_by(rap_practice) %>%
        dplyr::count(done) %>%
        dplyr::mutate(percent=paste0(as.numeric(round(n/sum(n)*100,0)),"%")) %>%
        select(!n) %>%
        pivot_wider(names_from = rap_practice, values_from = percent) %>%
        filter(is.na(done) == FALSE)
      
    }
  }, width = "90%")
  
  output$summary_rap_practice_G6 <- renderTable({
    if (input$summary_choice == "Number") {
      
      rap_level_summary_data() %>%
        filter(g6 == input$G6_dropdown) %>%
        group_by(rap_practice) %>%
        dplyr::count(done) %>% pivot_wider(names_from = rap_practice, values_from = n)
      
    } else if (input$summary_choice == "Percentage") {
      
      rap_level_summary_data() %>%
        filter(g6 == input$G6_dropdown) %>%
        group_by(rap_practice) %>%
        dplyr::count(done) %>%
        dplyr::mutate(percent=paste0(as.numeric(round(n/sum(n)*100,0)),"%")) %>%
        select(!n) %>%
        pivot_wider(names_from = rap_practice, values_from = percent)
      
    }
  }, width = "90%")
  
  output$steps_kpi <- renderText({
    
    if(input$G6_dropdown == 'All publications'){
    goodgreat_steps <- all_data$Data %>%
      filter(date < input$overviewDate) %>% 
      group_by(publication) %>% 
      slice(which.max(date)) %>% 
      ungroup() %>% 
      select(processing_with_code,
             sensible_folder_file_structure,
             approporiate_tools,
             single_database,
             documentation,
             files_meet_data_standards,
             basic_automated_qa,
             recyclable_code,
             single_data_production_scripts,
             final_code_in_repo,
             automated_insight_summaries,
             peer_review_within_team,
             publication_specifc_automated_qa) %>% 
      unlist()
    } else {
      goodgreat_steps <- all_data$Data %>%
        filter(date < input$overviewDate) %>% 
        group_by(publication) %>% 
        slice(which.max(date)) %>% 
        filter(g6 == input$G6_dropdown) %>%
        ungroup() %>% 
        select(processing_with_code,
               sensible_folder_file_structure,
               approporiate_tools,
               single_database,
               documentation,
               files_meet_data_standards,
               basic_automated_qa,
               recyclable_code,
               single_data_production_scripts,
               final_code_in_repo,
               automated_insight_summaries,
               peer_review_within_team,
               publication_specifc_automated_qa) %>% 
        unlist()
    }
    
    yes_steps <- length(goodgreat_steps[goodgreat_steps == "Yes"])
    
    overall_step_percentage <- round(100 * yes_steps / length(goodgreat_steps), 2)
    
    paste0(overall_step_percentage, "% of steps to good and great practice completed.")
    })
  
  output$summary_plot_level <- renderPlotly({
    
    plot_data <- rap_level_summary_data() %>%
      group_by(rap_level_label, rap_practice) %>%
      dplyr::count(done) %>%
      arrange(rap_practice) 
    plot_data <- ddply(plot_data,.(rap_level_label, rap_practice), transform, proportion = 100*round(n/sum(n), digits = 3))
    
    cols <- c("No response" = '#000000', "No" = '#454b51', "Working on it" = '#e87421', "Yes" = '#70ad47')
    
    plot <- plot_data %>%
      ggplot(aes(y=proportion, x=rap_level_label, fill = done, width = 0.85, text = paste('proportion: ',proportion,'%', sep = ''))) +
      geom_bar(stat = 'identity') +
      coord_flip() +
      scale_fill_manual('',values = cols) +
      #scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
      theme(plot.background = element_rect(fill = "#363b40", color = "#363b40"),
            plot.margin = unit(c(1, 1, 1, 4), "cm"),
            legend.background = element_rect(fill = "#363b40", color = "#363b40"),
            panel.background = element_rect(fill = "#363b40", color = "#363b40"),
            panel.grid = element_blank(),
            legend.key = element_rect(fill = NA),
            legend.title = element_text(size = 14, colour="#c8c8c8"),
            legend.text = element_text(size = 14, colour="#c8c8c8"),
            legend.position = "top",
            axis.text = element_text(size = 10, colour="#c8c8c8"),
            axis.text.x = element_blank(),
            strip.text = element_text(size = 14)
      ) +
      xlab("") +
      ylab("") +
      facet_grid(rap_practice~., scales = "free", space = "free")
    
    p <- ggplotly(plot, height = 600, width = 1000, tooltip = c('rap_level_label','done', 'text')) %>%
      plotly::config(displayModeBar = F) %>%
      layout(legend = list(orientation = 'h', x = 0.1, y = 1.1))
    
    #Force the facet bars to be the same size and customise the labels on the right hand side to match
    # There are 21 bars, the below sets out how many are in each facet and orces them to be the same size
    p$x$layout$yaxis4$domain <- c(0, 6/21)
    p$x$layout$yaxis3$domain <- c(6/21, 12/21)
    p$x$layout$yaxis2$domain <- c(12/21, 19/21)
    p$x$layout$yaxis$domain <- c(19/21, 1)
    #the below adjusts the ight-hand-side facet labels
    p$x$layout$annotations[[1]]$y <- 0.96
    p$x$layout$annotations[[2]]$y <- 0.75
    p$x$layout$annotations[[3]]$y <- 0.45
    p$x$layout$annotations[[4]]$y <- 0.15
    
    #the below resizes the grey boxes for the facet labels on the right-hand-side
    p$x$layout$shapes[[2]]$y0 <- 0.915
    
    p$x$layout$shapes[[4]]$y0 <- 0.58
    p$x$layout$shapes[[4]]$y1 <- 0.90
    
    p$x$layout$shapes[[6]]$y0 <- 0.29
    p$x$layout$shapes[[6]]$y1 <- 0.57
    
    p$x$layout$shapes[[8]]$y1 <- 0.28
   
    p
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
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#appropriate-tools",
                              "What the appropriate tools look like",
                              target = "_blank"
                            )
                          ), #Use appropriate tools
                          rag_it(
                            "Is all source data stored in a database?",
                            "T10_add",
                            10,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#all-source-data-stored-in-a-database",
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
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#dataset-production-scripts",
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
                            "Can the whole process (from creating data files to producing the publication) be reproduced via a single code script with integrated QA?",
                            "T23_add",
                            23,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#whole-publication-production-scripts",
                              "How to produce a publication using a single run script",
                              target = "_blank"
                            )
                          ), #Subject specific automated insights
                          # rag_it(
                          #   "Can the publication be reproduced using a single code script?",
                          #   "T23_add",
                          #   23,
                          #   DT,
                          #   a(
                          #     href = "https://rsconnect/rsc/stats-production-guidance/rap.html#single-publication-production-script",
                          #     "How to produce a publication using a single run script",
                          #     target = "_blank"
                          #   )
                          # ), #Single publication script
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
                            "Is your code publicly available in an open-source repository (if appropriate)?",
                            "T30_add",
                            30,
                            DT,
                            a(
                              href = "https://rsconnect/rsc/stats-production-guidance/rap.html#use-open-source-repositories",
                              "How and when we should use open-source repositories",
                              target = "_blank"
                            )
                          ), #Open-source repositories
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
                              href = "https://rsconnect/rsc/stats-production-guidance/resources/Content_Design_Logbook.xlsx",
                              "Download the content checklist",
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
                              href = "https://rsconnect/rsc/stats-production-guidance/cd.html#publication-scrums",
                              "Where to go to request a content peer review",
                              target = "_blank"
                            )
                          ), 
                          # # Targetted user research activities
                          div(class = "row",
                              div(class = "col-sm-4","What targetted user research activites are taking place?"),
                              div(class = "col-sm-3", textInput("T28_add",label = NULL, value = t(DT)[28,ncol(t(DT))])),
                              div(class = "col-sm-5", a(href = "https://rsconnect/rsc/stats-production-guidance/user_eng.html","What targetted user research looks like",target = "_blank" ))),
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
                                                                                  headerCallback = JS("function(thead, data, start, end, display){","  $(thead).css({'background-color': '#363b40'});","}"), # removes header
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
      statement <- paste0("DELETE FROM publicationTracking", environment," WHERE publication = '", str_replace_all(input$publication_choice,"'","''"),"' and [date] = CAST('", input$col_choice,"' AS DATETIME2)")
      
      rs <- dbSendStatement(connection, statement)
      dbHasCompleted(rs)
      dbClearResult(rs)
      
      if(nrow(all_data$Data %>% dplyr::filter(publication == input$publication_choice)) == 1){
        
        new_row <- data.frame(
          
          #date = "2019-09-28",
          g6 = "TBC",
          tl = "TBC",
          publication = str_replace_all(input$publication_choice,"'","''"),
          published_on_ees = "No",
          time_series_length = "No",
          processing_with_code = "No",
          sensible_folder_file_structure = "No",
          approporiate_tools = "No", # Leaving this typo in as it is the column name in the database now (also referred to in line 594, the SQL query, and in the KPIs in manualDB.R)
          single_database = "No",
          documentation = "No",
          files_meet_data_standards = "No",
          basic_automated_qa = "No",
          recyclable_code = "No",
          single_data_production_scripts = "No",
          final_code_in_repo = "No",
          automated_insight_summaries = "No",
          peer_review_within_team = "No",
          publication_specifc_automated_qa = "No",
          collab_develop_using_git = "No",
          pub_specific_automated_insight_summaries = "No",
          #single_data_production_scripts_with_qa = "No",
          single_publication_script = "No",
          clean_final_code = "No",
          open_source_repo = "No",
          peer_review_outside_team = "No",
          content_checklist = "No",
          content_peer_review = "No",
          targetted_user_research = "No",
          l_and_d_requests = "No"
        )
        
          statement <- paste0("INSERT INTO ", "publicationTracking", environment,
                              " ([date], [g6], [tl],[publication],[published_on_ees], [time_series_length], [processing_with_code], [sensible_folder_file_structure], [approporiate_tools], [single_database], [documentation], [files_meet_data_standards], [basic_automated_qa], [recyclable_code], [single_data_production_scripts], [final_code_in_repo], [automated_insight_summaries], [peer_review_within_team], [publication_specifc_automated_qa], [collab_develop_using_git], [pub_specific_automated_insight_summaries], [single_publication_script], [clean_final_code], [open_source_repo], [peer_review_outside_team], [content_checklist], [content_peer_review], [targetted_user_research], [l_and_d_requests])
                        VALUES (FORMAT(CURRENT_TIMESTAMP+1,  'yyyy-MM-dd hh:mm:ss tt'),'", paste0(as.vector(new_row), collapse = "', '"), "')")
          
        
        rs  <- dbSendStatement(connection, statement)
        dbHasCompleted(rs)
        dbClearResult(rs)
      }
      
      # Update the main data
      all_data$Data <- connection %>% tbl(paste0("publicationTracking", environment)) %>% collect()
      
      removeModal()
      
      sendSweetAlert(
        session = session,
        title = "Deleted!",
        type = "success"
      )
      
    }
    
  })
  
  # Adding new data to main data file ----
  
  observeEvent(input$go, {
    
    new_row <- data.frame(
      
      #date = as.character(Sys.time()), 
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
      #single_data_production_scripts_with_qa = input[["T22_add"]],
      single_publication_script = input[["T23_add"]],
      clean_final_code = input[["T24_add"]],
      open_source_repo = input[["T30_add"]],
      peer_review_outside_team = input[["T25_add"]],
      content_checklist = input[["T26_add"]],
      content_peer_review = input[["T27_add"]],
      targetted_user_research = str_replace_all(input[["T28_add"]],"'","''"),
      l_and_d_requests = str_replace_all(input[["T29_add"]],"'","''")
    )
    
    # Update SQL database
    statement <- paste0("INSERT INTO ", "publicationTracking", environment, 
                        " ([date], [g6], [tl], [publication], [published_on_ees], [time_series_length], [processing_with_code], [sensible_folder_file_structure], [approporiate_tools], [single_database], [documentation], [files_meet_data_standards], [basic_automated_qa], [recyclable_code], [single_data_production_scripts], [final_code_in_repo], [automated_insight_summaries], [peer_review_within_team], [publication_specifc_automated_qa], [collab_develop_using_git], [pub_specific_automated_insight_summaries], [single_publication_script], [clean_final_code], [open_source_repo], [peer_review_outside_team], [content_checklist], [content_peer_review], [targetted_user_research], [l_and_d_requests])
                        VALUES (FORMAT(CURRENT_TIMESTAMP,  'yyyy-MM-dd hh:mm:ss tt'),'", paste0(as.vector(new_row), collapse = "', '"), "')")
    
    statement2 <- paste0("INSERT INTO ", "publicationTracking", "Development", 
                        " ([date], [g6], [tl], [publication], [published_on_ees], [time_series_length], [processing_with_code], [sensible_folder_file_structure], [approporiate_tools], [single_database], [documentation], [files_meet_data_standards], [basic_automated_qa], [recyclable_code], [single_data_production_scripts], [final_code_in_repo], [automated_insight_summaries], [peer_review_within_team], [publication_specifc_automated_qa], [collab_develop_using_git], [pub_specific_automated_insight_summaries], [single_publication_script], [clean_final_code], [open_source_repo], [peer_review_outside_team], [content_checklist], [content_peer_review], [targetted_user_research], [l_and_d_requests])
                        VALUES (FORMAT(CURRENT_TIMESTAMP,  'yyyy-MM-dd hh:mm:ss tt'),'", paste0(as.vector(new_row), collapse = "', '"), "')")
    
    statement3 <- paste0("INSERT INTO ", "publicationTracking", "PreProduction", 
                         " ([date], [g6], [tl], [publication], [published_on_ees], [time_series_length], [processing_with_code], [sensible_folder_file_structure], [approporiate_tools], [single_database], [documentation], [files_meet_data_standards], [basic_automated_qa], [recyclable_code], [single_data_production_scripts], [final_code_in_repo], [automated_insight_summaries], [peer_review_within_team], [publication_specifc_automated_qa], [collab_develop_using_git], [pub_specific_automated_insight_summaries], [single_publication_script], [clean_final_code], [open_source_repo], [peer_review_outside_team], [content_checklist], [content_peer_review], [targetted_user_research], [l_and_d_requests])
                        VALUES (FORMAT(CURRENT_TIMESTAMP,  'yyyy-MM-dd hh:mm:ss tt'),'", paste0(as.vector(new_row), collapse = "', '"), "')")
    
    rs <- dbSendStatement(connection, statement)
    dbHasCompleted(rs)
    dbClearResult(rs)
    #rs <- dbSendStatement(connection, statement2)
    #dbHasCompleted(rs)
    #dbClearResult(rs)
    #rs <- dbSendStatement(connection, statement3)      
    #dbHasCompleted(rs)
    #dbClearResult(rs)
    
    # Remove any test rows
    
    DT <- all_data$Data %>% dplyr::filter(publication == input$publication_choice)
    
    if(any(DT$date == "2019-09-28")) {
      clean_statement <- paste0("DELETE FROM publicationTracking", environment, " WHERE [publication] = '", str_replace_all(input$publication_choice,"'","''"), "' AND [date] = '2019-09-28';")
      rs <- dbSendStatement(connection, clean_statement)
      dbHasCompleted(rs)
      dbClearResult(rs)
    }
    
    # Update the main data
    all_data$Data <- connection %>% tbl(paste0("publicationTracking", environment)) %>% collect()
    
    removeModal()
    
    sendSweetAlert(
      session = session,
      title = "Saved!",
      type = "success"
    )
    
  })
  
  # Adding a new publication  ----
  observeEvent (input$Add_publication_head,{
    
    showModal(modalDialog(title = "Add a new publication",#"Add a new row",
                          div(class = "row",
                              div(class = "col-sm-4","Enter publication name:"),
                              div(class = "col-sm-3", textInput("New_publication_add",label = NULL)),
                              div(class = "col-sm-5", "")),
                          
                          div(class = "col-sm-5", ""),
                          actionButton("add_publication_modal", "Add publication"),
                          
                          easyClose = TRUE, footer = NULL
                          , size = "l"
    ))
  })
  
  observeEvent (input$add_publication_modal,{
    new_row <- data.frame(
      
      #date = "2019-09-28",
      g6 = "TBC",
      tl = "TBC",
      publication = str_replace_all(input$New_publication_add,"'","''"),
      published_on_ees = "No",
      time_series_length = "No",
      processing_with_code = "No",
      sensible_folder_file_structure = "No",
      approporiate_tools = "No", # Leaving this typo in as it is the column name in the database now (also referred to in line 594, the SQL query, and in the KPIs in manualDB.R)
      single_database = "No",
      documentation = "No",
      files_meet_data_standards = "No",
      basic_automated_qa = "No",
      recyclable_code = "No",
      single_data_production_scripts = "No",
      final_code_in_repo = "No",
      automated_insight_summaries = "No",
      peer_review_within_team = "No",
      publication_specifc_automated_qa = "No",
      collab_develop_using_git = "No",
      pub_specific_automated_insight_summaries = "No",
      #single_data_production_scripts_with_qa = "No",
      single_publication_script = "No",
      clean_final_code = "No",
      open_source_repo = "No",
      peer_review_outside_team = "No",
      content_checklist = "No",
      content_peer_review = "No",
      targetted_user_research = "No",
      l_and_d_requests = "No"
    )
    
    # Update SQL database
    statement <- paste0("INSERT INTO ", "publicationTracking", environment,
                        " ([date], [g6], [tl],[publication],[published_on_ees], [time_series_length], [processing_with_code], [sensible_folder_file_structure], [approporiate_tools], [single_database], [documentation], [files_meet_data_standards], [basic_automated_qa], [recyclable_code], [single_data_production_scripts], [final_code_in_repo], [automated_insight_summaries], [peer_review_within_team], [publication_specifc_automated_qa], [collab_develop_using_git], [pub_specific_automated_insight_summaries], [single_publication_script], [clean_final_code], [open_source_repo], [peer_review_outside_team], [content_checklist], [content_peer_review], [targetted_user_research], [l_and_d_requests])
                        VALUES (FORMAT(CURRENT_TIMESTAMP+1,  'yyyy-MM-dd hh:mm:ss tt'),'", paste0(as.vector(new_row), collapse = "', '"), "')")
    
    rs <- dbSendStatement(connection, statement)
    dbHasCompleted(rs)
    dbClearResult(rs)
    
    # Update the main data
    all_data$Data <- connection %>% tbl(paste0("publicationTracking", environment)) %>% collect()
    
    updated_data <- all_data$Data  %>%  rbind(new_row %>% mutate(publication = str_replace_all(publication,"''","'")))
    
    updateSelectInput(session,"publication_choice",
                      choices = sort(unique(updated_data$publication)))
    
    removeModal()
    
    # send confirmation message
    sendSweetAlert(
      session = session,
      title = "Saved!",
      type = "success"
    )
    
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