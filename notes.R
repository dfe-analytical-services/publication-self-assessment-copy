# date
# g6
# tl
# publication
# published_on_ees
# time_series_length
# processing_with_code
# sensible_folder_file_structure
# approporiate_tools
# single_database
# documentation
# files_meet_data_standards
# basic_automated_qa
# recyclable_code
# single_data_production_scripts
# final_code_in_repo
# automated_insight_summaries
# peer_review_within_team
# publication_specifc_automated_qa
# collab_develop_using_git
# pub_specific_automated_insight_summaries
# single_data_production_scripts_with_qa
# single_publication_script
# clean_final_code
# peer_review_outside_team
# content_peer_review
# targetted_user_research
# l_and_d_requests
# 
# 
# Sys.setenv(no_proxy="*")
# 
# gs4_deauth() 
# 
# data <- read_sheet("https://docs.google.com/spreadsheets/d/1Fjr43xmPaXnL05INdbF8EuTt2G2sNl1TynjCS7--q1E/edit#gid=0")
# 
# sheet_write(data, ss = "https://docs.google.com/spreadsheets/d/1Fjr43xmPaXnL05INdbF8EuTt2G2sNl1TynjCS7--q1E/edit#gid=0")
# 
# 
# # making new rds file based on csv
# 
# start_data <- as.data.frame(read.csv("csv-data.csv", fileEncoding = 'UTF-8-BOM'))
# 
# 
# 
# saveRDS(start_data, "new_tracker_data.rds")
# 
# 
# 
# start_data %>%
#   group_by(Publication) %>% 
#   filter(row_number()==1)
# 
# 
# 
# start_data %>% 
#   group_by(Publication) %>% 
#   filter(row_number()==1)
# 
# 
#   start_data %>% 
#   group_by(Publication) %>% 
#   slice(1)
# 
# 
#   start_data %>% 
#   group_by(Publication) %>% 
#   top_n(n = -100)
# 
#   start_data %>%
#     group_by(Publication) %>%
#     last_row()
# #    summarise(first=head(value,1))#, count=n_distinct(value))
# 
#   
#   start_data %>%
#     #arrange(Publication, i..Date) %>% 
#     group_by(Publication ) %>% 
#     summarise_all(last)
# 
# # testing table output 
#   
#   #dataframe <- all_data$Data %>% dplyr::filter(publication == input$publication_choice)
#   
#   install.packages('janitor')
#   library(janitor)
#   
#   
#   
#   dataframe <- start_data
#   
#   x <- data.frame(t(dataframe))
#   
#   x <- x %>% row_to_names(row_number = 1)
#   
#   #names(x) <- x[1,]
#   
#   datatable(data.frame(x[5:28,]),
#             selection = 'single',
#             escape = F,
#             class = list(stripe = FALSE),
#             
#             # extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
#             
#             options = list(
#               dom = 't', # simple table output (add other letters for search, filter etc)
#               #headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}"), # removes header
#               
#               colnames = FALSE,
#               
#               # Fix width of columns
#               scrollX = TRUE,
#               autoWidth = TRUE,
#               columnDefs = list(list(width = '200px', targets = "_all")),
#               
#               # fixedHeader = TRUE,
#               #fixedColumns = list(leftColumns = 1),
#               
#               
#               pageLength = 30
#             )
#             
#             
#             
#             
#   ) %>% 
#     formatStyle(' ', #rownames col (replace with V1, V2 etc for others)
#                 backgroundColor = '#363b40')  %>% 
#     formatStyle(1:ncol(x), 
#                 color = '#c8c8c8',
#                 background = '#363b40', # background colour for app is '#363b40'
#                 target = 'row') %>%
#     formatStyle(1:ncol(x)-1,
#                 backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
#                                              c('#34373b', '#5e8742', '#c96c28'))) %>% # red - b05353
#     formatStyle(ncol(x):ncol(x),
#                 backgroundColor = styleEqual(c('No', 'Yes', 'Working on it'),
#                                              c('#454b51', '#70ad47', '#e87421'))) %>% # red - d45859
#     formatStyle(1:ncol(x), `text-align` = 'center') %>%
#     formatStyle(1:ncol(x), border = '1px solid #4d5154') %>% 
#     formatStyle(1:ncol(x), width='200px')
#   