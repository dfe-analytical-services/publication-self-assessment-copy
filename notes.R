date
g6
tl
publication
published_on_ees
time_series_length
processing_with_code
sensible_folder_file_structure
approporiate_tools
single_database
documentation
files_meet_data_standards
basic_automated_qa
recyclable_code
single_data_production_scripts
final_code_in_repo
automated_insight_summaries
peer_review_within_team
publication_specifc_automated_qa
collab_develop_using_git
pub_specific_automated_insight_summaries
single_data_production_scripts_with_qa
single_publication_script
clean_final_code
peer_review_outside_team
content_peer_review
targetted_user_research
l_and_d_requests







# making new rds file based on csv

new_data <- as.data.frame(read.csv("csv-data.csv"))

saveRDS(new_data, "tracker_data.rds")



start_data %>%
  group_by(Publication) %>% 
  filter(row_number()==1)



start_data %>% 
  group_by(Publication) %>% 
  filter(row_number()==1)


  start_data %>% 
  group_by(Publication) %>% 
  slice(1)


  start_data %>% 
  group_by(Publication) %>% 
  top_n(n = -100)

  start_data %>%
    group_by(Publication) %>%
    last_row()
#    summarise(first=head(value,1))#, count=n_distinct(value))

  
  start_data %>%
    #arrange(Publication, i..Date) %>% 
    group_by(Publication ) %>% 
    summarise_all(last)

# testing table output 

datatable(t(start_data),selection = 'single',
          escape=F,
          class = list(stripe = FALSE),
          options = list(dom = 't', # simple table output (add other letters for search, filter etc)
                         headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}") # removes header
          )
) %>% formatStyle(
  ' ', #rownames col (replace with V1, V2 etc for others)
  backgroundColor = 'lightgray',
  fontWeight = 'bold'
) %>%
  formatStyle(1:20,
            backgroundColor = styleEqual(c('R', 'G'), 
                                         c('red', 'green')
            )
)



