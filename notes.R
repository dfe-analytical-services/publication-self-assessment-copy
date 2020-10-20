install.packages('RODBC')
install.packages('odbc')

Sys.setenv(no_proxy="*")



create_sql_table <- function(query, queries_folder) {
  
  sqlString <-
    readLines(paste(queries_folder, "/", query, ".sql", sep = "")) %>%
    paste(collapse = "\n")
  
  myconn <- odbcDriverConnect(
    'driver={SQL Server};server=T1PRMDRSQL\\SQLPROD,55842;database=MDR_Modelling_DSAG_SU_ABS_EXC;trusted_connection=TRUE'
  )
  
  
  
  
  sqlQuery(myconn, sqlString) %>%
    as.data.frame() %>%
    tbl_df
  
  return(paste(query, " sql table has been created"))
  
  rm(myconn, sqlString)
}


# rownames(x)[rownames(x)== "published_on_ees"] <- "Publication is published on EES"
# rownames(x)[rownames(x)== "time_series_length"] <- "Average time series length within publication"
# rownames(x)[rownames(x)== "processing_with_code"] <- "Processing is done with code"
# rownames(x)[rownames(x)== "sensible_folder_file_structure"] <- "Sensible folder / file structure"
# rownames(x)[rownames(x)== "approporiate_tools"] <- "Use approporiate tools"
# rownames(x)[rownames(x)== "single_database"] <- "All source data stored in single database"
# rownames(x)[rownames(x)== "documentation"] <- "Documentation"
# rownames(x)[rownames(x)== "files_meet_data_standards"] <- "Files meet data standards"
# rownames(x)[rownames(x)== "basic_automated_qa"] <- "Basic automated QA"
# rownames(x)[rownames(x)== "recyclable_code"] <- "Recyclable code for future use"
# rownames(x)[rownames(x)== "single_data_production_scripts"] <- "Single production script(s)"
# rownames(x)[rownames(x)== "final_code_in_repo"] <- "Version controlled final versions of code"
# rownames(x)[rownames(x)== "automated_insight_summaries"] <- "Automated summaries generated for insight"
# rownames(x)[rownames(x)== "peer_review_within_team"] <- "Peer review of code within team"
# rownames(x)[rownames(x)== "publication_specifc_automated_qa"] <- "Publication specifc automated QA"
# rownames(x)[rownames(x)== "collab_develop_using_git"] <- "Collab and develop code using git"
# rownames(x)[rownames(x)== "pub_specific_automated_insight_summaries"] <- "Subject specific automated insights"
# rownames(x)[rownames(x)== "single_data_production_scripts_with_qa"] <- "Single production script(s) with integrated QA"
# rownames(x)[rownames(x)== "single_publication_script"] <- "Single production script for publication"
# rownames(x)[rownames(x)== "clean_final_code"] <- "Clean final code"
# rownames(x)[rownames(x)== "peer_review_outside_team"] <- "Peer review of code from outside the team"
# rownames(x)[rownames(x)== "content_peer_review"] <- "content_peer_review"
# rownames(x)[rownames(x)== "targetted_user_research"] <- "targetted_user_research"
# rownames(x)[rownames(x)== "l_and_d_requests"] <- "l_and_d_requests"



























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

saveRDS(new_data, "new_tracker_data.rds")



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



