
#library(DBI)
#library(dbplyr)
#library(stringr)
#library(config)
#library(odbc)
#library(readr)
#library(tidyverse)
#library(kableExtra)
#library(knitr)


# today's date
today <- Sys.Date()

# get data directly from SQL to avoid manual updates
config <- config::get("db_connection")

connection <- dbConnect(odbc::odbc(),
                        Driver = "SQL Server Native Client 11.0",
                        Server = "T1PRANMSQL\\SQLPROD,60125",
                        Database = "MA_SDT_NS_DATA",
                        Trusted_Connection = "yes")

self_assessment_data <- dplyr::tbl(connection, dbplyr::in_schema("dbo","publicationTrackingProduction")) %>%
  dplyr::collect()

# join on staff info
# not ideal as was downloaded from analytical resource tool so could go out of date, can we read from it directly maybe? 
org <- readr::read_csv("organogram-data.csv")

tidy_org <- org %>%
  select("display_name", "division", "directorate")

self_assessment_org <- self_assessment_data %>%
  left_join(tidy_org, by = c('g6' = 'display_name'))

# keep only most recent score
self_assessment_latest <- self_assessment_org %>%
  arrange(publication, desc(date)) %>%
  distinct(publication, .keep_all=T) %>%
  # Remove Laura's row as it was a test entry
  filter(!grepl("Laura Selby", g6))

# keep only NS and OS publications
# there's a publication with a blank name - what is it?
self_assessment_NS_OS <- self_assessment_latest %>%
  filter(designation == "NS" | designation == "OS") 

# define scores and %s per level by publication
# good: 7 principles
# great: 6 principles
# best: 6 principles
self_assessment_levels <- self_assessment_NS_OS %>%
  rowwise() %>% 
   mutate(good = sum(na.omit(c_across(processing_with_code:basic_automated_qa)) == "Yes")) %>%
   mutate(great = sum(na.omit(c_across(recyclable_code:publication_specifc_automated_qa)) == "Yes")) %>%
   mutate(best = sum(na.omit(c_across(collab_develop_using_git:peer_review_outside_team)) == "Yes")) %>%
   mutate(good_per = round((good/7)*100, 2)) %>%
   mutate(great_per = round((great/6)*100, 2)) %>%
   mutate(best_per = round((best/6)*100, 2)) 

### outputs required ###

## number of NS and OS publications
no_of_publications <- self_assessment_levels %>% 
  nrow()

# number of publications meeting good and great
good_and_great <- self_assessment_levels %>%
  filter(good_per == 100 & great_per == 100) %>%
  nrow()

# % of all good and great levels met
good_and_great_per <- self_assessment_levels %>%
  select(processing_with_code:publication_specifc_automated_qa) %>% 
  unlist()

yes_steps <- length(good_and_great_per[good_and_great_per == "Yes"])

overall_step_percentage <- round(100 * yes_steps / length(good_and_great_per), 2)

# number of completed development plans
completed_dev_plans <- 53
    
# number of development plans with agreed extension
extended_dev_plans <- 0

# number of development plans completed and sent to G6s
completed_dev_plans_g6 <- 0

# number of development plans outstanding
outstanding_dev_plans <- 1

# number of good and great reviews completed
good_great_rev_done <- 0

# number of good and great reviews outstanding
good_great_rev_left <- (good_and_great - good_great_rev_done)

rap_summary_table <- tibble(no_of_publications, good_and_great, overall_step_percentage, extended_dev_plans, completed_dev_plans_g6, outstanding_dev_plans, good_great_rev_done, good_great_rev_left) 
  

## G6 table
# G6 name
# number of publications
# % completion of good and great
g6_table <- self_assessment_levels %>%
  select(g6, tl, publication, good, good_per, great, great_per) %>%
  arrange(., g6)

g6_levels <- g6_table %>%
  group_by(g6) %>%
  mutate(good_total = sum(good)) %>%
  mutate(great_total = sum(great)) %>%
  mutate(pub_count = length(g6)) %>%
  # 7 because good has 7 elements
  mutate(total_possible_good = 7*pub_count) %>%
  mutate(total_possible_great = 6*pub_count) %>%
  mutate(good_per_by_g6 = round(((good_total/total_possible_good)*100),2)) %>%
  mutate(great_per_by_g6 = round(((great_total/total_possible_great)*100),2)) %>%
  mutate(good_and_great_by_g6 = round(((good_total+great_total)/(total_possible_good+total_possible_great)*100), 2)) %>%
  mutate(good_and_great_per = round(((good+great)/(7+6)*100), 2))

g6_output <- g6_levels %>%
  select(g6, pub_count, good_per_by_g6, great_per_by_g6, good_and_great_by_g6) %>%
  distinct()
  
g6_table <- g6_levels %>%
  mutate(easier_names = gsub(" ", "", g6))

g6_split <- g6_table %>%
  group_by(easier_names) %>%
  split(g6_table$easier_names) %>%
  list2env(g6_nested, envir = globalenv())


## Division table
# Some divisions missing
division_table <- self_assessment_levels %>%
  select(date, division, tl, publication, good, good_per, great, great_per) %>%
  arrange(., division)

division_levels <- division_table %>%
  group_by(division) %>%
  mutate(good_total = sum(good)) %>%
  mutate(great_total = sum(great)) %>%
  mutate(pub_count = length(division)) %>%
  mutate(total_possible_good = 7*pub_count) %>%
  mutate(total_possible_great = 6*pub_count) %>%
  mutate(good_per_by_division = round(((good_total/total_possible_good)*100),2)) %>%
  mutate(great_per_by_division = round(((great_total/total_possible_great)*100),2)) %>%
  mutate(good_and_great_by_division = round(((good_total+great_total)/(total_possible_good+total_possible_great)*100), 2))




