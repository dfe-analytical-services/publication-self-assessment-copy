# Script with code for manually updating the database

# Library calls ====

library(odbc)
library(DBI)
library(dplyr)
library(config)
library(stringr)
library(data.table)
library(rvest)
library(dbplyr)

# DB connection ====

config <- config::get("db_connection")

con <- DBI::dbConnect(odbc::odbc(),
  Driver = config$driver,
  Server = config$server,
  Database = config$database,
  UID = config$uid,
  PWD = config$pwd,
  Trusted_Connection = config$trusted
)

# KPIs ======================================================================================================================================

# Publication responses

nonResponders <- prodData %>%
  dplyr::mutate(date = as.character(date)) %>% 
  dplyr::filter(date == "2019-09-28 00:00:00") %>% # View()
  dplyr::filter(!publication %in% pubsToRemove$publication) %>% 
  dplyr::distinct(publication)

message("Out of ", totalPubs, " publications, ", nrow(nonResponders), " are yet to respond. These are: ")
print(nonResponders)

# Below here is a mess of things, I (Cam) will review and tidy to make more useful...

stop("Do not source or highlight all of this script. Run lines individually.")

prodData <- dplyr::tbl(con, dbplyr::in_schema("dbo","publicationTrackingProduction")) %>%
  dplyr::collect()

updated_master <- read.csv("08112021_masterList.csv")

upMasterPubOnly <- updated_master %>% 
  filter(rap == "Yes") %>% 
  distinct(publication)

upMasterPubRemove <- updated_master %>% 
  filter(rap == "No") %>% 
  distinct(publication)


pubsToRemove <- prodData %>% 
  dplyr::distinct(publication) %>% 
  dplyr::filter(publication %in% upMasterPubRemove$publication)

pubsToRemove <- rbind(pubsToRemove, "Multi-academy trust performance measures at key stage 2")
  

totalPubs <- prodData %>% 
  dplyr::distinct(publication) %>% 
  #dplyr::filter(!publication %in% extraneousPublications$publication) %>% 
  dplyr::filter(!publication %in% pubsToRemove$publication) %>% 
  nrow()

# On EES

onEESinApp <- prodData %>% 
  dplyr::filter(published_on_ees == "Yes") %>% 
  dplyr::distinct(publication) # This is usually out of date

onFindStats <- rvest::read_html("https://explore-education-statistics.service.gov.uk/find-statistics") %>% 
  rvest::html_nodes(xpath = "//li[@class='govuk-!-margin-bottom-0']") %>%
  rvest::html_text()

onEESactually <- onFindStats[onFindStats %>% stringr::str_detect(".*Create your own.*")] %>% 
  stringr::str_extract(".*View statistics and data") %>% 
  stringr::str_remove("View statistics and data")

# Good and great practice
  
goodGreatPubs <- prodData %>% 
  group_by(publication) %>%
  filter(date < "2021-10-31") %>% 
  slice(which.max(date)) %>% 
  ungroup() %>% 
  dplyr::filter(
    processing_with_code == "Yes",
    sensible_folder_file_structure == "Yes",
    approporiate_tools == "Yes",
    single_database == "Yes",
    documentation == "Yes",
    files_meet_data_standards == "Yes",
    basic_automated_qa == "Yes",
    recyclable_code == "Yes",
    single_data_production_scripts == "Yes",
    final_code_in_repo == "Yes",
    automated_insight_summaries == "Yes",
    peer_review_within_team == "Yes",
    publication_specifc_automated_qa == "Yes"
    ) %>% 
  dplyr::filter(!publication %in% pubsToRemove$publication) %>% 
  dplyr::distinct(publication)


goodPubs <- prodData %>% 
  group_by(publication) %>%
  slice(which.max(date)) %>% 
  ungroup() %>% 
  dplyr::filter(
    processing_with_code == "Yes",
    sensible_folder_file_structure == "Yes",
    approporiate_tools == "Yes",
    single_database == "Yes",
    documentation == "Yes",
    files_meet_data_standards == "Yes",
    basic_automated_qa == "Yes"
  ) %>% 
  dplyr::filter(!publication %in% extraneousPublications) %>% 
  dplyr::distinct(publication)

# Overall percentage

goodgreat_steps <- prodData %>% 
  group_by(publication) %>%
  slice(which.max(date)) %>% 
  ungroup() %>% 
  dplyr::filter(!publication %in% pubsToRemove$publication) %>% 
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

yes_steps <- length(goodgreat_steps[goodgreat_steps == "Yes"])
  
overall_percentage <- round(100 * yes_steps / length(goodgreat_steps), 2)

# Overall percentage

goodgreat_steps <- prodData %>% 
  group_by(publication) %>%
  filter(date < "2021-09-30") %>% 
  slice(which.max(date)) %>% 
  ungroup() %>% 
  dplyr::filter(!publication %in% pubsToRemove$publication) %>% 
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

yes_steps <- length(goodgreat_steps[goodgreat_steps == "Yes"])

overall_percentage <- round(100 * yes_steps / length(goodgreat_steps), 2)

# CSSU KPIs -------------------------------------------------------------------------------------------------------------------------------

# overall % to target

message("Progress: ", overall_percentage, "% towards great practice as a whole.")

# On EES
message("Progress: ", length(onEESactually) - 1, " out of ", totalPubs, " publications are on EES.") # Using -1 for now as there's one supplementary publication that isn't really a publication - though we could include for ease?

# At great practice
message("Progress: ", nrow(goodGreatPubs), " out of ", totalPubs, " publications are meeting all of good and great practice.")

# Filled in app at least once
message("Progress: ", totalPubs - nrow(nonResponders), " out of ", totalPubs, " publications have filled in the app at least once.")

# QA list ==================================================================================================================================

stop("Do not source or highlight all of this script. Run lines individually.")

# Rudimentary QA of the publications in the SA app compared to the master list

masterPublicationOnly <- dplyr::tbl(con, dbplyr::in_schema("dbo","publicationMasterList")) %>% 
  dplyr::collect() %>% # write.csv("08112021_masterList.csv", row.names = FALSE)
  dplyr::filter(where == "Find stats") %>%
  dplyr::distinct(publication)

selfAssPublicationOnly <- dplyr::tbl(con, dbplyr::in_schema("dbo", "publicationTrackingProduction")) %>%
  dplyr::collect() %>%
  dplyr::distinct(publication)

missingPublications <- setdiff(masterPublicationOnly, selfAssPublicationOnly)
extraneousPublications <- setdiff(selfAssPublicationOnly, masterPublicationOnly)

if(length(missingPublications) == 0 && length(extraneousPublications) == 0){
  message("Self-assessment app is up to date with the master list. Celebrate!")
} else if (length(missingPublications) != 0 && length(extraneousPublications) != 0){
  message("Self-assessment app has both extraneous and missing publications when compared to the master list. Fix it.")
  message("Missing publications are:")
  print(missingPublications)
  message("Extraneous publications are:")
  print(extraneousPublications)
} else if (length(missingPublications) != 0){
  message("Self-assessment app has missing publications when compared to the master list. Fix it.")
  message("Missing publications are:")
  print(missingPublications)
} else {
  message("Self-assessment app has extraneous publications when compared to the master list. Fix it.")
  message("Extraneous publications are:")
  print(extraneousPublications)
}

# Update publication names ===================================================================================================================

stop("Do not source or highlight all of this script. Run lines individually.")

name_replacements <- data.table::data.table(
  original = c(
    "Further education outcome-based success measures", "GCSE and equivalent results", "Initial teacher training: trainee number census",
    "NEET statistics annual brief", "Participation rates in higher education", "Secondary and primary schools applications and offers",
    "Outcomes for children in need, including children looked after by LAs in England"
  ),
  replacement = c(
    "Further education: outcome-based success measures", "Key stage 4 performance", "Initial Teacher Training Census",
    "NEET annual brief", "Participation measures in higher education", "Secondary and primary school applications and offers",
    "Outcomes for children in need, including children looked after by local authorities in England"
  )
) %>%
  as.matrix()

updatePubNameSQL <- function(replacement_row) {
  original <- replacement_row[1]
  replacement <- replacement_row[2]

  statement <- paste0(
    "UPDATE dbo.publicationTrackingProduction SET publication = REPLACE(publication, '",
    stringr::str_replace_all(original, "'", "''"), "', '",
    stringr::str_replace_all(replacement, "'", "''"), "')"
  )

  DBI::dbSendStatement(con, statement)
}

apply(name_replacements, 1, updatePubNameSQL)

# Create publications ======================================================================================================================

stop("Do not source or highlight all of this script. Run lines individually.")

# Create a list of publications that are missing from the master list, and create those, pre-filling whether or not they are on EES
# Note that if a publication changes name then it will flag as missing, when actually you should run the renaming section above

masterTablePublications <- dplyr::tbl(con, dbplyr::in_schema("dbo","publicationMasterList")) %>%
  dplyr::collect() %>%
  dplyr::filter(where == "Find stats") %>%
  select(publication, onEES)

selfAssessmentProd <- dplyr::tbl(con, dbplyr::in_schema("dbo","publicationTrackingProduction")) %>%
  dplyr::collect()

publications_to_create <- masterTablePublications %>%
  dplyr::full_join(selfAssessmentProd, by = c("publication" = "publication")) %>%
  # View()
  dplyr::distinct(publication, published_on_ees, onEES) %>%
  dplyr::filter(is.na(published_on_ees)) %>%
  dplyr::distinct(publication, onEES) %>% 
  as.matrix()

# Create new starter rows for those publications

createInSQL <- function(publication_row) {
  publication <- publication_row[1]
  onEES <- publication_row[2]
  
  new_row <- data.frame(
    date = "2019-09-28",
    g6 = "TBC",
    tl = "TBC",
    publication = str_replace_all(publication, "'", "''"),
    published_on_ees = onEES,
    time_series_length = "No",
    processing_with_code = "No",
    sensible_folder_file_structure = "No",
    approporiate_tools = "No", # Leaving this typo in as it is the column name in the database now (also referred to in line 594, the SQL query)
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
    single_data_production_scripts_with_qa = "No",
    single_publication_script = "No",
    clean_final_code = "No",
    peer_review_outside_team = "No",
    content_checklist = "No",
    content_peer_review = "No",
    targetted_user_research = "No",
    l_and_d_requests = "No"
  )

  statement <- paste0(
    "INSERT INTO ", "dbo.publicationTrackingProduction",
    " ([date], [g6], [tl],[publication],[published_on_ees], [time_series_length], [processing_with_code], [sensible_folder_file_structure], [approporiate_tools], [single_database], [documentation], [files_meet_data_standards], [basic_automated_qa], [recyclable_code], [single_data_production_scripts], [final_code_in_repo], [automated_insight_summaries], [peer_review_within_team], [publication_specifc_automated_qa], [collab_develop_using_git], [pub_specific_automated_insight_summaries], [single_data_production_scripts_with_qa], [single_publication_script], [clean_final_code], [peer_review_outside_team], [content_checklist], [content_peer_review], [targetted_user_research], [l_and_d_requests])
                        VALUES ('", paste0(as.vector(new_row), collapse = "', '"), "')"
  )

  DBI::dbSendStatement(con, statement)
}

apply(publications_to_create, 1, createInSQL)

# Delete publications ===========================================================================================================================================

stop("Do not source or highlight all of this script. Run lines individually.")


publications_to_delete <- fread("08112021_masterlist.csv") %>% 
  filter(rap == "No") %>% 
  pull(publication)

publications_to_delete <- c("Destinations of key stage 4 and key stage 5 pupils", "Graduate Outcomes (LEO): all publications")

deleteFromSQL <- function(publication) {
  statement <- paste0("DELETE FROM dbo.publicationTrackingProduction WHERE publication = '", stringr::str_replace_all(publication, "'", "''"), "';")
  DBI::dbSendStatement(con, statement)
}

lapply(publications_to_delete, deleteFromSQL) # You will get errors saying expired and Cancelling previous query, however it does genuinely delete them

