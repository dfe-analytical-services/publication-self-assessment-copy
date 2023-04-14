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

stop("Do not source or highlight all of this script. Run desired lines individually.")

#check prod and dev tables are the same
prodData <- dplyr::tbl(con, dbplyr::in_schema("dbo","publicationTrackingProduction")) %>%
  dplyr::collect()

devData <- dplyr::tbl(con, dbplyr::in_schema("dbo","publicationTrackingDevelopment")) %>%
  dplyr::collect()

# Discounted publications

fread("cam_manual_faffing/08112021_masterList.csv") %>% 
  filter(rap == "No") %>% 
  pull(publication)

# Count of publications

totalPubs <- prodData %>% 
  dplyr::distinct(publication) %>% 
  nrow()

# Publication responses

nonResponders <- prodData %>%
  dplyr::mutate(date = as.character(date)) %>% 
  dplyr::filter(date == "2019-09-28 00:00:00") %>% # View()
  dplyr::filter(!publication %in% pubsToRemove$publication) %>% 
  dplyr::distinct(publication)

message("Out of ", totalPubs, " publications, ", nrow(nonResponders), " are yet to respond. These are: ")
print(nonResponders)

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

message("Progress: ", length(onEESactually) - 1, " out of ", totalPubs, " publications are on EES.") # Using -1 for now as there's one supplementary publication that isn't really a publication - though we could include for ease?


# Update publication names ===================================================================================================================

stop("Do not source or highlight all of this script. Run desired lines individually.")

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
    "UPDATE dbo.publicationTrackingDevelopment SET publication = REPLACE(publication, '",
    stringr::str_replace_all(original, "'", "''"), "', '",
    stringr::str_replace_all(replacement, "'", "''"), "')"
  )

  DBI::dbSendStatement(con, statement)
}

apply(name_replacements, 1, updatePubNameSQL)

# Create publications ======================================================================================================================

stop("Do not source or highlight all of this script. Run desired lines individually.")

# Create vector of publications to create

publications_to_create <- c("", "")

# Create new starter rows for publications

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
    #publication_specifc_automated_qa = "No",
    collab_develop_using_git = "No",
    pub_specific_automated_insight_summaries = "No",
    single_data_production_scripts_with_qa = "No",
    single_publication_script = "No",
    clean_final_code = "No",
    open_source_repo = "No",
    peer_review_outside_team = "No",
    content_checklist = "No",
    content_peer_review = "No",
    targetted_user_research = "No",
    l_and_d_requests = "No"
  )

  statement <- paste0(
    "INSERT INTO ", "dbo.publicationTrackingDevelopment",
    " ([date], [g6], [tl],[publication],[published_on_ees], [time_series_length], [processing_with_code], [sensible_folder_file_structure], [approporiate_tools], [single_database], [documentation], [files_meet_data_standards], [basic_automated_qa], [recyclable_code], [single_data_production_scripts], [final_code_in_repo], [automated_insight_summaries], [peer_review_within_team], [publication_specifc_automated_qa], [collab_develop_using_git], [pub_specific_automated_insight_summaries], [single_publication_script], [clean_final_code], [open_source_repo], [peer_review_outside_team], [content_checklist], [content_peer_review], [targetted_user_research], [l_and_d_requests])
                        VALUES ('", paste0(as.vector(new_row), collapse = "', '"), "')"
  )

  DBI::dbSendStatement(con, statement)
}

apply(publications_to_create, 1, createInSQL)

# Delete publications ===========================================================================================================================================

stop("Do not source or highlight all of this script. Run desired lines individually.")

publications_to_delete <- c("", "")

deleteFromSQL <- function(publication) {
  statement <- paste0("DELETE FROM dbo.publicationTrackingDevelopment WHERE publication = '", stringr::str_replace_all(publication, "'", "''"), "';")
  DBI::dbSendStatement(con, statement)
}

lapply(publications_to_delete, deleteFromSQL) # You will get errors saying expired and Cancelling previous query, however it does genuinely delete them

# Copy changes into the prod table ===========================================================================================================================================

stop("Do not source or highlight all of this script. Run desired lines individually.")

#check the dev table before copying
devData <- dplyr::tbl(con, dbplyr::in_schema("dbo","publicationTrackingDevelopment")) %>%
  dplyr::collect()

copy_data_to_prod_db()


# In case of emergency ===========================================================================================================================================

#Using the latest downloaded data
data <- read.csv("....../All data2022-12-08.csv")

#In order to overwrite or append the existing DB table, you must manually add apend or overwrite = TRUE to the below:
dbWriteTable(con, Id(schema = "dbo", table = "publicationTrackingDevelopment"), data)