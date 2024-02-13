source("renv/activate.R")

copy_data_to_prod_db <- function() {
  message("Copying publicationTrackingDevelopment to publicationTrackingProduction")
  
  dbWriteTable(
    conn = connection,
    name = Id(
      schema  = "dbo",
      table   = "publicationTrackingProduction"
    ),
    value = tbl(connection, "publicationTrackingDevelopment") %>% as.data.frame(),
    overwrite = TRUE
  )
  
  message("Complete!")
  message("")
}

