# making new rds file based on csv -------------------------------------------------------------------

# new_data <- as.data.frame(read.csv("csv-data.csv"))
# 
# saveRDS(new_data, "new_tracker_data.rds")
# 
# # writing data to SQL from R
# 
# # running locally ------------------------------------------------------------------------------------
# 
# conWINDOWS <- DBI::dbConnect(odbc::odbc(),
#                  Driver = "ODBC Driver 13 for SQL Server",
#                  Server = "T1PRANMSQL\\SQLPROD,60125",
#                  Database = "MA_SDT_NS_DATA",
#                  UID = "",
#                  PWD = "",
#                  Trusted_Connection = "Yes")
# 
# conAPP <- DBI::dbConnect(odbc::odbc(),
#                              Driver = "ODBC Driver 13 for SQL Server",
#                              Server = "T1PRANMSQL.ad.hq.dept,60125",
#                              Database = "MA_SDT_NS_DATA",
#                              UID = "username",
#                              PWD = "password",
#                              Trusted_Connection = "No")
# 
# # writing a new SQL table ----------------------------------------------------------------------------
# 
# downloaded_data <- fread("data/All data2020-11-17.csv", encoding = "UTF-8", na.strings = "", strip.white = FALSE)
# 
# dbWriteTable(connection, "publicationTrackingPreProduction", downloaded_data)
# 
# 
# # app connection -------------------------------------------------------------------------------------
# 
# # use tbl() %>% collect() to pull the data in, then dbWriteTable() to write it back out, also something around live updates and invalidating?
# 
# tableData <- conWINDOWS %>% tbl("publicationTracking") %>% collect()