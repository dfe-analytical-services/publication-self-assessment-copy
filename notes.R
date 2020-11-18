# making new rds file based on csv -------------------------------------------------------------------

new_data <- as.data.frame(read.csv("csv-data.csv"))

saveRDS(new_data, "new_tracker_data.rds")

# writing data to SQL from R

# running locally ------------------------------------------------------------------------------------

con <- dbConnect(odbc::odbc(),
                 Driver = "ODBC Driver 13 for SQL Server",
                 Server = "T1PRANMSQL\\SQLPROD,60125",
                 Database = "MA-SDT-NS-DATA",
                 UID = "",
                 PWD = "",
                 Trusted_Connection = "Yes")


# app connection -------------------------------------------------------------------------------------

# use tbl() %>% collect() to pull the data in