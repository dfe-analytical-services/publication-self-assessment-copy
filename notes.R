# making new rds file based on csv -------------------------------------------------------------------

new_data <- as.data.frame(read.csv("csv-data.csv"))

saveRDS(new_data, "new_tracker_data.rds")

# writing data to SQL from R

library()


