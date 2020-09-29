

# making new rds file based on csv

new_data <- as.data.frame(read.csv("csv-data.csv"))

saveRDS(new_data, "new_tracker_data.rds")





# testing table output 

datatable(t(data),selection = 'single',
          escape=F,
          class = list(stripe = FALSE),
          options = list(dom = 't', # simple table output (add other letters for search, filter etc)
                         headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}") # removes header
          )
) %>% formatStyle(
  ' ', #rownames col (replace with V1, V2 etc for others)
  backgroundColor = 'lightgray',
  fontWeight = 'bold'
)







