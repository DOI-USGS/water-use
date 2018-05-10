process.mtClean <- function(viz){
  library(dplyr)
  library(tidyr)
  
  waterMT <- readData(viz[['depends']])

  cleaned_mt <- waterMT %>%
    filter(year == 2010) %>%
    select(year, 
           Irrigation = `Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d`,
           `Public Supply` = `Public Supply total self-supplied withdrawals, fresh, in Mgal/d`) %>%
    gather(category, value, -year) %>%
    mutate(state_cd = as.character(dataRetrieval::stateCdLookup("MT", "id")),
           state_name = dataRetrieval::stateCdLookup("MT", "fullName"))
  
  saveRDS(cleaned_mt, viz[["location"]])
  
}