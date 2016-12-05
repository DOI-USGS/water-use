process.awudsOldClean <- function(viz){
  library(tidyr)
  library(dplyr)
  library(dataRetrieval)
  
  wuAwuds <- readData(viz[['depends']][['histWaterUse']])
  wuData <- readData(viz[['depends']][['waterUse']])
  
  
  
  wuAwudsLong <- gather(wuAwuds, category, value, -Area, -YEAR) %>%
    rename(state_cd = Area, 
           year = YEAR) %>%
    mutate(year = as.numeric(year),
           state_name = stateCdLookup(state_cd, outputType = "fullName")) %>%
    select(state_cd, state_name, year, category, value)
  
  
  full.out <- bind_rows(wuData, wuAwudsLong) %>%
    arrange(state_name, category, year)
  
  saveRDS(full.out, file = viz[["location"]])
  
}
