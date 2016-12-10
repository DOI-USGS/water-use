process.nationalClean <- function(viz){
  library(tidyr)
  library(dplyr)
  
  stateData <- readData(viz[['depends']][["stateData"]])
  
  national <- stateData %>%
    group_by(year, category) %>%
    summarise(value = sum(value)) %>%
    data.frame()
  
  national$value[national$year < 1960 & 
                   national$category %in% c("Industrial","Thermoelectric")] <- NA
  
  saveRDS(national, file=viz[["location"]])
}