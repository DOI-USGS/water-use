fetch.waterUse <- function(viz){
  library(dataRetrieval)
  library(dplyr)
  
  all.states <- data.frame()
  
  for(i in c(state.abb,"DC")){
    state.data <- readNWISuse(stateCd = i, countyCd = NULL, convertType = FALSE)
    all.states <- bind_rows(all.states, state.data)
  }
  
  write.csv(all.states, viz[["location"]], row.names = FALSE)
  
}

fetch.wu_data_MT <- function(viz){
  
  library(data.table)
  mt_data <- setDF(fread(viz[["file_path"]]))
  
  saveRDS(mt_data, viz[["location"]])
  
}