fetch.histWaterUseAWUDS <- function(viz){
  
  library(wateRuse)
  library(tidyr)
  
  awudsOLD <- get_awuds_data(awuds.data.path = "data")
  
  saveRDS(awudsOLD, viz[["location"]])
  
}