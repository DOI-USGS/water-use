fetch.waterUse <- function(viz){
  library(dataRetrieval)
  library(dplyr)
  
  all.states <- data.frame()
  
  for(i in tolower(state.abb)){
    state.url <- paste0("http://waterdata.usgs.gov/",i,
                        "/nwis/water_use?format=rdb&rdb_compression=value&",
                        "wu_area=State Total&",
                        "wu_year=ALL&",
                        "wu_category=ALL&",
                        "wu_category_nms=--ALL Categories--")
    dater <- importRDB1(state.url, convertType = FALSE)
    all.states <- bind_rows(all.states, dater)
  }
  
  write.csv(all.states, viz[["location"]], row.names = FALSE)
  
}