
fetch.nationalUse <- function(viz){

  library(dataRetrieval)
  
  national <- readNWISuse(stateCd = NULL, 
                          countyCd = NULL, 
                          transform = TRUE)
  
  write.csv(national, viz[["location"]], row.names = FALSE)

}