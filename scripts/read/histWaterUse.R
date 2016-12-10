readData.histWaterUse <- function(viz){

  library(wateRuse)
  library(tidyr)
  files <- unzip(zipfile = viz[['location']], exdir = dirname(viz[['location']]), overwrite = TRUE)
  awudsOLD <- get_awuds_data(awuds.data.files = files)

  return(awudsOLD)

}
