library(tidyr)
library(dplyr)

process.scaleNational <- function(viz) {

  nationalData <- readData(viz[['depends']][['nationalClean']])

  # nationalData<-readRDS("cache/nationalClean.rds")

  nationalData<-spread(nationalData$nationalData, category, value)[,c(1,2,5,8,10,11,16)]

  names(nationalData) <- c("Year", "Population", "Irrigation", "Public Supply", "Industrial", "Thermoelectric", "Total")

  scaleNational <- list()

  scaleNational["totData"][[1]] <- gather(nationalData, key = category, value = value, -Year, -Population)

  scaleNational["pCapData"][[1]] <- scaleNational$totData %>% 
    mutate(value = value/Population) %>% 
    mutate(value = value*365)

  maxList <- list(totData = 500, pCapData = 800)

  for(col in c("totData", "pCapData")) {
    scaleNational[col][[1]] <- mutate(scaleNational[col][[1]], barScale = value / maxList[col][[1]])
  }
  
  saveRDS(scaleNational,
          file=viz[["location"]])
  
}

