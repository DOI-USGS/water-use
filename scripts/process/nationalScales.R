library(tidyr)
library(dplyr)

process.nationalScales <- function(viz) {

  nationalData <- readData(viz[['depends']][['nationalClean']])

  # nationalData<-readRDS("cache/nationalClean.rds")

  nationalData<-spread(nationalData$nationalData, category, value)[,c(1,2,5,8,10,11,16)]

  names(nationalData) <- c("Year", "Population", "Irrigation", "Public Supply", "Industrial", "Thermoelectric", "Total")

  nationalScales <- list()

  nationalScales["totData"][[1]] <- gather(nationalData, key = category, value = value, -Year, -Population)

  nationalScales["pCapData"][[1]] <- nationalScales$totData %>% 
    mutate(value = value/Population) %>% 
    mutate(value = value*365)

  maxList <- list(totData = 500, pCapData = 800)

  for(col in c("totData", "pCapData")) {
    nationalScales[col][[1]] <- mutate(nationalScales[col][[1]], barScale = value / maxList[col][[1]])
  }
  
  saveRDS(nationalScales,
          file=viz[["location"]])
  
}