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

  maxList <- list(totData = list(), pCapData = list())
  
  breaks <- function(x) {
    for(i in 1:length(x)) {
      if(x[i]<=50) x[i] <- 50
      if(x[i]>50) x[i] <- ceiling(x[i]/100)*100
    }
    return(x)
  }
  
  for(col in c("totData", "pCapData")) {
      maxList[[col]] <- aggregate(value ~ category, data = scaleNational$totData, max) %>% 
        mutate(maxCat = breaks(value)) %>%
        select(-value)
      scaleNational[col][[1]] <- left_join(scaleNational[col][[1]], maxList[[col]], by = "category") %>%
        group_by(category) %>%
        mutate(barScale = value / maxCat) %>%
        select(-maxCat)
  }
  
  saveRDS(scaleNational,
          file=viz[["location"]])
  
}

