library(tidyr)
library(dplyr)

process.scaleNational <- function(viz) {

  nationalData <- readData(viz[['depends']][['nationalClean']])
  
  breaks <- function(x) {
    for(i in 1:length(x)) {
      if(x[i]<=50) x[i] <- 50
      if(x[i]>50) x[i] <- ceiling(x[i]/100)*100
    }
    return(x)
  }

  maxList <- aggregate(value ~ category, data = nationalData, max) %>%
    mutate(maxCat = breaks(value)) %>%
    select(-value)
  
  scaleNational <- left_join(nationalData, maxList, by = "category") %>%
    group_by(category) %>%
    mutate(barScale = value / maxCat) %>%
    select(-maxCat)
  
  saveRDS(scaleNational, file=viz[["location"]])
  
}

