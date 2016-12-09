library(tidyr)
library(dplyr)

process.scaleNational <- function(viz) {

  nationalData <- readData(viz[['depends']][['nationalClean']])
  
  # nationalData<-readRDS("cache/nationalClean.rds")
# 
#   nationalData<-spread(nationalData$nationalData, category, value)[,c(1,2,5,8,10,11,16)]
# 
#   names(nationalData) <- c("Year", "Population", "Irrigation", "Public Supply", "Industrial", "Thermoelectric", "Total")
# 
#   scaleNational <- list()
# 
#   scaleNational$totData <- gather(nationalData, key = category, value = value, -Year, -Population) %>% # billion gallons per day
#     mutate(value = value * 1000) # million gallons per day
# 
#   # scaleNational$pCapData <- scaleNational$totData %>% # billion gallons per year
  #   mutate(value = value/Population) %>% # thousand gallons per year per person
  #   mutate(value = value*365) # thousand gallons per day per person
  # 
  # maxList <- list(totData = list(), pCapData = list())
  
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
  
  
  # for(col in c("totData", "pCapData")) {
  #     maxList[[col]] <- aggregate(value ~ category, data = scaleNational$totData, max) %>% 
  #       mutate(maxCat = breaks(value)) %>%
  #       select(-value)
  #     scaleNational[col][[1]] <- left_join(scaleNational[col][[1]], maxList[[col]], by = "category") %>%
  #       group_by(category) %>%
  #       mutate(barScale = value / maxCat) %>%
  #       select(-maxCat)
  # }
  
  saveRDS(scaleNational, file=viz[["location"]])
  
}

