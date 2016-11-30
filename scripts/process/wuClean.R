process.wuClean <- function(viz){
  library(tidyr)
  library(dplyr)
  
  rawWaterUse <- readData(viz[['depends']][['waterUse']])
  
  longWU <- gather(rawWaterUse, category, value, -state_cd, -state_name, -year)

  uglyCats <- c("Public.Supply.total.self.supplied.withdrawals..total..in.Mgal.d",
                "Irrigation..Total.total.self.supplied.withdrawals..in.Mgal.d",
                "Industrial.total.self.supplied.withdrawals..in.Mgal.d",
                "Total.Thermoelectric.Power.total.self.supplied.withdrawals..total..in.Mgal.d")
  
  longWU <- longWU %>%
    mutate(year = as.numeric(year)) %>%
    filter(value != "-") %>%
    filter(category %in% uglyCats) %>%
    mutate(value = as.numeric(value))
  
  longWU$category[longWU$category == uglyCats[1]] <- "Public Supply"
  longWU$category[longWU$category == uglyCats[2]] <- "Irrigation"
  longWU$category[longWU$category == uglyCats[3]] <- "Industrial"
  longWU$category[longWU$category == uglyCats[4]] <- "Thermoelectric"
  
  saveRDS(longWU, file=viz[["location"]])
}