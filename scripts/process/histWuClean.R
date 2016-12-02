process.histWuClean <- function(viz){
  library(tidyr)
  library(dplyr)
  
  wuExcel <- readData(viz[['depends']][['histWaterUse']])
  wuData <- readData(viz[['depends']][['waterUse']])

  histExDat <- list(list(sheet = "1950", skip = 12, ind = c("sIndGw", "sIndSurf"), iri = "irrTotal", pub = NULL, thr = NULL),
                    list(sheet = "1955", skip = 22, ind = "sIndTotal", iri = "irrTotal", pub = "pubTotal", thr = NULL),
                    list(sheet = "1960", skip = 12, ind = "sIndTotFr", iri = "irrTotal", pub = "pubTotal", thr = "sThmTotFr"),
                    list(sheet = "1965", skip = 13, ind = "sIndTotFr", iri = "irrTot", pub = "pubTotal", thr = "sThmTotFr"),
                    list(sheet = "1970", skip = 16, ind = c("sIndGwFr", "sIndSurfFr"), iri = "irrTot", pub = "pubTotal", thr = "sThmTotFr"),
                    list(sheet = "1975", skip = 15, ind = "sIndTotFr" , iri = "irrTot", pub = "pubTotal", thr = "sThmTotFr"),
                    list(sheet = "1980", skip = 16, ind = c("sIndGwFr", "sIndSurfFr"), iri = "irrTot", pub = "pubTotal", thr = "sThmTotFr"))
  
  full.data <- data.frame(Year = as.integer(),
                          StateCode = as.character(),
                          StateName = as.character(),
                          Industrial = as.numeric(),
                          Irrigation = as.numeric(),
                          public = as.numeric(),
                          Thermoelectric = as.numeric(),
                          stringsAsFactors = FALSE)
  
  for(i in seq_along(histExDat)){
    dataYear <- wuExcel[[i]]

    if(!is.null(histExDat[[i]][["ind"]])){
      Industrial <- rowSums(dataYear[,histExDat[[i]][["ind"]]], na.rm = TRUE)
    } else {
      Industrial <- NA
    }

    if(!is.null(histExDat[[i]][["iri"]])){
      Irrigation <- rowSums(dataYear[,histExDat[[i]][["iri"]]], na.rm = TRUE)
    } else {
      Irrigation <- NA
    }
    
    if(!is.null(histExDat[[i]][["pub"]])){
      public <- rowSums(dataYear[,histExDat[[i]][["pub"]]], na.rm = TRUE)
    } else {
      public <- NA
    }
    
    if(!is.null(histExDat[[i]][["thr"]])){
      Thermoelectric <- rowSums(dataYear[,histExDat[[i]][["thr"]]], na.rm = TRUE)
    } else {
      Thermoelectric <- NA
    }
      
    if(dataYear$Year[1] == 1950){
      Irrigation <- Irrigation*0.8921
    }
    
    dataClean <- bind_cols(dataYear[,c("Year", "StateCode", "StateName")] ,
                           data.frame(Industrial, Irrigation, public, Thermoelectric)) %>%
      rename(`Public Supply` = public) %>%
      select(-public)
    
    full.data <- bind_rows(full.data, dataClean)
    
  }
  
  full.long <- gather(full.data, category, value, -Year, -StateCode, -StateName) %>%
    rename(state_cd = StateCode,
           state_name = StateName,
           year = Year)
  
  full.out <- bind_rows(wuData, full.long)
  
  saveRDS(full.out, viz[["location"]])
  
}
