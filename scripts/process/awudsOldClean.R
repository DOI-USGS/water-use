process.awudsOldClean <- function(viz){
  library(tidyr)
  library(dplyr)
  library(dataRetrieval)
  
  wuAwuds <- readData(viz[['depends']][['histWaterUse']])
  wuData <- readData(viz[['depends']][['waterUse']])
  
  wuAwuds <- wuAwuds %>%
    rename(state_cd = Area, 
           year = YEAR) %>%
    mutate(year = as.numeric(year),
           state_name = stateCdLookup(state_cd, outputType = "fullName")) %>%
    select(state_cd, state_name, year, everything())
  
  histExDat <- list(list(sheet = "1950", ind = c("INPT.WGWFr", "INPT.WSWFr"), iri = c("IR.WGWFr", "IR.WSWFr"), pub = c("PS.WGWFr", "PS.WSWFr"), thr = NULL),
                    list(sheet = "1955", ind = c("INPT.WGWFr", "INPT.WSWFr"), iri = c("IR.WGWFr", "IR.WSWFr"), pub = c("PS.WGWFr", "PS.WSWFr"), thr = NULL),
                    list(sheet = "1960", ind = c("OI.WGWFr", "OI.WSWFr"), iri = c("IR.WGWFr", "IR.WSWFr"), 
                         pub = c("PS.WGWFr", "PS.WSWFr"), thr = c("PT.WGWFr", "PT.WSWFr")),
                    list(sheet = "1965", ind = c("OI.WGWFr", "OI.WSWFr"), iri = c("IR.WGWFr", "IR.WSWFr"), 
                         pub = c("PS.WGWFr", "PS.WSWFr"), thr = c("PT.WGWFr", "PT.WSWFr")),
                    list(sheet = "1970", ind = c("OI.WGWFr", "OI.WSWFr"), iri = c("IR.WGWFr", "IR.WSWFr"), 
                         pub = c("PS.WGWFr", "PS.WSWFr"), thr = c("PT.WGWFr", "PT.WSWFr")),
                    list(sheet = "1975", ind = c("OI.WGWFr", "OI.WSWFr"), iri = c("IR.WGWFr", "IR.WSWFr"), 
                         pub = c("PS.WGWFr", "PS.WSWFr"), thr = c("PT.WGWFr", "PT.WSWFr")),
                    list(sheet = "1980", ind = c("OI.WGWFr", "OI.WSWFr"), iri = c("IR.WGWFr", "IR.WSWFr"), 
                         pub = c("PS.WGWFr", "PS.WSWFr"), thr = c("PT.WGWFr", "PT.WSWFr")))
  
  
  full.data <- data.frame(year = as.integer(),
                          state_cd = as.character(),
                          state_name = as.character(),
                          Industrial = as.numeric(),
                          Irrigation = as.numeric(),
                          public = as.numeric(),
                          Thermoelectric = as.numeric(),
                          Total = as.numeric(),
                          stringsAsFactors = FALSE)
  
  for(i in seq_along(histExDat)){
    dataYear <- filter(wuAwuds, year == as.numeric(histExDat[[i]][["sheet"]]))
    
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

    Total <- rowSums(data.frame(Industrial, Irrigation, public, Thermoelectric), na.rm = TRUE)
    
    if(as.numeric(histExDat[[i]][["sheet"]]) == 1950 || as.numeric(histExDat[[i]][["sheet"]]) == 1955) {
      Thermoelectric <- NA
      Industrial <- NA
    } 
    
    dataClean <- bind_cols(select(dataYear, state_cd, state_name, year ),
                           data.frame(Industrial, Irrigation, public, Thermoelectric, Total)) 
    
    if(as.numeric(histExDat[[i]][["sheet"]]) == 1950 || as.numeric(histExDat[[i]][["sheet"]]) == 1955) { 
      dataClean[which(dataClean$state_name == "Hawaii" | dataClean$state_name == "Alaska"), 3:8] <- NA
    }
    
    full.data <- bind_rows(full.data, dataClean)
    
  }
  
  wuAwudsLong <- rename(full.data, `Public Supply` = public) %>%
    gather(category, value, -state_cd, -state_name, -year) 
  
  wuAwudsLong$state_name[wuAwudsLong$state_name == "District of Columbia"] <- "district of columbia"
  
  wuData$state_name[wuData$state_name == "Dist. of Columbia"] <- "district of columbia"
    
  
  full.out <- bind_rows(wuData, wuAwudsLong) %>%
    arrange(state_name, category, year)
  
  saveRDS(full.out, file = viz[["location"]])
  
}
