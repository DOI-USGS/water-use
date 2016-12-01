process.histWuClean <- function(viz){
  library(tidyr)
  library(dplyr)
  
  wuExcel <- readData(viz[['depends']][['histWaterUse']])
  wuData <- readData(viz[['depends']][['waterUse']])

  histExDat <- list(list(sheet = "1950", skip = 12, ind = c("sIndGw", "sIndSurf"), iri = "irrTotal", pub = NULL, thr = NULL),
                    list(sheet = "1955", skip = 22, ind = "sIndTotal", iri = "irrTotal", pub = "pubTotal", thr = NULL),
                    list(sheet = "1960", skip = 12, ind = "sIndTotFr", iri = "irrTotal", pub = "pubTotal", thr = "sThmTotFr"),
                    list(sheet = "1965", skip = 13, ind = "sIndTotFr", iri = "irrTotal", pub = "pubTotal", thr = "sThmTotFr"),
                    list(sheet = "1970", skip = 16, ind = c("sIndGwFr", "sIndSurfFr"), iri = "irrTotal", pub = "pubTotal", thr = "sThmTotFr"),
                    list(sheet = "1975", skip = 15, ind = "sIndTotFr" , iri = "irrTotal", pub = "pubTotal", thr = "sThmTotFr"),
                    list(sheet = "1980", skip = 16, ind = c("sIndGwFr", "sIndSurfFr"), iri = "irrTotal", pub = "pubTotal", thr = "sThmTotFr"))
  
}
