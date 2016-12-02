fetch.histWaterUse <- function(viz){
  library(readxl)
  
  filePath <- tempdir()
  
  download.file("ftp://ftpint.usgs.gov/private/er/wi/middleton/dblodgett/SIU_USStateWaterUse1950_80.xls", file.path(filePath,"SIU_USStateWaterUse1950_80.xls"), mode='wb')
  
  histExDat <- list(list(sheet = "1950", skip = 12, ind = c("sIndGw", "sIndSurf"), iri = "irrTotal", pub = NULL, thr = NULL),
                    list(sheet = "1955", skip = 22, ind = "sIndTotal", iri = "irrTotal", pub = "pubTotal", thr = NULL),
                    list(sheet = "1960", skip = 12, ind = "sIndTotFr", iri = "irrTotal", pub = "pubTotal", thr = "sThmTotFr"),
                    list(sheet = "1965", skip = 13, ind = "sIndTotFr", iri = "irrTotal", pub = "pubTotal", thr = "sThmTotFr"),
                    list(sheet = "1970", skip = 16, ind = c("sIndGwFr", "sIndSurfFr"), iri = "irrTotal", pub = "pubTotal", thr = "sThmTotFr"),
                    list(sheet = "1975", skip = 15, ind = "sIndTotFr" , iri = "irrTotal", pub = "pubTotal", thr = "sThmTotFr"),
                    list(sheet = "1980", skip = 16, ind = c("sIndGwFr", "sIndSurfFr"), iri = "irrTotal", pub = "pubTotal", thr = "sThmTotFr"))
  histDat <- list()
  
  for(sheet in 1:length(histExDat)) {
    sheet.data <- list(read_excel(file.path(filePath,"SIU_USStateWaterUse1950_80.xls"), sheet = histExDat[[sheet]]["sheet"][[1]], skip = histExDat[[sheet]]["skip"][[1]]))
    histDat <- append(histDat, sheet.data)
  }
  
  saveRDS(histDat, viz[["location"]])
  
}