process.nationalClean <- function(viz){
  library(tidyr)
  library(dplyr)
  
  national <- readData(viz[['depends']][['nationalUse']])
  
  a_matrix <- data.frame(matrix(FALSE, nrow = nrow(national), ncol = ncol(national)))
  names(a_matrix) <- names(national)
  
  subList <- list("a" = a_matrix, 
                  "b" = a_matrix, 
                  "c" = a_matrix)

  for(j in c("a","b","c")){
    for(i in colnames(national)[-1]){
      index <- grep(j,national[[i]])
      if(length(index) > 0){
        national[[i]] <- as.numeric(national[[i]])
      } else {
        subList[[j]][[i]][index] <- TRUE
        national[[i]] <- as.numeric(gsub(j, "", national[[i]]))
      }
    }
  }

  saveRDS(list("nationalData" = national, 
               "dataNotes" = subList),
          file=viz[["location"]])
}