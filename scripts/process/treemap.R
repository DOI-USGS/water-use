
visualize.treemap <- function(viz){
  ### getting data ready
  library(dplyr)
  nationalData_list <- readDepends(viz)[[1]] 
  wateruse_data <- nationalData_list[['nationalData']]%>% 
    mutate(category = as.factor(category),
           color = as.numeric(category))
  
  library(treemap)
  
  yrs_in_data <- sort(unique(wateruse_data$year))
  lapply(yrs_in_data, function(y, wateruse_data, viz){
    wateruse_year <- wateruse_data %>% filter(year == y)
    filepath_year <- file.path(viz[['location']], paste0(y, '.png'))
    png(filename=filepath_year)
    treemap(wateruse_year, index="category", vSize="value", type="index", title=y)
    dev.off()
  }, wateruse_data, viz)

}
