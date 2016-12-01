
process.treemap <- function(viz){
  library(dplyr)  
  library(treemap)
  
  nationalData_list <- readDepends(viz)[[1]] 
  wateruse_data <- nationalData_list[['nationalData']]%>% 
    mutate(category = as.factor(category),
           color = as.numeric(category))

  # create treemap for each year, save to list (ignore total category)
  yrs_in_data <- sort(unique(wateruse_data$year))
  all_treemaps <- lapply(yrs_in_data, function(y, wu_data, viz){
    wateruse_year <- wu_data %>% filter(year == y)
    treemap(wateruse_year, index="category", vSize="value", type="index", title=y)
  }, wu_data = filter(wateruse_data, category != "Total withdrawals, in Bgal/d"), viz)
  names(all_treemaps) <- paste0('tm_', yrs_in_data)
  
  # add total category to list
  all_treemaps$total_wu <- filter(wateruse_data, category == "Total withdrawals, in Bgal/d")
  
  saveRDS(all_treemaps, file=viz[['location']])
}
