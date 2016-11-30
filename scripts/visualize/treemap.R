
visualize.treemap <- function(viz){
  ### getting data ready
  library(dplyr)
  wateruse_data <- readData(viz[['depends']][['waterUse_clean']]) %>% 
    mutate(category = as.factor(category),
           color = as.numeric(category))
  
  #### trying out different tree maps
  
  # library(portfolio)
  # treemap_1985 <- map.market(id=1:nrow(wateruse_1985), 
  #                            area=wateruse_1985$value, 
  #                            group=wateruse_1985$category, 
  #                            color=wateruse_1985$color, 
  #                            main="Wateruse 1985")
  
  
  library(treemap)
  
  yrs_in_data <- sort(unique(wateruse_data$year))
  lapply(yrs_in_data, function(y, wateruse_data){
    wateruse_year <- wateruse_data %>% filter(year == y)
    treemap(wateruse_year, index="category", vSize="value", type="index", title=y)
    filepath_year <- file.path(viz[['location']], paste0(y, '.png'))
    png(filename=filepath_year)
  }, wateruse_data)

}
