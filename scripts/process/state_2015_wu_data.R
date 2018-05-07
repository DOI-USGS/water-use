#' Simplify original water use data to only what the viz needs
#'
process.state_15_wu_data <- function(viz) {
  library(dplyr)
  library(tidyr)
  
  deps <- readDepends(viz)
  wu_df <- deps[["wu_data_15_simple"]]

  wu_df_sel <- wu_df %>%
    group_by(STATEFIPS) %>%
    dplyr::summarise(Total = sum(total),
                   Thermoelectric = sum(thermoelectric),
                   `Public Supply` = sum(publicsupply),
                   Irrigation = sum(irrigation),
                   Industrial = sum(industrial)) %>%
    gather(category, value, -STATEFIPS) %>%
    rename(state_cd = STATEFIPS) %>%
    left_join(select(dataRetrieval::stateCd, state_cd=STATE, state_name=STATE_NAME), by="state_cd") %>%
    mutate(year = 2015)
    
  
  saveRDS(wu_df_sel, viz[["location"]])
}