#' Simplify original water use data to only what the viz needs
#'
process.simplify_wu_data <- function(viz) {
  
  deps <- readDepends(viz)
  wu_df <- deps[["wu_data_15"]]
  orig_wu_type_col <- viz[["orig_names"]]
  new_wu_type_col <- viz[["new_names"]]
  
  columns_to_keep <- c("STATE", "STATEFIPS", "COUNTY", "COUNTYFIPS", "FIPS", "YEAR") # also double as character cols
  columns_to_rename <- c("TP-TotPop", orig_wu_type_col)
  new_names <- c("countypop", new_wu_type_col)
  
  # none of the columns we kept had characters (-- or N/A) that caused the entire column to
  # be read in as character and needed to be converted back to numeric, but in case
  # we add more data as we go along, here is the code that does that:
  which_numeric_cols <- !names(wu_df) %in% columns_to_keep
  numeric_data <- wu_df[, which_numeric_cols]
  fixed_numeric_data <- as.data.frame(sapply(numeric_data, function(v) { 
    has_missing_symbol <- grep("--|N/A", v)
    v[has_missing_symbol] <- NA
    v <- as.numeric(v)
    return(v)
  }))
  wu_df[, which_numeric_cols] <- fixed_numeric_data
  
  # now select only the pertinent columns
  wu_df_sel <- wu_df[, c(columns_to_keep, columns_to_rename)]
  names(wu_df_sel) <- c(columns_to_keep, new_names)
  
  saveRDS(wu_df_sel, viz[["location"]])
}