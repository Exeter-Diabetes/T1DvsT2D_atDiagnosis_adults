library(dplyr)
library(purrr)
library(rlang)

apply_thresholds_tidy <- function(data, threshold_ref_table) {
  
  # Use purrr::pmap to iterate over the rows of the threshold_ref_table
  new_cols <- pmap(threshold_ref_table, function(variable, direction, threshold) {
    
    # Get the vector to check
    values <- data[[variable]]
    
    # Build and evaluate condition dynamically using rlang
    condition_expr <- parse_expr(paste0("values ", direction, " ", threshold))
    condition <- eval(condition_expr)
    
    # Return Pass/Fail/Missing vector
    case_when(
      is.na(condition)        ~ "Missing",
      condition               ~ "Pass",
      !condition              ~ "Fail"
    )
  })
  
  # Name the new columns with _status suffix
  names(new_cols) <- paste0(threshold_ref_table$variable, "_status")
  
  # Bind new columns to the original data
  bind_cols(data, new_cols)
}