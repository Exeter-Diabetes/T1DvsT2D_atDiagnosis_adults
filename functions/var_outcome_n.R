var_outcome_n <- function(varlist = varlist, varlist_cat = varlist_cat, dataset){
  var_outcome_table <- data.frame()
  varlist_all <- c(varlist, varlist_cat)
  for (var in varlist_all){
    sum_table <- data.frame(variable = NA, n=NA, nT1D = NA, nT2D = NA)
    sum_table$variable <- var
    sum_table$n <- sum(!is.na(dataset[[var]] == TRUE))
    sum_table$nT1D <- sum(!is.na(dataset[[var]]) == TRUE & dataset$insulinRequire3 == "1")
    sum_table$nT2D <- sum(!is.na(dataset[[var]]) == TRUE & dataset$insulinRequire3 == "0")
    var_outcome_table <- rbind(var_outcome_table, sum_table)
  }
  var_outcome_table <<- as.data.frame(var_outcome_table)
}
