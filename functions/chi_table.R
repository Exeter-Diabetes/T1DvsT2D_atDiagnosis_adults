chi_table <- function(varlist_cat = varlist_cat, dataset = dataset, outcome = outcome){
  #browser()
  chiTable <- data.frame()
  #outcome
  for (var in varlist_cat){
    print(var)
    chi <- chisq.test(table(dataset[[outcome]], dataset[[var]]))
    result_table <- data.frame(variable = NA, p_value = NA)
    result_table$variable <- var
    result_table$p_value <- chi$p.value
    chiTable <- rbind(chiTable, result_table)
  }
  chiTable <<- as.data.frame(chiTable)
}

#chi_table(varlist_cat, dataset, outcome = "insulinRequire3")