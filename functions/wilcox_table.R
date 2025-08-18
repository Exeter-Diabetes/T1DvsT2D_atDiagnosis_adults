wilcox_table <- function(varlist = varlist, dataset = dataset, outcome = outcome){
  WilTable <- data.frame()
  #outcome
  #browser()
  for (var in varlist){
    dataset <- dataset %>%
      filter(!is.na(outcome))
    print(var)
    dataset[[var]] <- as.numeric(dataset[[var]])
    dataset[[outcome]] <- factor(dataset[[outcome]])
    #WIL <- wilcox.test(dataset[[var]], dataset[[outcome]], paired = FALSE)
    WIL <- wilcox.test(formula(paste0(var, "~", outcome)), data = dataset, exact = FALSE)
    result_table <- data.frame(variable = NA, p_value = NA)
    result_table$variable <- var
    result_table$p_value <- WIL$p.value
    WilTable <- rbind(WilTable, result_table)
  }
  WilTable <<- as.data.frame(WilTable)
}
