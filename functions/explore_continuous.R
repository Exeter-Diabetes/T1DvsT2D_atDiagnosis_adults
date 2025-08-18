#######################################################################################

#explore_continuous

#This function allows you to explore the distributions of continuous variables
# in a dataset

#In particular:
# Per variable:
#   boxplots
#   5/6 number summary
#   density histograms
#In dataset:
# Correlation structure between variables

#####################################################################################

explore_continuous(
  dataset = dataset, 
  varlist = varlist,
  outcome = NULL,
  output_name = NULL,
  correlation = TRUE
){
  #browser()
  #Argument checks 
  ##Checks for "dataset"
  if(missing(dataset) | is.null(dataset)) {stop("'dataset' needs to be provided.")}
  ##Checks for "varlist"
  if(missing(varlist) | is.null(varlist)) {stop("'varlist_cat' (list of continuous variable names in dataset) needs to be provided.")}
  ##Checks for "dataset"
  #if(missing(outcome) | is.null(outcome)) {stop("'outcome' (binary outcome variable in dataset) needs to be provided.")}
  
  #Load relevant libraries
  library(tidyverse)
  library(writexl)
  
  #save names as vectors
  dataset_name <- deparse(substitute(dataset))
  name <- output_name
  #Save table with default name
  uni_cont_table <<- as.data.frame(uni_cont_table)
  mv(from= "uni_cont_table", to = paste0(dataset_name, "univariate_continuous_models_table"), envir = globalenv())
  write_xlsx(uni_cont_table, paste0(dataset_name,"univariate_continuous_models_table.xlsx"))
  
  uni_cont_table <<- as.data.frame(uni_cont_table)
  write_xlsx(uni_cont_table, paste0(name,".xlsx"))
  mv(from= "uni_cont_table", to = paste0(name), envir = globalenv())
  
  #For plots 
  plot_list = list()
  
  if(is.null(output_name)){
    pdf(paste0(dataset_name,"_univariate_continuous_models_plots.pdf")) 
  } else{
    pdf(paste0(name,".pdf"))   
    }
  for (var in varlist){
    var_name <- paste0(var)
    #Make plot object to print to individual PDF page  
    model_assess_plots <- patchwork::wrap_plots(
      #roc_plot
      #boxplot
      ggplot(dataset_cc, aes(x=as.character(.data[[outcome]]), y = pred_prob)) +
        geom_boxplot(),
      #calibration plot
      )
    plot_list[[var]] = model_assess_plots
    }
  print(plot_list)
  dev.off()
}
