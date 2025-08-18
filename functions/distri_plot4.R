#Distribution plotting function for multiple continuous variables ##########################################################
##########################################################################################################
#dataset - the dataframe in-which the continous variables are
#vars - list of continous variable in the dataframe
#group - if present, the grouping (categorical) variable in the dataframe by which to plot

#dependencies on:
#library(tidyverse)

#########################################################################################################
distri_plot <- function(dataset = NULL, vars = NULL, pdf_name = NULL,group = NULL){
  #browser()
  #Checks for "dataset"
  if(missing(dataset) | is.null(dataset)) {stop("'dataset' needs to be provided.")}
  #Checks for "vars"
  if(missing(vars) | is.null(vars)) {stop("'vars' needs to be provided.")}
  
  # load libraries
  library(tidyverse)
  library(patchwork)
  library(gridExtra)
  
  if(is.null(pdf_name)){
  
  #state that dataset is a dataframe
  dataset <- as.data.frame(dataset)
  #describe that vars are a list of variables/columns in dataset/data
  #var <- as.list(vars)
  #for each variable in the list that is present in dataset/data produce summary and 4 plots
  plot_list = list()
  pdf("varlist_plots.pdf")
  for (v in vars){
    #browser()
    #summary(dataset$v)
    name <- paste0(v)
    var_name_plot <- patchwork::wrap_plots(
      #visualising boxplot
      ggplot(dataset, aes(x=.data[[v]])) +
        geom_boxplot() +
        labs(title="StartRight distributions", 
             font="bold") , 
      #produce density histogram
     ggplot(dataset, aes(x=.data[[v]])) + 
        geom_histogram(aes(y=after_stat(density)),fill="bisque",color="white",alpha=0.7, binwidth = 2) + 
        geom_density() +
        geom_rug() +
        theme_minimal(), 
      #JUST DENSITY 
      ggplot(dataset, aes(x = .data[[v]])) + 
        geom_density(alpha = 0.5, fill = "bisque"),  
   
      #histogram
       ggplot(dataset, aes(x=.data[[v]])) + 
       geom_histogram(aes(y=after_stat(density)),fill="bisque",color="white",alpha=0.7, binwidth = 2),
     ncol = 2, nrow = 2
     )
    plot_list[[v]] = var_name_plot
    
  }
  
  print(plot_list)
  
  dev.off()
  
  }
  if(!is.null(pdf_name)){
    name <- pdf_name
    #state that dataset is a dataframe
    dataset <- as.data.frame(dataset)
    #describe that vars are a list of variables/columns in dataset/data
    #var <- as.list(vars)
    #for each variable in the list that is present in dataset/data produce summary and 4 plots
    plot_list = list()
    pdf(paste0(name,".pdf"))
    for (v in vars){
      #browser()
      #summary(dataset$v)
      name <- paste0(v)
      var_name_plot <- patchwork::wrap_plots(
        #visualising boxplot
        ggplot(dataset, aes(x=.data[[v]])) +
          geom_boxplot() +
          labs(title="StartRight distributions", 
               font="bold") , 
        #produce density histogram
        ggplot(dataset, aes(x=.data[[v]])) + 
          geom_histogram(aes(y=after_stat(density)),fill="bisque",color="white",alpha=0.7, binwidth = 2) + 
          geom_density() +
          geom_rug() +
          theme_minimal(), 
        #JUST DENSITY 
        ggplot(dataset, aes(x = .data[[v]])) + 
          geom_density(alpha = 0.5, fill = "bisque"),  
        
        #histogram
        ggplot(dataset, aes(x=.data[[v]])) + 
          geom_histogram(aes(y=after_stat(density)),fill="bisque",color="white",alpha=0.7, binwidth = 2),
        ncol = 2, nrow = 2
      )
      plot_list[[v]] = var_name_plot
      
    }
    
    print(plot_list)
    
    dev.off()
    
  }
  }