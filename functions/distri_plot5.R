#Distribution plotting function for multiple continuous variables ##########################################################
##########################################################################################################
#dataset - the dataframe in-which the continous variables are
#vars - list of continous variable in the dataframe
#group - if present, the grouping (categorical) variable in the dataframe by which to plot

#dependencies on:
#library(tidyverse)

#########################################################################################################
distri_plot <- function(
    dataset = NULL, 
    vars = NULL, 
    pdf_name = NULL,
    group = NULL){
  
  #browser()
  #Checks for "dataset"
  if(missing(dataset) | is.null(dataset)) {stop("'dataset' needs to be provided.")}
  #Checks for "vars"
  if(missing(vars) | is.null(vars)) {stop("'vars' needs to be provided.")}
  
  # load libraries
  library(tidyverse)
  library(patchwork)
  library(gridExtra)
  
  
  #state that dataset is a dataframe
  dataset <- as.data.frame(dataset)
  if(!is.null(pdf_name)){
    name <- pdf_name  
  }
  
  if(is.null(group)){
  #describe that vars are a list of variables/columns in dataset/data
  #var <- as.list(vars)
  #for each variable in the list that is present in dataset/data produce summary and 4 plots
  plot_list = list()
  if(is.null(pdf_name)){
    pdf("varlist_plots.pdf")
  } else{
    pdf(paste0(name,".pdf"))
    }
  for (v in vars){
    #browser()
    #summary(dataset$v)
    name <- paste0(v)
    var_name_plot <- patchwork::wrap_plots(
      #visualising boxplot
      ggplot(dataset, aes(x= "", y=.data[[v]])) +
        geom_boxplot() +
        #geom_vline(xintercept = mean(.data[v], na.rm = TRUE), color="red", linetype="dashed") +
        stat_summary(fun=mean, 
                     geom="point", 
                     shape=20, 
                     size=5, 
                     color="red", 
                     fill="red") +
        labs(title="StartRight distributions", 
             font="bold") +
        theme_light(), 
      #produce density histogram
      ggplot(dataset, aes(x=.data[[v]])) + 
        geom_histogram(aes(y=after_stat(density)),
                       fill="bisque",
                       color="white",
                       alpha=0.7, 
                       binwidth = 2) + 
        geom_density() +
        geom_rug() +
        theme_light(), 
      #JUST DENSITY 
      ggplot(dataset, aes(x = .data[[v]])) + 
        geom_density(alpha = 0.5, 
                    fill = "bisque") +
        theme_light(),  
   
      #histogram
      ggplot(dataset, aes(x=.data[[v]])) + 
        geom_histogram(aes(y=after_stat(density)),
                       fill="bisque",
                       color="white",
                       alpha=0.7, 
                       binwidth = 2) +
        theme_light(),
      ncol = 2, nrow = 2
      )
    plot_list[[v]] = var_name_plot
    }
  print(plot_list)
  dev.off()
  }
  
  if(!is.null(group)){
    dataset <- as.data.frame(dataset)
    plot_list = list()
    if(is.null(pdf_name)){
      pdf("varlist_plots.pdf")
      } else{
        pdf(paste0(name,".pdf"))
        }
    for (v in vars){
      #browser()
      #summary(dataset$v)
      name <- paste0(v)
      dat1 <- dataset %>%
        summarise(
          mean = mean(v, na.rm = TRUE))
      var_name_plot <- patchwork::wrap_plots(
        #visualising boxplot
        ggplot(dataset, aes(x=.data[[group]], 
                            y=.data[[v]])) +
          geom_boxplot() +
          geom_abline(x = dat1$mean) +
          labs(title="StartRight distributions", 
               font="bold") +
          theme_light() , 
        #produce density histogram
        ggplot(dataset, aes(x=.data[[v]])) + 
          geom_histogram(aes(y=after_stat(density)),
                         fill="bisque",
                         color="white",
                         alpha=0.7, 
                         binwidth = 2) + 
          geom_density() +
          geom_rug() +
          theme_light() + 
          facet_wrap(vars(.data[[group]])), 
        #JUST DENSITY 
        ggplot(dataset, aes(x = .data[[v]])) + 
          geom_density(alpha = 0.5, 
                       fill = "bisque") + 
          facet_wrap(vars(.data[[group]])) +
          theme_light(),  
        #histogram
        ggplot(dataset, aes(x=.data[[v]])) + 
          geom_histogram(aes(y=after_stat(density)),
                         fill="bisque",
                         color="white",
                         alpha=0.7, 
                         binwidth = 2) + 
          facet_wrap(vars(.data[[group]])) +
          theme_light(),
        ncol = 2, nrow = 2
        )
      plot_list[[v]] = var_name_plot
      }
    print(plot_list)
    dev.off()
  }
}
    