#####################################################################################

#model_continuous()

#function that takes in list of continuous variables and an binary outcome
#and runs a logistic regression model for each continuous variable individually
#produces a data.frame and Excell summary table of the variable list
#containing accuracy, sensitivity, specificity, PPV, NPV, variable coefficient,
#optimal variable threshold (at which previous values are calculated) and ROC AUC
#also produces PDF of individual ROCs 

########################################################################################
#Arguments:
# varlist (variable list) - string list of numeric variable names in dataset 
#                           (must be in interger or numeric class)
# dataset - name of dataset where in variables in varlist and varlist_cat are found
# outcome - name of binary outcome variable, as found in dataset
# saving_name (optional) - user-defined name of output table (Excell) & PDF
#                          If NULL uses default "'dataset'_univariate_continuous_models"

model_continuous <- function(
    varlist = varlist,
    dataset = dataset, 
    outcome = outcome,
    saving_name = NULL){
  
  #browser()
  
  #Argument checks 
  ##Checks for "dataset"
  if(missing(dataset) | is.null(dataset)) {stop("'dataset' needs to be provided.")}
  ##Checks for "varlist"
  if(missing(varlist) | is.null(varlist)) {stop("'varlist' (list of numeric variable names in dataset) needs to be provided.")}
  ##Checks for "dataset"
  if(missing(outcome) | is.null(outcome)) {stop("'outcome' (binary outcome variable in dataset) needs to be provided.")}
  
  #Load relevant libraries
  library(tidyverse)
  library(pROC)
  library(writexl)
  library(gdata)
  
  #save names as vectors
  dataset_name <- deparse(substitute(dataset))
  name <- saving_name
  
  #make output data.frame
  uni_cont_table <- data.frame()
  #cycle through each numerical variable in 'varlist'
  for(var in varlist){
    #make dummy data.frame for each variable
    sum_table <- data.frame(variable = NA, 
                            accuracy = NA, 
                            sensitivity = NA, 
                            specificity = NA, 
                            PPV = NA, 
                            NPV = NA)
    #assign numeric variable name to row
    sum_table$variable <- var
    #print this variable to screen (for debugging purposes)
    print(var)
    #run logistic regression of defined outcome using numeric variable
    model <- glm(formula(paste0(outcome, "~as.numeric(", var, ")")), 
                 data = dataset, 
                 family = binomial)
    #make new columns in dataset
    dataset <- dataset %>%
      mutate(
        #make predicted probability of model in dataset column 
        model_pp = predict(model, dataset, type = "response"),
        #make log-odds of model in dataset column
        model_log_odds = predict(model, dataset)
        )
    #Run ROC analysis
    roc_model <- roc(dataset[[outcome]], 
                     dataset[[var]], 
                     plot = TRUE, 
                     print.thres = "best", 
                     print.auc = TRUE)
    #Extract valuable summary information from ROC object
    model_pr <- coords(roc_model, 
                       x = "best", 
                       ret=c("threshold", 
                             "specificity", 
                             "sensitivity", 
                             "accuracy", 
                             "precision", 
                             "recall", 
                             "ppv", 
                             "npv"), 
                       transpose = FALSE)
    
    #Assign model summary information to dummary data.frame
    sum_table$accuracy <- model_pr$accuracy
    sum_table$sensitivity <- model_pr$sensitivity
    sum_table$specificity <- model_pr$specificity
    sum_table$PPV <- model_pr$ppv
    sum_table$NPV <- model_pr$npv
    sum_table$coefficient <- summary(model)$coefficients[2,1]
    sum_table$threshold <- model_pr$threshold
    sum_table$ROCAUC <- roc_model$auc
    
    #Bind this summary information row to the output data.frame
    uni_cont_table <- rbind(uni_cont_table, sum_table)
    #Print ROC curve
    print(roc_model)
    uni_cont_table <- uni_cont_table %>%
      arrange(desc(ROCAUC))
  }
  #Save output data.frame to global environment
  ## If user-defined saving name not provided
  if(is.null(saving_name)){
    #Save table with default name
    uni_cont_table <<- as.data.frame(uni_cont_table)
    mv(from= "uni_cont_table", to = paste0(dataset_name, "univariate_continuous_models_table"), envir = globalenv())
    write_xlsx(uni_cont_table, paste0(dataset_name,"univariate_continuous_models_table.xlsx"))
    #Produce PDF with default name
    plot_list = list()
    pdf(paste0(dataset_name,"_univariate_continuous_models_plots.pdf")) 
    for (var in varlist){
      #browser()
      #summary(dataset$v)
      var_name <- paste0(var)
      model <- glm(formula(paste0(outcome, "~as.numeric(", var, ")")), 
                   data = dataset, 
                   family = binomial)
      dataset <- dataset %>%
        mutate(pred_prob = predict(model, dataset, type = "response"))
      pred_prob <- predict(model, dataset, type = "response")
      model_outcome <- dataset[[outcome]]
      #browser()
      #ROC curve preparation
      roc_curves <- data.frame(prob = pred_prob) %>%
        cbind(Outcome = dataset[[outcome]]) %>%
        pROC::roc(response = Outcome, predictor = prob) %>%
        magrittr::extract(2:4) %>%
        as.data.frame() %>%
        mutate(
          auc =  unname(data.frame(prob = pred_prob) %>%
                          cbind(Outcome = dataset[[outcome]]) %>%
                          pROC::roc(response = Outcome, predictor = prob) %>%
                          magrittr::extract(c(9)) %>%
                          unlist())
        ) 
      
      dat_text <- roc_curves %>%
        select(-sensitivities, -specificities) %>%
        distinct() %>%
        mutate(
          auc_full = paste0("AUC: ", signif(auc, 2))
        )
      
      
      #MODEL CALIBRATION preparation
      ## split into deciles (hack to keep edge cases)
      brks_nm1 <- unique(quantile(pred_prob, probs = seq(0, 1, by = 0.1), na.rm = TRUE))
      brks_nm1[1] <- 0.99 * brks_nm1[1]
      brks_nm1[length(brks_nm1)] <- 1.1 * brks_nm1[length(brks_nm1)]
      dec_nm1 <- cut(
        pred_prob, 
        breaks = unique(brks_nm1),
        include_lowest = TRUE
      )
      
      dec_nm1 <- data.frame(y = dataset[[outcome]], pred = pred_prob, dec = dec_nm1) %>%
        group_by(dec) %>%
        mutate(prob_obs = sum(y) / n(), 
               obs = sum(y),
               n_group = n(),
               mnpred = mean(pred),
               lower = lapply(sum(y), prop.test, n = n()), 
               upper = sapply(lower, function(x) x$conf.int[2]), 
               lower = sapply(lower, function(x) x$conf.int[1]))
    #Make plot object to print to individual PDF page  
    model_assess_plots <- patchwork::wrap_plots(
      #roc_plot
      roc_curves %>%
        ggplot(aes(x = 1- specificities, y = sensitivities)) +
        geom_path() +
        theme_bw() +
        scale_y_continuous("Sensitivity", labels = scales::percent) +
        scale_x_continuous("1- Specificity", labels = scales::percent) +
        theme_bw() +
        geom_label(
          data = dat_text,
          mapping = aes(x = 0.55, y = 0.1, label = auc_full, hjust = "center"),
          size = 7,
          label.r = unit(0, "pt"),
          label.padding=unit(0.4, "lines")
        ) +
        theme(
          panel.spacing.x = unit(1.5, "lines")
        ) +
        labs(title = paste0(var_name, " model plots")),
      #boxplot
      ggplot(dataset, aes(x=as.character(.data[[outcome]]), y = pred_prob)) +
        geom_boxplot(),
      #calibration plot
      ggplot(dec_nm1, aes(x = mnpred, y = prob_obs)) +
        geom_point() +
        xlab("Mean predicted probability in each decile") +
        ylab("Proportion of outcome \n in each decile") +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
        ylim(c(0, 1)) + xlim(c(0, 1)) +
        geom_errorbar(aes(ymin = lower, ymax = upper)),
      ncol = 1, nrow = 3
    )
    plot_list[[var]] = model_assess_plots
    }
    
    print(plot_list)
    dev.off()
    
  } else {
    ## If user-defined saving name provided
    #Save table with provided name
    uni_cont_table <<- as.data.frame(uni_cont_table)
    write_xlsx(uni_cont_table, paste0(name,".xlsx"))
    mv(from= "uni_cont_table", to = paste0(name), envir = globalenv())
    #Produce pdf with provided name
    plot_list = list()
    pdf(paste0(name,".pdf")) 
    for (var in varlist){
      #browser()
      #summary(dataset$v)
      var_name <- paste0(var)
      model <- glm(formula(paste0(outcome, "~as.numeric(", var, ")")), 
                   data = dataset, 
                   family = binomial)
      dataset <- dataset %>%
        mutate(pred_prob = predict(model, dataset, type = "response"))
      pred_prob <- predict(model, dataset, type = "response")
      model_outcome <- dataset[[outcome]]
      #browser()
      #ROC curve preparation
      roc_curves <- data.frame(prob = pred_prob) %>%
        cbind(Outcome = dataset[[outcome]]) %>%
        pROC::roc(response = Outcome, predictor = prob) %>%
        magrittr::extract(2:4) %>%
        as.data.frame() %>%
        mutate(
          auc =  unname(data.frame(prob = pred_prob) %>%
                          cbind(Outcome = dataset[[outcome]]) %>%
                          pROC::roc(response = Outcome, predictor = prob) %>%
                          magrittr::extract(c(9)) %>%
                          unlist())
        ) 
      
      dat_text <- roc_curves %>%
        select(-sensitivities, -specificities) %>%
        distinct() %>%
        mutate(
          auc_full = paste0("AUC: ", signif(auc, 2))
        )
      
      
      #MODEL CALIBRATION preparation
      ## split into deciles (hack to keep edge cases)
      brks_nm1 <- unique(quantile(pred_prob, probs = seq(0, 1, by = 0.1), na.rm = TRUE))
      brks_nm1[1] <- 0.99 * brks_nm1[1]
      brks_nm1[length(brks_nm1)] <- 1.1 * brks_nm1[length(brks_nm1)]
      dec_nm1 <- cut(
        pred_prob, 
        breaks = unique(brks_nm1),
        include_lowest = TRUE
      )
      
      dec_nm1 <- data.frame(y = dataset[[outcome]], pred = pred_prob, dec = dec_nm1) %>%
        group_by(dec) %>%
        mutate(prob_obs = sum(y) / n(), 
               obs = sum(y),
               n_group = n(),
               mnpred = mean(pred),
               lower = lapply(sum(y), prop.test, n = n()), 
               upper = sapply(lower, function(x) x$conf.int[2]), 
               lower = sapply(lower, function(x) x$conf.int[1]))
      #Make plot object to print to individual PDF page  
      model_assess_plots <- patchwork::wrap_plots(
        #roc_plot
        roc_curves %>%
          ggplot(aes(x = 1- specificities, y = sensitivities)) +
          geom_path() +
          theme_bw() +
          scale_y_continuous("Sensitivity", labels = scales::percent) +
          scale_x_continuous("1- Specificity", labels = scales::percent) +
          theme_bw() +
          geom_label(
            data = dat_text,
            mapping = aes(x = 0.55, y = 0.1, label = auc_full, hjust = "center"),
            size = 7,
            label.r = unit(0, "pt"),
            label.padding=unit(0.4, "lines")
          ) +
          theme(
            panel.spacing.x = unit(1.5, "lines")
          ) +
          labs(title = paste0(var_name, " model plots")),
        #boxplot
        ggplot(dataset, aes(x=as.character(.data[[outcome]]), y = pred_prob)) +
          geom_boxplot(),
        #calibration plot
        ggplot(dec_nm1, aes(x = mnpred, y = prob_obs)) +
          geom_point() +
          xlab("Mean predicted probability in each decile") +
          ylab("Proportion of outcome \n in each decile") +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
          ylim(c(0, 1)) + xlim(c(0, 1)) +
          geom_errorbar(aes(ymin = lower, ymax = upper)),
        ncol = 1, nrow = 3
      )
      plot_list[[var]] = model_assess_plots
    }
    
    print(plot_list)
    dev.off()
  }
}