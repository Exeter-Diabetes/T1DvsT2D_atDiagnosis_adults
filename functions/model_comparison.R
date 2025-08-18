#####################################################################################

#model_comparisons()


#produces a data.frame and Excell summary table of the variable list
#containing accuracy, sensitivity, specificity, PPV, NPV, variable coefficient,
#optimal variable threshold (at which previous values are calculated) and ROC AUC
#also produces PDF of individual ROCs 

########################################################################################
#Arguments:
# varlist (variable list) - string list of numeric variable names in dataset 
#                           (must be in interger or numeric class)
# base_model_vars - string list of numeric variables want in base model
# dataset - name of dataset where in variables in varlist, outcome & base_model_vars are found
# outcome - name of binary outcome variable, as found in dataset
# saving_name (optional) - user-defined name of output table (Excell) & PDF
#                          If NULL uses default "'dataset'_univariate_continuous_models"

model_comparisons <- function(
    varlist = varlist,
    base_model_vars = base_model_vars,
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
  ##Checks for "base_model_vars"
  if(missing(base_model_vars) | is.null(base_model_vars)) {stop("'base_model_vars' (list of numeric variable names in dataset that will be used in base model) needs to be provided.")}
  
  #Load relevant libraries
  library(tidyverse)
  library(pROC)
  library(writexl)
  library(gdata)
  library(epiDisplay)
  
  #save names as vectors
  dataset_name <- deparse(substitute(dataset))
  name <- saving_name
  all_vars <- c(varlist)
  
  #Make complete case dataset for all examined variables
  dataset_cc <- dataset %>%
    drop_na(all_of(varlist))
  
  #make output data.frame
  model_comp_table <- data.frame(model = NA,
                                 n = NA,
                                 coef_estimate = NA,
                                 coef_pvalue = NA,
                                 accuracy = NA,
                                 sensitivity = NA,
                                 specificity = NA,
                                 PPV = NA,
                                 NPV = NA,
                                 threshold = NA, 
                                 ROCAUC = NA,
                                 AIC = NA,
                                 lrtest = NA)
  
  new_order <- c("variable", "category")
  
  first_base_var <- base_model_vars[1]
  
  # Loop over each group to add columns in the "x" and "y" alternating pattern
  for(b in base_model_vars[-1]) {
    variable_order <- c(paste(first_base_var, "+", b, sep = " "))
    #print(g)
    #print(new_order)
  }
  
  print(variable_order)
  
  #make base model
  base_model <- glm(formula(paste0(outcome, "~", variable_order)), 
                    data = dataset_cc, 
                    family = binomial)
  
  dataset_cc <- dataset_cc %>%
    mutate(
      #make predicted probability of model in dataset column 
      model_pp = predict(base_model, dataset_cc, type = "response"),
      #make log-odds of model in dataset column
      model_log_odds = predict(base_model, dataset_cc)
    )
  #Run ROC analysis
  roc_base_model <- roc(dataset_cc[[outcome]], 
                        dataset_cc$model_pp, 
                   plot = TRUE, 
                   print.thres = "best", 
                   print.auc = TRUE)
  #Extract valuable summary information from ROC object
  base_model_pr <- coords(roc_base_model, 
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
  model_comp_table$model <- variable_order
  model_comp_table$n <- nrow(dataset_cc)
  #model_comp_table$model <- "R"
  model_comp_table$accuracy <- base_model_pr$accuracy
  model_comp_table$sensitivity <- base_model_pr$sensitivity
  model_comp_table$specificity <- base_model_pr$specificity
  model_comp_table$PPV <- base_model_pr$ppv
  model_comp_table$NPV <- base_model_pr$npv
  #sum_table$coefficient <- summary(base_model)$coefficients[2,1]
  model_comp_table$threshold <- base_model_pr$threshold
  model_comp_table$ROCAUC <- roc_base_model$auc
  model_comp_table$AIC <- base_model$aic
  
  short_varlist <-varlist[!varlist %in% base_model_vars]
  
  for(var in short_varlist){
    # Loop over each group to add columns in the "x" and "y" alternating pattern
      variable_order1 <- paste(variable_order, "+", var, sep = " ")
    
    print(variable_order1)
    
    #make dummy data.frame for each variable
    sum_table <- data.frame(model = NA, 
                            n = NA,
                            coef_estimate = NA,
                            coef_pvalue = NA,
                            accuracy = NA, 
                            sensitivity = NA, 
                            specificity = NA, 
                            PPV = NA, 
                            NPV = NA,
                            threshold = NA, 
                            ROCAUC = NA,
                            AIC = NA,
                            lrtest = NA)
    #assign numeric variable name to row
    sum_table$model <- deparse(substitute(variable_order1))
    
    #make base model
    model <- glm(formula(paste0(outcome, "~", variable_order1)), 
                      data = dataset_cc, 
                      family = binomial)
    
    dataset_cc <- dataset_cc %>%
      mutate(
        #make predicted probability of model in dataset column 
        model_pp = predict(model, dataset_cc, type = "response"),
        #make log-odds of model in dataset column
        model_log_odds = predict(model, dataset_cc)
      )
    #Run ROC analysis
    roc_model <- roc(dataset_cc[[outcome]],
                     dataset_cc$model_pp,
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
    
    likely_test <- lrtest(base_model, model)
    
    #Assign model summary information to dummary data.frame
    #model_comp_table$model <- variable_order
    #model_comp_table$model <- "R"
    sum_table$n <- nrow(dataset_cc)
    sum_table$coef_estimate <- coef(summary(model))[row.names(coef(summary(model))) %in% var, 1]
    sum_table$coef_pvalue <- coef(summary(model))[row.names(coef(summary(model))) %in% var, 4]
    sum_table$accuracy <- model_pr$accuracy
    sum_table$sensitivity <- model_pr$sensitivity
    sum_table$specificity <- model_pr$specificity
    sum_table$PPV <- model_pr$ppv
    sum_table$NPV <- model_pr$npv
    #sum_table$coefficient <- summary(base_model)$coefficients[2,1]
    sum_table$threshold <- model_pr$threshold
    sum_table$ROCAUC <- roc_model$auc
    sum_table$AIC <- model$aic
    sum_table$lrtest <- likely_test$p.value
    
    model_comp_table <- rbind(model_comp_table, sum_table)
  }
  #Save output data.frame to global environment
  ## If user-defined saving name not provided
  if(is.null(saving_name)){
    #Save table with default name
    model_comp_table <<- as.data.frame(model_comp_table)
    mv(from= "model_comp_table", to = paste0(dataset_name, "_univariate_model_comparisons_table"), envir = globalenv())
    write_xlsx(model_comp_table, paste0(dataset_name,"_univariate_model_comparisons_table.xlsx"))
    #Produce PDF with default name
    plot_list = list()
    pdf(paste0(dataset_name,"_univariate_model_comparisons_plots.pdf")) 
    for (var in short_varlist){
      #browser()
      #summary(dataset$v)
      var_name <- paste0(var)
      # Loop over each group to add columns in the "x" and "y" alternating pattern
      variable_order1 <- paste(variable_order, "+", var, sep = " ")
      
      print(variable_order1)
      #make base model
      model <- glm(formula(paste0(outcome, "~", variable_order1)), 
                   data = dataset_cc, 
                   family = binomial)
      
      dataset_cc <- dataset_cc %>%
        mutate(pred_prob = predict(model, dataset_cc, type = "response"))
      pred_prob <- predict(model, dataset_cc, type = "response")
      model_outcome <- dataset_cc[[outcome]]
      #browser()
      #ROC curve preparation
      roc_curves <- data.frame(prob = pred_prob) %>%
        cbind(Outcome = dataset_cc[[outcome]]) %>%
        pROC::roc(response = Outcome, predictor = prob) %>%
        magrittr::extract(2:4) %>%
        as.data.frame() %>%
        mutate(
          auc =  unname(data.frame(prob = pred_prob) %>%
                          cbind(Outcome = dataset_cc[[outcome]]) %>%
                          pROC::roc(response = Outcome, predictor = prob) %>%
                          magrittr::extract(c(9)) %>%
                          unlist())
        ) 
      
      dat_text <- roc_curves %>%
        dplyr::select(-sensitivities, -specificities) %>%
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
      
      dec_nm1 <- data.frame(y = dataset_cc[[outcome]], pred = pred_prob, dec = dec_nm1) %>%
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
        ggplot(dataset_cc, aes(x=as.character(.data[[outcome]]), y = pred_prob)) +
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
    model_comp_table <<- as.data.frame(model_comp_table)
    write_xlsx(model_comp_table, paste0(name,".xlsx"))
    mv(from= "model_comp_table", to = paste0(name), envir = globalenv())
    #Produce pdf with provided name
    plot_list = list()
    pdf(paste0(name,".pdf")) 
    for (var in varlist){
      #browser()
      #summary(dataset$v)
      var_name <- paste0(var)
      # Loop over each group to add columns in the "x" and "y" alternating pattern
      variable_order1 <- paste(variable_order, "+", var, sep = " ")
      
      print(variable_order1)
      #make base model
      model <- glm(formula(paste0(outcome, "~", variable_order1)), 
                   data = dataset_cc, 
                   family = binomial)
      dataset_cc <- dataset_cc %>%
        mutate(pred_prob = predict(model, dataset_cc, type = "response"))
      pred_prob <- predict(model, dataset_cc, type = "response")
      model_outcome <- dataset_cc[[outcome]]
      #browser()
      #ROC curve preparation
      roc_curves <- data.frame(prob = pred_prob) %>%
        cbind(Outcome = dataset_cc[[outcome]]) %>%
        pROC::roc(response = Outcome, predictor = prob) %>%
        magrittr::extract(2:4) %>%
        as.data.frame() %>%
        mutate(
          auc =  unname(data.frame(prob = pred_prob) %>%
                          cbind(Outcome = dataset_cc[[outcome]]) %>%
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
      
      dec_nm1 <- data.frame(y = dataset_cc[[outcome]], pred = pred_prob, dec = dec_nm1) %>%
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
        ggplot(dataset_cc, aes(x=as.character(.data[[outcome]]), y = pred_prob)) +
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
  
  #model_comp_table <<- as.data.frame(model_comp_table)
}
  
  
  