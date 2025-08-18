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
# outcome - name of binary outcome variable, as found in dataset, must be of class numeric (1 = predicted outcome, 0 = reference outcome)
# saving_name (optional) - user-defined name of output table (Excell) & PDF
#                          If NULL uses default "'dataset'_univariate_continuous_models"

model_continuous <- function(
    varlist = varlist,
    dataset = dataset, 
    outcome = outcome,
    saving_name = NULL,
    complete_case = FALSE,
    thresholds = NULL, 
    plots = TRUE, 
    decimals = 2){
  
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
  library(qwraps2)
  
  #save names as vectors
  dataset_name <- deparse(substitute(dataset))
  name <- saving_name
  #Making needed extra tables
  if(!is.null(thresholds)){
    threshold_ref_table <- data.frame(variable = varlist, 
                                      threshold = thresholds)
    
    threshold_ref_table <- threshold_ref_table %>%
      separate_wider_regex(threshold, 
                           patterns = c(direction = "<|>", 
                                        threshold = "[0-9]+"))
    threshold_ref_table$threshold <- as.numeric(threshold_ref_table$threshold)
  }
  
  #make output data.frame
  uni_cont_table <- data.frame()
  #cycle through each numerical variable in 'varlist'
  for(var in varlist){
    #make dummy data.frame for each variable
    sum_table <- data.frame(variable = NA, 
                            n = NA,
                            Accuracy = NA, 
                            Sensitivity = NA, 
                            Specificity = NA, 
                            PPV = NA, 
                            NPV = NA)
    #assign numeric variable name to row
    sum_table$variable <- var
    #IF complete_case is FALSE
    if(complete_case == FALSE){
      dataset_cc <- dataset %>%
        drop_na(all_of(var))
    }
    else{
      dataset_cc <- dataset %>%
        drop_na(all_of(varlist))
    }
    groups <- unique(dataset_cc[[outcome]])
    groups <- groups[!groups == 'NA']
    #print this variable to screen (for debugging purposes)
    print(var)
    #run logistic regression of defined outcome using numeric variable
    model <- glm(formula(paste0(outcome, "~scale(as.numeric(", var, "))")), 
                 data = dataset_cc, 
                 family = binomial)
    #make new columns in dataset
    dataset_cc <- dataset_cc %>%
      mutate(
        #make predicted probability of model in dataset column 
        model_pp = predict(model, dataset_cc, type = "response"),
        #make log-odds of model in dataset column
        model_log_odds = predict(model, dataset_cc)
        )
    #Run ROC analysis
    roc_model <- roc(dataset_cc[[outcome]], 
                     dataset_cc[[var]], 
                     plot = TRUE, 
                     print.thres = "best", 
                     print.auc = TRUE, 
                     ci = TRUE)
    #Extract valuable summary information from ROC object
    if(is.null(thresholds)){
    model_pr <- coords(roc_model, 
                       x = "best", 
                       ret=c("threshold"), 
                       transpose = FALSE)
    
    #Assign model summary information to dummary data.frame
    threshold <- model_pr$threshold
    sum_table$Threshold <- model_pr$threshold
    conf_matrix <- confusion_matrix(dataset_cc[[outcome]], 
                                    dataset_cc[[var]], 
                                    thresholds = threshold)
    sens <- conf_matrix$cm_stats["sensitivity"]
    sens_lci <- conf_matrix$cm_stats["sensitivity_lcl"]
    sens_uci <- conf_matrix$cm_stats["sensitivity_ucl"]
    sum_table$Sensitivity <- paste0(round(sens[2,1],decimals),
                                    " (", round(sens_lci[2,1],decimals), "; ",
                                    round(sens_uci[2,1],decimals), ")")
    specif <- conf_matrix$cm_stats["specificity"]
    specif_lci <- conf_matrix$cm_stats["specificity_lcl"]
    specif_uci <- conf_matrix$cm_stats["specificity_ucl"]
    sum_table$Specificity <- paste0(round(specif[2,1],decimals), 
                                    " (", round(specif_lci[2,1],decimals), "; ", 
                                    round(specif_uci[2,1],decimals), ")")
    ppv <- conf_matrix$cm_stats["ppv"]
    ppv_lci <- conf_matrix$cm_stats["ppv_lcl"]
    ppv_uci <- conf_matrix$cm_stats["ppv_ucl"]
    sum_table$PPV <- paste0(round(ppv[2,1],decimals),
                               " (", round(ppv_lci[2,1],decimals), "; ",
                               round(ppv_uci[2,1],decimals), ")")
    npv <- conf_matrix$cm_stats["npv"]
    npv_lci <- conf_matrix$cm_stats["npv_lcl"]
    npv_uci <- conf_matrix$cm_stats["npv_ucl"]
    sum_table$NPV <- paste0(round(npv[2,1],decimals),
                               " (", round(npv_lci[2,1],decimals), "; ",
                               round(npv_uci[2,1],decimals), ")")
    accur <- conf_matrix$cm_stats["accuracy"]
    accur_lci <- conf_matrix$cm_stats["accuracy_lcl"]
    accur_uci <- conf_matrix$cm_stats["accuracy_ucl"]
    sum_table$Accuracy <- paste0(round(accur[2,1],decimals),
                                    " (", round(accur_lci[2,1],decimals), "; ",
                                    round(accur_uci[2,1],decimals), ")")
    auc_ci <-roc_model[["ci"]]
    sum_table$ROC_AUC <- paste0(round(roc_model$auc,decimals),
                                   " (", round(auc_ci[1],decimals), "; ",
                                   round(auc_ci[3],decimals), ")")

    } else{
      direction <- threshold_ref_table[threshold_ref_table$variable == var, 
                                         "direction"]
      threshold <- threshold_ref_table[threshold_ref_table$variable == var, 
                                         "threshold"]
      combo <- paste0("dataset_cc[['", var, "']]", " ",
                        direction, " ",
                        "as.numeric(",
                        threshold,
                        ")")
      print(combo)
      condition <- eval(parse(text = combo))
      dataset_cc$cut_threshold <- ifelse(condition,
                                         "1", 
                                         "0")
      sum_table$Threshold <- combo
      sens_spec <- prop.table(table(dataset_cc$cut_threshold, 
                                    dataset_cc[[outcome]]), 
                              margin = 2)
      sum_table$sensitivity <- sens_spec[str_detect(row.names(sens_spec), "1"), 
                        str_detect(colnames(sens_spec), "1")]
      sum_table$specificity <- sens_spec[str_detect(row.names(sens_spec), "0"),
                        str_detect(colnames(sens_spec), "0")]
      ppv_npv <- prop.table(table(dataset_cc$cut_threshold, 
                                    dataset_cc[[outcome]]), 
                              margin = 1)
      sum_table$PPV <- ppv_npv[str_detect(row.names(ppv_npv), "1"), 
                                         str_detect(colnames(ppv_npv), "1")]
      sum_table$NPV <- ppv_npv[str_detect(row.names(ppv_npv), "0"),
                                         str_detect(colnames(ppv_npv), "0")]
      accuracy_table <- table(dataset_cc$cut_threshold, 
                              dataset_cc[[outcome]])
      sum_table$Accuracy <- (accuracy_table[str_detect(row.names(accuracy_table), "1"), 
                                            str_detect(colnames(accuracy_table), "1")] + 
                               accuracy_table[str_detect(row.names(accuracy_table), "0"), 
                                              str_detect(colnames(accuracy_table), "0")])/nrow(dataset_cc)
      
    }
    
    sum_table$n <- nrow(dataset_cc)
    
    for (g in groups) {
      table_outcome_groups <- data.frame(n=NA)
      dataset1 <- data.frame(Outcome = dataset_cc[[outcome]]) 
      dataset1 <- as.data.frame(dataset1[dataset1$Outcome== g & !is.na(dataset1$Outcome),])
      table_outcome_groups$n <- nrow(dataset1)
      colnames(table_outcome_groups)[colnames(table_outcome_groups) == "n"] <- paste(g, "n")
      sum_table <- cbind(sum_table, table_outcome_groups)
      
    }
    sum_table$coefficient <- summary(model)$coefficients[2,1]
    sum_table$coef_2.5CI <- confint(model)[str_detect(row.names(confint(model)), var), 1]
    sum_table$coef_97.5CI <- confint(model)[str_detect(row.names(confint(model)), var), 2]
    sum_table$coef_ci <- paste0(round(sum_table$coefficient, decimals), 
                                " (", round(sum_table$coef_2.5CI, decimals), "; ", 
                               round(sum_table$coef_97.5CI, decimals), ")")
    sum_table$p_value <- coef(summary(model))[str_detect(row.names(coef(summary(model))), var), 4]
    sum_table$ROCAUC <- round(roc_model$auc,3)
    sum_table$ROC_CI <- paste0("(", round(roc_model$ci[1], 3), ";", round(roc_model$ci[3], 3), ")")
    sum_table$ROCAUC_CI <- paste(round(sum_table$ROCAUC, 3), sum_table$ROC_CI)
    sum_table$AIC <- model$aic
    
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
    if(plots == TRUE){
    plot_list = list()
    pdf(paste0(dataset_name,"_univariate_continuous_models_plots.pdf")) 
    for (var in varlist){
      #browser()
      #summary(dataset$v)
      var_name <- paste0(var)
      #IF complete_case is FALSE
      if(complete_case == FALSE){
        dataset_cc <- dataset %>%
          drop_na(all_of(var))
      }
      else{
        dataset_cc <- dataset %>%
          drop_na(all_of(varlist))
      }
      model <- glm(formula(paste0(outcome, "~scale(as.numeric(", var, "))")), 
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
    }
    
  } else {
    ## If user-defined saving name provided
    #Save table with provided name
    uni_cont_table <<- as.data.frame(uni_cont_table)
    write_xlsx(uni_cont_table, paste0(name,".xlsx"))
    mv(from= "uni_cont_table", to = paste0(name), envir = globalenv())
    #Produce pdf with provided name
    if(plots == TRUE){
    plot_list = list()
    pdf(paste0(name,".pdf")) 
    for (var in varlist){
      #browser()
      #summary(dataset$v)
      var_name <- paste0(var)
      #IF complete_case is FALSE
      if(complete_case == FALSE){
        dataset_cc <- dataset %>%
          drop_na(all_of(var))
      }
      else{
        dataset_cc <- dataset %>%
          drop_na(all_of(varlist))
      }
      model <- glm(formula(paste0(outcome, "~scale(as.numeric(", var, "))")), 
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
      
      #Proportion preparation
      ## split into deciles (hack to keep edge cases)
      brks_nm2 <- unique(quantile(dataset_cc[[var]], 
                                  probs = seq(0, 1, by = 0.1), 
                                  na.rm = TRUE))
      brks_nm2[1] <- 0.99 * brks_nm2[1]
      brks_nm2[length(brks_nm2)] <- 1.1 * brks_nm2[length(brks_nm2)]
      dec_nm2 <- cut(
        dataset_cc[[var]], 
        breaks = unique(brks_nm2),
        include_lowest = TRUE
      )
      
      dec_nm2 <- data.frame(y = dataset_cc[[outcome]], 
                            pred = dataset_cc[[var]], 
                            dec = dec_nm2) %>%
        group_by(dec) %>%
        mutate(prob_obs = sum(y) / n(), 
               obs = sum(y),
               n_group = n(),
               log_prob_obs = log(prob_obs/(1-prob_obs)),
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
            mapping = aes(x = 0.55, 
                          y = 0.1, 
                          label = auc_full, 
                          hjust = "center"),
            size = 7,
            label.r = unit(0, "pt"),
            label.padding=unit(0.4, "lines")
          ) +
          theme(
            panel.spacing.x = unit(1.5, "lines")
          ) +
          labs(title = paste0(var_name, " model plots")),
        #boxplot
        ggplot(dataset_cc, aes(x=as.character(.data[[outcome]]), 
                               y = pred_prob)) +
          geom_boxplot(),
        patchwork::wrap_plots(
          #Proportion to decile plot
          ggplot(dec_nm2, aes(x = mnpred, 
                              y = log_prob_obs)) +
            geom_point() +
            xlab(paste("Mean", var, "in each decile", sep = " ")) +
            ylab("log(p/(1-p))") +
            geom_abline(intercept = 0, 
                        slope = 1, 
                        linetype = "dashed"), #+
            #ylim(c(0, 1)) + xlim(c(0, 1)) +
            #geom_errorbar(aes(ymin = lower, ymax = upper)),
          #calibration plot
          ggplot(dec_nm1, aes(x = mnpred, y = prob_obs)) +
            geom_point() +
            xlab("Mean predicted probability in each decile") +
            ylab("Proportion of outcome \n in each decile") +
            geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
            ylim(c(0, 1)) + xlim(c(0, 1)) +
            geom_errorbar(aes(ymin = lower, ymax = upper))
          ),
        ncol = 1, nrow = 3
        )
      plot_list[[var]] = model_assess_plots
      }
    print(plot_list)
    dev.off()
    }
  }
}