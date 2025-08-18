#####################################################################################

#cat_contingency()

#function that takes in list of categorical variables and an binary outcome
#and runs a logistic regression model for each categorical variable individually
#produces a data.frame and Excell summary table of the variable list
#containing accuracy, sensitivity, specificity, PPV, NPV, variable coefficient,
#optimal variable threshold (at which previous values are calculated) and ROC AUC
#also produces PDF of individual ROCs 

########################################################################################
#Arguments:
# varlist_cat (variable list) - string list of categorical variable names in dataset 
#                           (must be in character or factor class), currently for only 2 categories per variable
# dataset - name of dataset where in variables in outcome and varlist_cat are found
# outcome - name of binary outcome variable, as found in dataset
# saving_name (optional) - user-defined name of output table (Excell) & PDF
#                          If NULL uses default "'dataset'_univariate_categorical_models"


cat_contingency <- function(
    varlist_cat = varlist_cat, 
    dataset = dataset, 
    outcome = NULL,
    saving_name = NULL,
    complete_case = FALSE, 
    plots = TRUE, 
    decimals = 2){
  
  #browser()
  #Argument checks 
  ##Checks for "dataset"
  if(missing(dataset) | is.null(dataset)) {stop("'dataset' needs to be provided.")}
  ##Checks for "varlist"
  if(missing(varlist_cat) | is.null(varlist_cat)) {stop("'varlist_cat' (list of categorical variable names in dataset) needs to be provided.")}
  ##Checks for "dataset"
  if(missing(outcome) | is.null(outcome)) {stop("'outcome' (binary outcome variable in dataset) needs to be provided.")}
  
  #Load relevant libraries
  library(tidyverse)
  library(pROC)
  library(writexl)
  library(gdata)
  library(gt)
  library(gtsummary)
  library(png)
  library(qwraps2)
  
  #browser()
  #save names as vectors
  dataset_name <- deparse(substitute(dataset))
  name <- saving_name
  
  
  #Load nb functions
  gt_grob <- function(gt_object, ...){
    out_name <- file.path(
      tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".png")
      )
    gtsave(gt_object, out_name, ...)
    in_png <- png::readPNG(out_name)
    on.exit(file.remove(out_name), add=TRUE)
    grid::rasterGrob(in_png)
  }
  
  #If outcome is specified
  if(!is.null(outcome)){
    #Make output data.frame
    uni_cat_table <- data.frame()
    uni_cat_coeffs <- data.frame()
    #cycle through each categorical variable in 'varlist_cat'
    for(var in varlist_cat){
      #Make dummy data.frame for summary information of model per var
      sum_table <- data.frame(variable = NA, 
                              n = NA,
                              Accuracy = NA, 
                              Sensitivity = NA, 
                              Specificity = NA, 
                              PPV = NA, 
                              NPV = NA)
      #Assign var name to row
      sum_table$variable <- var
      #IF complete_case is FALSE
      if(complete_case == FALSE){
        dataset_cc <- dataset %>%
          drop_na(all_of(var))
      }
      #If want complete case for all of varlist_cat
      else{
        dataset_cc <- dataset %>%
          drop_na(all_of(varlist_cat))
      }
      groups <- unique(dataset_cc[[outcome]])
      groups <- groups[!groups == 'NA']
      
      #Train logistic regression model using categorical var on binary outcome
      model <- glm(formula(paste0(outcome, "~factor(", var, ")")), 
                   data = dataset_cc, 
                   family = binomial)
      #Make new columns of model predictions
      dataset_cc <- dataset_cc %>%
        mutate(
          #Make column for predicted probabilities of new model
          model_pp = predict(model, dataset_cc, type = "response"),
          #Make column for log-odds of new model
          model_log_odds = predict(model, dataset_cc))
      #get model coefficients
      coef_table <- as.data.frame(model$coefficient[2])
      coef_table$variable <- var
      coef_table$category <- as.character(rownames(coef_table))
      
      coef_table$p_value <- coef(summary(model))[str_detect(row.names(coef(summary(model))), var), 4]
      coef_table <- coef_table %>%
        rename(
          coefficients = `model$coefficient[2]`
          )
      coef_table$coef_2.5CI <- confint(model)[str_detect(row.names(confint(model)), var), 1]
      coef_table$coef_97.5CI <- confint(model)[str_detect(row.names(confint(model)), var), 2]
      coef_table$coef_ci <- paste0(round(coef_table$coefficients, decimals), " (", round(coef_table$coef_2.5CI, decimals), "; ", 
                                   round(coef_table$coef_97.5CI, decimals), ")")
      #Run ROC analysis
      
      
      #Assign relevant model summary information to dummy table row
      sum_table$n <- nrow(dataset_cc)
      conf_matrix <- confusion_matrix(dataset_cc[[outcome]], 
                                      dataset_cc[[var]], 
                                      thresholds = 1)
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
      auc_ci <-conf_matrix[["auroc_ci"]]
      sum_table$ROC_AUC <- paste0(round(conf_matrix[["auroc"]],decimals),
                                     " (", round(auc_ci[1],decimals), "; ",
                                     round(auc_ci[2],decimals), ")")
      sum_table$ROCAUC <- round(conf_matrix[["auroc"]],decimals)
      # sum_table$accuracy <- model_pr$accuracy
      # sum_table$sensitivity <- model_pr$sensitivity
      # sum_table$specificity <- model_pr$specificity
      
      for (g in groups) {
        #browser()
        table_outcome_groups <- data.frame(n=NA)
        dataset1 <- data.frame(Outcome = dataset_cc[[outcome]]) 
        dataset1 <- as.data.frame(dataset1[dataset1$Outcome== g & !is.na(dataset1$Outcome),])
        table_outcome_groups$n <- nrow(dataset1)
        colnames(table_outcome_groups)[colnames(table_outcome_groups) == "n"] <- paste(g, "n")
        #table_outcome_groups$mean <- mean(as.numeric(dataset1$prob))
        #colnames(table_outcome_groups)[colnames(table_outcome_groups) == "mean"] <- paste(g, "mean")
        sum_table <- cbind(sum_table, table_outcome_groups)
        
      }

      
      #Join var row to output data.frame
      uni_cat_table <- rbind(uni_cat_table, sum_table)
      uni_cat_coeffs <- rbind(uni_cat_coeffs, coef_table)
      
      #Print ROC plot
      #print(roc_model)
      
      
      
      if(complete_case == TRUE){
        uni_cat_table <- uni_cat_table %>%
          arrange(desc(ROCAUC))
      }
    }
    
    uni_cat_table <- full_join(uni_cat_table, uni_cat_coeffs)
    uni_cat_coeffs <<- as.data.frame(uni_cat_coeffs)
    uni_cat_table <<- as.data.frame(uni_cat_table)
    
    #Save output data.frame to global environment
    ## If user-defined saving name not provided
    if(is.null(saving_name)){
      #Save table with default name
      mv(from= "uni_cat_coeffs", 
         to = paste0(dataset_name, "categorical_models_coefficients_table"), 
         envir = globalenv())
      write_xlsx(uni_cat_coeffs, 
                 paste0(dataset_name,
                        "categorical_models_coefficients_table.xlsx"))
      mv(from= "uni_cat_table", 
         to = paste0(dataset_name, "univariate_categorical_models_table"), 
         envir = globalenv())
      write_xlsx(uni_cat_table, 
                 paste0(dataset_name,
                        "univariate_categorical_models_table.xlsx"))
      } 
    #if user-defined name specified
    else{
      #save excell tables with user-defined name
      mv(from= "uni_cat_coeffs", 
         to = paste0(name, "_coefficients"), 
         envir = globalenv())
      write_xlsx(uni_cat_coeffs, 
                 paste0(name,"_coefficients.xlsx"))
      write_xlsx(uni_cat_table, 
                 paste0(name,".xlsx"))
      mv(from= "uni_cat_table", 
         to = paste0(name), 
         envir = globalenv())
      }
    #Produce PDF with default name
    if(plots == TRUE){
      plot_list = list()
      if(is.null(saving_name)){
        pdf(paste0(dataset_name,"_univariate_categorical_models_plots.pdf"))
        }
      #if user-defined name specified
      else{
        pdf(paste0(name,".pdf")) 
        }
      for (var in varlist_cat){
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
            drop_na(all_of(varlist_cat))
        }
        #Run model
        model <- glm(formula(paste0(outcome, "~as.factor(", var, ")")), 
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
        #Prep contingency table
        contingency_tab <- dataset_cc %>% 
          select(all_of(var),.data[[outcome]]) %>%
          tbl_cross(row = all_of(var),
                    col = .data[[outcome]],
                    percent = "cell",
                    missing = "no",
                    label = list(all_of(var) ~ paste0(var_name))) %>%
          bold_labels()
        contingency_tab <- as_gt(contingency_tab)
        gt_plot_cont_tab <- gt_grob(contingency_tab)
        
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
          patchwork::wrap_plots(
            #boxplot
            ggplot(dataset_cc, aes(x=as.character(.data[[var]]), y = pred_prob)) +
              geom_boxplot() +
              ylab("Predicted probability \n in each group") +
              xlab(paste0(var_name)),
            #calibration
            dataset_cc %>%
              dplyr::select(pred_prob, .data[[outcome]]) %>%
              group_by(pred_prob) %>%
              #count(.data[[outcome]]) %>%
              drop_na(pred_prob) %>%
              mutate(Proportion = sum(.data[[outcome]])/n()) %>%
              dplyr::select(pred_prob, Proportion) %>%
              unique() %>%
              ggplot(aes(x=pred_prob, y = Proportion)) +
              geom_point() +
              geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
              ylim(c(0, 1)) + 
              xlim(c(0, 1)) +
              xlab("Predicted probability \n in each group") +
              ylab("Proportion of outcome \n in each group"),
            ncol = 2, nrow = 1
            ),
          #calibration plot
          patchwork::wrap_plots(
            #Contingency table
            gt_plot_cont_tab,
            #Bar plot
            dataset_cc %>%
              dplyr::select(any_of(var), .data[[outcome]]) %>%
              group_by(.data[[var]]) %>%
              #count(.data[[outcome]]) %>%
              drop_na(.data[[var]]) %>%
              mutate(Proportion = sum(.data[[outcome]])/n()) %>%
              dplyr::select(any_of(var), Proportion) %>%
              unique() %>%
              ggplot(aes(x=as.character(.data[[var]]), y = Proportion)) +
              geom_col() +
              geom_text(aes(label = round(Proportion,decimals)), vjust=-0.2, size = 3) +
              ylim(c(0, 1)) + 
              xlab(paste0(var_name)) +
              ylab("Proportion of outcome \n in each group"),
            ncol = 2, nrow = 1
            ),
          ncol = 1, nrow = 3
          )
        plot_list[[var]] = model_assess_plots
        }
      print(plot_list)
      dev.off()
    }
    } 
  #if no outcome - only looking at categorical vars only
  else{
    #Analysis stuff for when no outcomes
    if(is.null(saving_name)){
      #Save table with default name
      } 
    else{
      #save tables with specified name
      }
    #Produce PDF with default name
    if(plots == TRUE){
      plot_list = list()
      if(is.null(saving_name)){
        #Provide default plots name
        }
      else{
        #Provide user-defined plots name
        }
      for (var in varlist_cat){
        #browser()
        #summary(dataset$v)
        var_name <- paste0(var)
        #IF complete_case is FALSE
        if(complete_case == FALSE){
          dataset_cc <- dataset %>%
            drop_na(all_of(var))
          }
        #If want complete case for all vars
        else{
          dataset_cc <- dataset %>%
            drop_na(all_of(varlist_cat))
        }
      }
    }
  }
  }

