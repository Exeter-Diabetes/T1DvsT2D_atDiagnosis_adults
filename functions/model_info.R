#################################################################################
#Function for applying model to new data, and getting out model info
#####################################################################################

model_info <- function(model = NULL, 
                       test_data = NULL, 
                       outcome = NULL, 
                       saving_name = NULL){
  #load libraries
  library(tidyverse)
  library(pROC)
  library(patchwork)
  library(writexl)
  library(gdata)
  
  
  #Checks for "model"
  if(missing(model) | is.null(model)) {stop("'model' needs to be provided.")}
  #Checks for "test_data"
  if(missing(test_data) | is.null(test_data)) {stop("'test_data' needs to be provided.")}
  
  #save names as vectors
  name <- saving_name
  
  #MODEL COEFFICIENTS 
  summaryTable <- data.frame(model = NA, 
                             dataset = NA, 
                             n = NA, 
                             #outcome1_n = NA, 
                             #outcome2_n = NA,
                             ROC_AUC = NA,
                             Threshold = NA,
                             Sensitivity = NA, 
                             Specificity = NA,
                             mean_prob = NA #, 
                             #mean_outcome1_prob = NA, 
                             #mean_outcome2_prob = NA
                             )
  #browser()
  
  model_name <- deparse(substitute(model))
  test_data_name <- deparse(substitute(test_data))
  summaryTable$model <- deparse(substitute(model))
  summaryTable$dataset <- deparse(substitute(test_data))
  summaryTable$n <- nrow(test_data)
  groups <- unique(test_data[[outcome]])
  groups <- groups[!groups == 'NA']
  
  
  
  coef <- as.data.frame(round(model$coefficient, 2))
  coef_t <- as.data.frame(t(round(model$coefficient, 2)))
  
  conf_int <- as.data.frame(confint(model))
  coefs <- cbind(coef, conf_int)
  coefs$variable <- row.names(coefs)
  coefs <- coefs %>%
    rename(
      estimate = `round(model$coefficient, 2)`,
      lower = `2.5 %`, 
      upper = `97.5 %`
    )
  
  coefs <<- as.data.frame(coefs)
  
  conf_int$conf_int <- paste0("(", round(conf_int$`2.5 %`,2), ", ", 
                              round(conf_int$`97.5 %`, 2), ")")
  conf_int$`2.5 %` <- NULL
  conf_int$`97.5 %` <- NULL
  
  conf_int <- as.data.frame(t(conf_int))
  
  for (c in colnames(coef_t)) {
    coef_t[[c]] <- paste0(coef_t[1,c], " ", conf_int[1,c])
  }
  summaryTable <- cbind(summaryTable, coef_t)
  
  
  
  #MODEL DISCRIMINATION
  #
  test_data <- test_data %>%
    mutate(pred_prob = predict(model, test_data, type = "response"))
  pred_prob <- predict(model, test_data, type = "response")
  model_outcome <- test_data[[outcome]]
  #browser()
  #ROC curve preparation
  roc_curves <- data.frame(prob = pred_prob) %>%
    cbind(Outcome = test_data[[outcome]]) %>%
    pROC::roc(response = Outcome, predictor = prob) %>%
    magrittr::extract(2:4) %>%
    as.data.frame() %>%
    mutate(
      auc =  unname(data.frame(prob = pred_prob) %>%
                      cbind(Outcome = test_data[[outcome]]) %>%
                      pROC::roc(response = Outcome, predictor = prob) %>%
                      magrittr::extract(c(9)) %>%
                      unlist()),
      sum_sens_spec = sensitivities + specificities, 
      max_ss_sum = max(sum_sens_spec),
      best_threshold = ifelse(sum_sens_spec == max_ss_sum, thresholds, NA),
      best_sens = ifelse(sum_sens_spec == max_ss_sum, sensitivities, NA),
      best_spec = ifelse(sum_sens_spec == max_ss_sum, specificities, NA),
      mean = mean(pred_prob, na.rm = TRUE), 
      auc_low = unname(data.frame(prob = pred_prob) %>%
                         cbind(Outcome = test_data[[outcome]]) %>%
                         pROC::roc(response = Outcome, 
                                   predictor = prob, 
                                   ci = TRUE) %>%
                         magrittr::extract2(16) %>%
                         magrittr::extract(1) %>%
                         unlist()),
      auc_high = unname(data.frame(prob = pred_prob) %>%
                          cbind(Outcome = test_data[[outcome]]) %>%
                          pROC::roc(response = Outcome, 
                                    predictor = prob, 
                                    ci = TRUE) %>%
                          magrittr::extract2(16) %>%
                          magrittr::extract(3) %>%
                          unlist())
    ) 
  summaryTable$AIC <- model$aic
  summaryTable$ROC_AUC <- unique(roc_curves$auc)
  summaryTable$ROC_CI <- paste0("(", round(unique(roc_curves$auc_low), 3), ", ", 
                                round(unique(roc_curves$auc_high), 3), ")")
  summaryTable$ROCAUC_CI <- paste(round(summaryTable$ROC_AUC, 3), summaryTable$ROC_CI)
  summaryTable$mean_prob <- unique(roc_curves$mean)
  summaryTable$Threshold <- max(roc_curves$best_threshold, na.rm = TRUE)
  summaryTable$Sensitivity <- max(roc_curves$best_sens, na.rm = TRUE)
  summaryTable$Specificity <- max(roc_curves$best_spec, na.rm = TRUE)
  for (g in groups) {
    #browser()
    table_outcome_groups <- data.frame(n=NA, mean = NA)
    dataset1 <- data.frame(prob = as.numeric(pred_prob)) %>%
      cbind(Outcome = test_data[[outcome]])
    dataset1 <- dataset1[dataset1$Outcome== g & !is.na(dataset1$Outcome),]
    table_outcome_groups$n <- nrow(dataset1)
    colnames(table_outcome_groups)[colnames(table_outcome_groups) == "n"] <- paste(g, "n")
    table_outcome_groups$mean <- mean(as.numeric(dataset1$prob))
    colnames(table_outcome_groups)[colnames(table_outcome_groups) == "mean"] <- paste(g, "mean")
    summaryTable <- cbind(summaryTable, table_outcome_groups)
    
  }
  dat_text <- roc_curves %>%
    dplyr::select(-sensitivities, -specificities) %>%
    distinct() %>%
    mutate(
      auc_full = paste0("AUC: ", signif(auc, 2)),
      mean = paste0("Mean prob:", signif(mean, 2)*100, "%")
    )
  
  
  #MODEL CALIBRATION preparation
  ## split into deciles (hack to keep edge cases)
  brks_nm1 <- quantile(pred_prob, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
  brks_nm1[1] <- 0.99 * brks_nm1[1]
  brks_nm1[length(brks_nm1)] <- 1.1 * brks_nm1[length(brks_nm1)]
  dec_nm1 <- cut(
    pred_prob, 
    breaks = brks_nm1,
    include_lowest = TRUE
  )
  
  dec_nm1 <- data.frame(y = test_data[[outcome]], 
                        pred = pred_prob, 
                        dec = dec_nm1) %>%
    group_by(dec) %>%
    mutate(prob_obs = sum(y) / n(), 
           obs = sum(y),
           n_group = n(),
           mnpred = mean(pred),
           lower = lapply(sum(y), prop.test, n = n()), 
           upper = sapply(lower, function(x) x$conf.int[2]), 
           lower = sapply(lower, function(x) x$conf.int[1]))
  
  #print model assessment table
  summaryTable <<- as.data.frame(summaryTable)
  if(is.null(saving_name)){
    #Save table with default name
  mv(from= "summaryTable", 
     to = paste0(model_name, "_", 
                 test_data_name,"_", outcome,"_summary_table"), 
     envir = globalenv())
  write_xlsx(summaryTable, 
             paste0(model_name,"_",test_data_name,"_", outcome,"_model_assessment_table.xlsx"))
  } else {
    mv(from= "summaryTable", 
       to = name, 
       envir = globalenv())
    write_xlsx(summaryTable, 
               paste0(name, ".xlsx"))
  }
  
  ## plot 
  if(is.null(saving_name)){
 pdf(paste0(model_name,"_",test_data_name,"_", outcome,"_model_assessment_plots.pdf"), height = 12, width = 7) 
  } else
  {pdf(paste0(name,".pdf"), height = 12, width = 7)}
  model_assess_plots <- patchwork::wrap_plots(
    #model_coefficients
    ggplot(coefs, aes(x=variable, y=as.numeric(estimate), ymin=as.numeric(lower), ymax=as.numeric(upper))) +
      geom_pointrange(aes(x=variable, 
                          y=estimate, 
                          ymin=lower, 
                          ymax=upper)) + 
      geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
      coord_flip() +  # flip coordinates (puts labels on y axis)
      xlab("Model variables") + 
      ylab("Estimate (95% CI)") +
      theme_bw(),  # use a white background
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
      ),
    #boxplot
    ggplot(test_data, aes(x=as.character(.data[[outcome]]), y = pred_prob)) +
      geom_boxplot() +
      xlab(outcome)+
      ylab("Predicted probability \n of outcome") +
      theme_bw(),
    #calibration plot
    ggplot(dec_nm1, aes(x = mnpred, y = prob_obs)) +
      geom_point() +
      xlab("Mean predicted probability in each decile") +
      ylab("Proportion of outcome \n in each decile") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      ylim(c(0, 1)) + xlim(c(0, 1)) +
      geom_errorbar(aes(ymin = lower, ymax = upper))+
      theme_bw(),
    ncol = 1, nrow = 4 
    #heights = 3, widths = 4
  ) #+
  #   plot_layout(nrow = 3, design = "
  #               #1#
  #               #2#
  #               #3#
  #               ")
  print(model_assess_plots)
  dev.off()
}

#model_info(model = modela, test_data = SR, outcome = "SRoutcome")


#dataset[dataset[[var_cat]],]
#outcome <- SR[SR[[SRoutcome]],]
#pred_prob <- predict(modela, SR, type = "response")
#roc(outcome, pred_prob, plot = TRUE, print.thres = "best", print.auc = TRUE, ci = TRUE)

#class(SR$SRoutcome)
#class(SR_no_bias$osmotic)

#########################################################################################



#roc_function <- function(dataset = dataset, outcome = outcome, prediction = prediction, group = group){
  #browser()
 # groups <- unique(dataset[[group]])
  #uni_cont_table <- data.frame()
  #for(g in groups){
  #  dataset1 <- dataset[dataset[[group]]== g & !is.na(dataset[[group]]),]
  #  sum_table <- data.frame(group = NA, accuracy = NA, sensitivity = NA, specificity = NA, PPV = NA, NPV = NA)
  #  sum_table$group <- g
  #  print(g)
  #  if(length(unique(dataset1[[outcome]])) == 2) {
   #   roc_model <- roc(dataset1[[outcome]], dataset1[[prediction]], plot = TRUE, print.thres = "best", print.auc = TRUE)
   #   model_pr <- coords(roc_model, x = "best", ret=c("threshold", "specificity", "sensitivity", "accuracy", "precision", "recall", "ppv", "npv"), transpose = FALSE)
      #model_pr <- model_pr[c(-1,-3),]
      
      
    #  sum_table$threshold <- model_pr$threshold
    #  sum_table$ROCAUC <- roc_model$auc
    #  sum_table$accuracy <- model_pr$accuracy
    #  sum_table$sensitivity <- model_pr$sensitivity
    #  sum_table$specificity <- model_pr$specificity
    #  sum_table$PPV <- model_pr$ppv
     # sum_table$NPV <- model_pr$npv
      
     # uni_cont_table <- rbind(uni_cont_table, sum_table)
     # print(roc_model)
    #} else {
    #  print("outcome has >/< two levels")}
  #}
 # uni_cont_table <<- as.data.frame(uni_cont_table)
#} 

#roc_function(dataset = MY_T1D_SA_long, outcome = "M", prediction = "probs", group = "model")

#MY_T1D_SA_ROC_table <- as.data.frame(uni_cont_table)
#write_xlsx(MY_T1D_SA_ROC_table,"MY_T1D_SA_ROC_table.xlsx")

#roc_function(dataset = MY_T2D_SA_long, outcome = "M", prediction = "probs", group = "model")

#MY_T2D_SA_ROC_table <- as.data.frame(uni_cont_table)
#write_xlsx(MY_T2D_SA_ROC_table,"MY_T2D_SA_ROC_table.xlsx")


##CALIBRATION PLOTS ---------------------------------------------------------------------------


#cal_function <- function(dataset = dataset, outcome = outcome, prediction = prediction, group = group){
  #browser()
#  groups <- unique(dataset[[group]])
#  for(g in groups){
#    dataset1 <- dataset[dataset[[group]]== g & !is.na(dataset[[group]]),]
#    print(g)
#    if(length(unique(dataset1[[outcome]])) == 2) {
 #     brks_1_SR_dd <- unique(quantile(dataset1[[prediction]], probs = seq(0, 1, by = 0.1), na.rm = TRUE))
 #     brks_1_SR_dd[1] <- 0.99 * brks_1_SR_dd[1]
 #     brks_1_SR_dd[length(brks_1_SR_dd)] <- 1.1 * brks_1_SR_dd[length(brks_1_SR_dd)]
 #     dec_1_SR_dd <- cut(
  #      dataset1[[prediction]], 
    #    breaks = brks_1_SR_dd,
    #    include_lowest = TRUE
    #  )
      
    #  dec_1_SR_dd <- data.frame(y = dataset1[[outcome]], pred = dataset1[[prediction]], dec = dec_1_SR_dd) %>%
     #   group_by(dec) %>%
     #   mutate(prob_obs = sum(y) / n(), 
      #         obs = sum(y),
      ##         n_group = n(),
      #         mnpred = mean(pred),
      #         lower = lapply(sum(y), prop.test, n = n()), 
      #         upper = sapply(lower, function(x) x$conf.int[2]), 
      #         lower = sapply(lower, function(x) x$conf.int[1]))
      
      ## plot 
     # PLOT <- ggplot(dec_1_SR_dd, aes(x = mnpred, y = prob_obs)) +
     #   geom_point() +
     #   xlab("Mean predicted probability in each decile") +
     #   ylab("Proportion of MODY in each decile") +
     #   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
     #   geom_errorbar(aes(ymin = lower, ymax = upper)) +
     #   ylim(c(0, 1)) + xlim(c(0, 1)) +
     #   coord_cartesian(xlim=c(0,1)) +
     #   scale_y_continuous(labels = scales::percent) +
     #   scale_x_continuous(labels = scales::percent) +
     #   theme_light()
     # print(PLOT)
    #} else {
    #  print("outcome has >/< two levels")}
  #}
#} 

#cal_function(dataset = SR, outcome = "SRoutcome", prediction = "probs", group = "model")