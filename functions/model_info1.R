#################################################################################
#Function for applying model to new data, and getting out model info
#####################################################################################

model_info <- function(model = NULL,
                       test_data = NULL,
                       outcome = NULL,
                       saving_name = NULL,
                       manual_plotting = FALSE,
                       manual_plot_name = NULL,
                       threshold = NULL,
                       decimals = 2
                       ){
  #load libraries
  library(tidyverse)
  library(pROC)
  library(patchwork)
  library(writexl)
  library(gdata)
  library(qwraps2)
  library(ROCR)
  #browser()

  #Checks for "model"
  if(missing(model) |
     is.null(model))
    {stop("'model' needs to be provided.")}
  #Checks for "test_data"
  if(missing(test_data) |
     is.null(test_data))
    {stop("'test_data' needs to be provided.")}
  #IF manual_plotting is TRUE, need to provide name
  if(manual_plotting == TRUE &
     (missing(manual_plot_name) |
      is.null(manual_plot_name)))
    {stop("'manual_plot_name' needs to be provided")}

  #save names as vectors
  name <- saving_name
  man_plot_name <- manual_plot_name

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


  model_name <- deparse(substitute(model))
  test_data_name <- deparse(substitute(test_data))
  summaryTable$model <- deparse(substitute(model))
  summaryTable$dataset <- deparse(substitute(test_data))
  summaryTable$n <- nrow(test_data)
  groups <- unique(test_data[[outcome]])
  groups <- groups[!groups == 'NA']

  #coefficients for full output table (transposed)
  coef_t <- as.data.frame(t(round(model$coefficient, decimals)))
  odds_ratio_t <- as.data.frame(t(round(exp(model$coefficient),decimals)))


  #coefficients for coefficients table
  ##log-odds/estimate/beta
  estimate <- as.data.frame(round(model$coefficient, decimals))
  ##log-odds/estimate confidence intervals
  conf_int <- as.data.frame(confint(model))
  ##odds-ratio
  odds_ratio <- as.data.frame(round(exp(cbind(OR = coef(model),
                                              confint(model))),
                                    decimals)) %>%
    rename(
      OR_lower = `2.5 %`,
      OR_upper = `97.5 %`
    )
  p_values <- as.data.frame(summary(model)$coefficients[, 'Pr(>|z|)']) %>%
    rename(p_value = `summary(model)$coefficients[, "Pr(>|z|)"]`)


  coefs <- cbind(estimate, conf_int, odds_ratio, p_values)
  coefs$variable <- row.names(coefs)
  coefs <- coefs %>%
    rename(
      estimate = `round(model$coefficient, decimals)`,
      lower = `2.5 %`,
      upper = `97.5 %`
    )

  coefs <<- as.data.frame(coefs)
  if(manual_plotting == TRUE) {
    mv(from= "coefs",
       to = paste0("coefs_", man_plot_name),
       envir = globalenv())
  }


  conf_int$conf_int <- paste0("(", round(conf_int$`2.5 %`,decimals), ", ",
                              round(conf_int$`97.5 %`, decimals), ")")
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
  if(manual_plotting == TRUE) {
    pred_prob <<- predict(model, test_data, type = "response")
    mv(from= "pred_prob",
       to = paste0("pred_prob_", man_plot_name),
       envir = globalenv())
  }
  model_outcome <- test_data[[outcome]]
  #browser()
  #ROC curve preparation
  if(is.null(threshold)){
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
  roc_model <- roc(test_data[[outcome]],
                   pred_prob,
                   plot = TRUE,
                   print.thres = "best",
                   print.auc = TRUE,
                   ci = TRUE)
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

  #Assign model summary information to summary data.frame
  #Precision-recall
  prec_recal_curves <- data.frame(prob = pred_prob) %>%
    cbind(Outcome = test_data[[outcome]]) %>%
    pROC::roc(response = Outcome, predictor = prob) %>%
    pROC::coords(ret = c("precision", "recall")) %>%
    as.data.frame() %>%
    mutate(
      auc = ROCR::performance(prediction(test_data$pred_prob, test_data[[outcome]]), "aucpr")@y.values[[1]])

  }
  if(manual_plotting == TRUE) {
    prec_recal_curves <<- as.data.frame(prec_recal_curves)
    mv(from= "prec_recal_curves",
       to = paste0("prec_recal_curves_", man_plot_name),
       envir = globalenv())
    roc_curves <<- as.data.frame(roc_curves)
    mv(from= "roc_curves",
       to = paste0("roc_curves_", man_plot_name),
       envir = globalenv())
  }

  summaryTable$AIC <- model$aic
  if(is.null(threshold)) {
  summaryTable$ROC_AUC <- unique(roc_curves$auc)
  summaryTable$ROC_CI <- paste0("(", round(unique(roc_curves$auc_low), 3), ", ",
                                round(unique(roc_curves$auc_high), 3), ")")
  summaryTable$ROCAUC_CI <- paste(round(summaryTable$ROC_AUC, 3), summaryTable$ROC_CI)
  summaryTable$mean_prob <- unique(roc_curves$mean)
  summaryTable$Threshold <- max(roc_curves$best_threshold, na.rm = TRUE)
  summaryTable$Sensitivity <- max(roc_curves$best_sens, na.rm = TRUE)
  summaryTable$Specificity <- max(roc_curves$best_spec, na.rm = TRUE)
  summaryTable$PPV <- model_pr$ppv
  summaryTable$NPV <- model_pr$npv
  summaryTable$Accuracy <- model_pr$accuracy
  }
  if(!is.null(threshold)) {
    conf_matrix <- confusion_matrix(model, thresholds = threshold)
    sens <- conf_matrix$cm_stats["sensitivity"]*100
    sens_lci <- conf_matrix$cm_stats["sensitivity_lcl"]*100
    sens_uci <- conf_matrix$cm_stats["sensitivity_ucl"]*100
    summaryTable$Sensitivity <- paste0(round(sens[2,1],decimals),
                                       " (", round(sens_lci[2,1],decimals), "; ",
                                       round(sens_uci[2,1],decimals), ")")
    specif <- conf_matrix$cm_stats["specificity"]*100
    specif_lci <- conf_matrix$cm_stats["specificity_lcl"]*100
    specif_uci <- conf_matrix$cm_stats["specificity_ucl"]*100
    summaryTable$Specificity <- paste0(round(specif[2,1],decimals),
                                       " (", round(specif_lci[2,1],decimals), "; ",
                                       round(specif_uci[2,1],decimals), ")")
    ppv <- conf_matrix$cm_stats["ppv"]*100
    ppv_lci <- conf_matrix$cm_stats["ppv_lcl"]*100
    ppv_uci <- conf_matrix$cm_stats["ppv_ucl"]*100
    summaryTable$PPV <- paste0(round(ppv[2,1],decimals),
                               " (", round(ppv_lci[2,1],decimals), "; ",
                               round(ppv_uci[2,1],decimals), ")")
    npv <- conf_matrix$cm_stats["npv"]*100
    npv_lci <- conf_matrix$cm_stats["npv_lcl"]*100
    npv_uci <- conf_matrix$cm_stats["npv_ucl"]*100
    summaryTable$NPV <- paste0(round(npv[2,1],decimals),
                               " (", round(npv_lci[2,1],decimals), "; ",
                               round(npv_uci[2,1],decimals), ")")
    accur <- conf_matrix$cm_stats["accuracy"]*100
    accur_lci <- conf_matrix$cm_stats["accuracy_lcl"]*100
    accur_uci <- conf_matrix$cm_stats["accuracy_ucl"]*100
    summaryTable$Accuracy <- paste0(round(accur[2,1],decimals),
                                    " (", round(accur_lci[2,1],decimals), "; ",
                                    round(accur_uci[2,1],decimals), ")")
    auc_ci <-conf_matrix[["auroc_ci"]]
    summaryTable$ROC_AUC <- paste0(round(conf_matrix[["auroc"]],decimals),
                                   " (", round(auc_ci[1],decimals), "; ",
                                   round(auc_ci[2],decimals), ")")
    summaryTable$Threshold <- threshold
    summaryTable$mean_prob <- mean(pred_prob, na.rm = TRUE)
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
    summaryTable$ROC_AUC <- unique(roc_curves$auc)
    summaryTable$ROC_CI <- paste0("(", round(unique(roc_curves$auc_low), 3), ", ",
                                  round(unique(roc_curves$auc_high), 3), ")")
    summaryTable$ROCAUC_CI <- paste(round(summaryTable$ROC_AUC, 3), summaryTable$ROC_CI)
  }

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

  if(is.null(threshold)) {
  dat_text <- roc_curves %>%
    dplyr::select(-sensitivities, -specificities) %>%
    distinct() %>%
    mutate(
      auc_full = paste0("AUC: ", signif(auc, 3)),
      mean = paste0("Mean prob:", signif(mean, 3)*100, "%")
    )
  dat_text_pr <- prec_recal_curves %>%
    dplyr::select(-precision, -recall) %>%
    distinct() %>%
    mutate(
      auc_full = paste0("AUC: ", signif(auc, 3)))

  if(manual_plotting == TRUE) {
    dat_text <<- as.data.frame(dat_text)
    mv(from= "dat_text",
       to = paste0("dat_text_", man_plot_name),
       envir = globalenv())
    dat_text_pr <<- as.data.frame(dat_text_pr)
    mv(from= "dat_text_pr",
       to = paste0("dat_text_pr_", man_plot_name),
       envir = globalenv())
  }


  #MODEL CALIBRATION preparation
  ## split into deciles (hack to keep edge cases)
  brks_nm1 <- quantile(pred_prob, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
  brks_nm1[1] <- 0.99 * brks_nm1[1]
  brks_nm1[length(brks_nm1)] <- 1.1 * brks_nm1[length(brks_nm1)]
  dec_nm1 <- cut(
    pred_prob,
    breaks = unique(brks_nm1),
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

  if(manual_plotting == TRUE) {
    dec_nm1 <<- as.data.frame(dec_nm1)
    mv(from= "dec_nm1",
       to = paste0("cal_prep_", man_plot_name),
       envir = globalenv())
  }
  }
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

  if(is.null(threshold)) {
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

  )
  print(model_assess_plots)
  dev.off()
  }
}


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
