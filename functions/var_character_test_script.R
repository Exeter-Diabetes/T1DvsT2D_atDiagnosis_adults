source("functions/var_characteristics_1.R")


#load data-------------------------------------------------------------------------
#load dataset from 01_11
load("~/PhD/StartRight_paper/data/SR1.1_RE_cc_1_2025.RData")

#1) by whole group --------------------------------------------------------------
##a) characteristics table ------------------------------------------------------
#make sure all in numeric format
varlist = c("AgeatDiagnosis", "bmi", "bmi_diag")
#create varlist_cat (categorical variables of interest names)
varlist_cat = c("num_anti", "famhisauto", "famhisinsdiab", 
                "famhisnoninsdiab")


var_characteristics(varlist = varlist, 
                    varlist_cat = varlist_cat, 
                    dataset = SR1.1_RE_cc, 
                    numeric_option = "meanSD")


var_characteristics(varlist = varlist, 
                    varlist_cat = varlist_cat, 
                    dataset = SR1.1_RE_cc, 
                    numeric_option = "meanSD", 
                    missingness = FALSE)

var_characteristics(varlist = varlist, 
                    varlist_cat = varlist_cat, 
                    dataset = SR1.1_RE_cc, 
                    numeric_option = "meanSD", 
                    table_name = "test9")

var_characteristics(varlist = varlist, 
                    varlist_cat = varlist_cat, 
                    dataset = SR1.1_RE_cc, 
                    numeric_option = "meanSD", 
                    missingness = FALSE, 
                    table_name = "test10")

var_characteristics(varlist = varlist, 
                    varlist_cat = varlist_cat, 
                    dataset = SR1.1_RE_cc, 
                    numeric_option = "meanSD", 
                    group = "SRoutcome", 
                    table_name = "test8", 
                    stat_test_cont = "Wilcox",
                    stat_test_cat = "Chi_square")


var_characteristics(varlist = varlist, 
                    varlist_cat = varlist_cat, 
                    dataset = SR1.1_RE_cc, 
                    numeric_option = "meanSD", 
                    missingness = FALSE, 
                    group = "SRoutcome", table_name = "test3")

SR_TEST <- SR1.1_RE_cc %>%
  select(AgeatDiagnosis, bmi, Gender_v1, Snack_or_drink_since_last_meal_v1)

SR_TEST <- SR_TEST %>%
  mutate(age_cat = ifelse(!is.na(SR_TEST[[4]]), Gender_v1, "MISSING MEAL"))

groups <- c("bees", "cats")

test <- data.frame("bees n" = c(NA, NA, NA, 1, 2, 3),
                   `bees size` = c(34, 56, 23, NA, NA, NA), 
                   `cats n` = c(NA, NA, NA, 4, 6, 2),
                   `cats size` = c(45, 12, 78, NA, NA, NA),
                   nums = c(1, 2, 3, 4, 5, 6),
                   check.names = FALSE)

test <- test %>%
  mutate(
    `bees n/size` = coalesce(`bees.n`, `bees.size`),
    `cats n/size` = coalesce(`cats.n`, `cats.size`)
  )

for(group in groups) {
  test <- test %>%
    mutate(
      !!paste(group, "n/size", sep = " ") := coalesce(!!sym(paste(group, "n", sep = " ")), !!sym(paste(group, "size", sep = " ")))
    )
}

for(group in groups) {
  test <- test %>%
    mutate(
      !!paste(group, "n/size", sep = "/") := coalesce(!!sym(paste(group, "n", sep = " ")), !!sym(paste(group, "size", sep = " ")))
    ) %>%
    select(-matches(paste(group, "(n|size)", sep = " ")))  # Remove the 'n' and 'size' columns for this group
}

for(g in groups){
  test$n_num_option_combined <- ifelse(!is.na(select(test, contains(g) & ends_with("n"))),
                                       "cat", 
                                       "dog")
  colnames(test)[colnames(test) == "n_num_option_combined"] <- paste0(g, "n / size")
  #print(n_num_option_combined)
}
#Make combined columns per group and remove separate columns
#`Female n/median [IQR]` = ifelse(!is.na(`Female n`), `Female n`, `Female median_IQR`),
#`Male n/median [IQR]` = ifelse(!is.na(`Male n`), `Male n`, `Male median_IQR`))
#for(g in groups){
#summaryTable_GROUP_missing$n_num_option_combined <- ifelse(!is.na(str_detect(names(summaryTable_GROUP_missing),)))
# colnames(summaryTable_GROUP_missing)[colnames(summaryTable_GROUP_missing) == "n_num_option_combined"] <- paste(g, "n /", num_option_name)
#combine mean, lci and uci columns into 1 (mean_ci)

# summaryTable_nmiss$perc_1 <- NULL
#colnames(summaryTable_nmiss)[colnames(summaryTable_nmiss) == "n_miss"] <- paste(g, "n missing(%)")

#}

#Remove separate n and mean/median columns and rearrange logically
#summaryTable_GROUP_missing <- summaryTable_GROUP_missing %>%
#select(variable, category, `Male n/median [IQR]`, `Male n missing(%)`, 
#`Female n/median [IQR]`, `Female n missing(%)`)

groups <- c("bees", "cats")

test <- data.frame(
  nums = c(1, 2, 3, 4, 5, 6),
  "bees n" = c(NA, NA, NA, 1, 2, 3),
  `cats n` = c(NA, NA, NA, 4, 6, 2),
  `cats size` = c(45, 12, 78, NA, NA, NA),
  `bees size` = c(34, 56, 23, NA, NA, NA), 
  check.names = FALSE)

length(groups)

df <- data.frame(
  var = 1:5,
  cat = c("A", "B", "C", "D", "E"),
  blue_x = 1:5,
  brown_x = 6:10,
  blue_y = 11:15,
  brown_y = 16:20
)

# Groups vector (can be modified as per input)
groups <- c("blue", "brown")

# Function to rearrange columns based on group pattern
rearrange_columns <- function(df, groups) {
  # Start with the initial columns ("var" and "cat")
  new_order <- c("var", "cat")
  
  # Loop over each group to add columns in the "x" and "y" alternating pattern
  for(group in groups) {
    new_order <- c(new_order, paste(group, "x", sep = "_"), paste(group, "y", sep = "_"))
  }
  
  # Reorder columns
  df <- df[, new_order]
  return(df)
}

# Apply the function to rearrange the columns
df_rearranged <- rearrange_columns(df, groups)

# View the result
print(df_rearranged)
