#######################################################################################

#explore_categorical

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