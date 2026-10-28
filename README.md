# T1D vs T2D at Diagnosis in Adults: Clinical Prediction Modelling

Data analysis code accompanying the paper for the development of a clinical calculator for use at diabetes presentation to differentiate type 1 and type 2 diabetes.

---

# functions: Data exploration and model building and performance functions

The `functions` directory contains reusable analysis and plotting functions used throughout the project.

Examples include:
- `cat_contingency.R`
- `model_comparison4.R`
- `model_continuous.R`
- `model_info.R`
- `var_characteristics.R`


These functions are designed to:
- streamline repeated analyses
- standardise model summaries and plots
- reduce duplication across scripts
- improve reproducibility and maintainability

---

# scripts: Analysis scripts

## Cleaning and preparation

### 01_cleaning: 
- `00_Universal_StartRight_cleaning.R`
- `01_01_Cleaning_StartRight_Age18to50.R`
- `01_02_Cleaning_StartRight_Age50plus.R`
- `01_11_Sense_checks_StartRight_Age18to50.R`
- `01_12_Sense_checks_StartRight_Age50plus.R`

These scripts prepare the analysis dataset, apply cleaning steps, and perform basic checks of data quality and completeness.

---

## Main manuscript outputs
---
### 02_main_tables: Main tables
---
#### Table 1: Baseline characteristics
- File: `01_table1_baseline_characteristics.R`
- Purpose: summarises demographic and clinical characteristics at diagnosis
- Output: manuscript-ready summary table

#### Table 2: StartRight score summary
- File: `02_table2_startright_score.R`
- Purpose: summarises score performance and model comparison metrics
- Output: manuscript-ready summary table
---
### 03_main_figures: Main figures
---

#### Figure 1: Univariate distributions
- File: `01_figure1_univariate_distributions.R`
- Purpose: displays the distributions of key predictors
- Output: publication-style visualisation

#### Figure 2: Model performance (StartRight)
- File: `02_figure2_model_performance_startright.R`
- Purpose: compares model discrimination and performance
- Output: ROC or performance figure

#### Figure 3: Model performance (StartRight prime)
- File: `03_figure3_model_performance_startright_prime.R`
- Purpose: assesses performance of the enhanced model
- Output: ROC or performance figure

#### Figure 4: Model correlation for T2D treated in CPRD
- File: `04_figure4_model_correlation_T2D_treated_CPRD.R`
- Purpose: evaluates correlation structure relevant to model interpretation
- Output: correlation or model comparison figure

#### Figure 5: Antibody testing strategies
- File: `05_figure5_antibody_testing_strategies.R`
- Purpose: summarises policy or clinical testing strategy scenarios
- Output: strategy comparison figure

These scripts are intended to map directly to the main manuscript display items.

---

## Supporting scripts 
---
### 04_appendix: Appendix document outputs

The project includes a substantial appendix section, with analysis scripts organised in the `04_appendix` folder.

Examples include:
- diagnostic and model comparison tables
- additional coefficient summaries
- decision curve analyses
- precision-recall analyses
- additional subgroup analyses
- additional sensitivity analyses
- model stability and optimism analyses
- startright score performance
- startright score with islet-autoantibodies
- extended model and score performance in external CPRD data

The appendix outputs are intended to support the main paper.

---

### 05_supplementary: Supplementary materials

This folder contains additional supplementary analyses and visualisations that are not present in the manuscript or accompanying Appendix, but are however important or valuable to the project.

These include:

- model development
- linearity assessment
- power calculation
- additional supporting results not included in the main manuscript

---



# outputs: Script Outputs

Generated outputs should be saved in the `outputs` directory to keep source code and final deliverables separate.

Contains:
- `tables` – final manuscript and appendix tables
- `figures` – final publication-ready figures

---

# CPRD

The codelists for defining autoimmune disease in CPRD Aurum for this analysis.


---
# generating_your_own: Produce model probabilites and the StartRight Score

Scripts and documentation for applying the models and scores developed and validated in this study in your own datasets. 

Contains how-to's for
- model_probabilities
- startright_score


---


# Authors

Julieanne Knupp
j.knupp@exeter.ac.uk
University of Exeter

---


# Contact

[Add corresponding author or contact details here]
