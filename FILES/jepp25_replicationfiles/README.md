*Replication Pipeline for Sorace, M. "The Europeanisation of Policy Preferences: Cross-national Similarity and Convergence 2014-2024" JEPP.*

The analysis consists of 6 sequential scripts that must be run in a specific order to ensure reproducibility and the political parties metadata from the 2014, 2019, 2024 European Election Studies - Voter Studies.
Download the three EES studies and place into the ees_datasets folder. GESIS link (last accessed: 13/06/2025) here: https://www.gesis.org/en/services/finding-and-accessing-data/international-survey-programs/european-election-studies.
Scripts

1_sorace_epo_setup.R - Setup and packages
2_sorace_epo_dataloadclean.R - Data loading/cleaning
3_sorace_epo_emdcalcs.Rmd - EMD calculations
4_sorace_epo_irt.Rmd - IRT analysis
5_sorace_epo_randomforest.Rmd - Random Forest modeling
6_sorace_epo_main_analyses.Rmd - Main analyses

Requirements

R 4.0+
All 6 files and ees_datasets folder in the same working directory
Required packages (installed by setup script)

Execution
r# Step 1: Setup
source("1_sorace_epo_setup.R")

# Step 2: Data loading and cleaning
source("2_sorace_epo_dataloadclean.R")

# Step 3: EMD calculations
source("3_sorace_epo_emdcalcs.R")

# Step 4: IRT analysis
source("4_sorace_epo_irt.R")

# Step 5: Random Forest analysis
source("5_sorace_epo_randomforest.R")

# Step 6: Main analyses
knitr::knit("6_sorace_epo_main_analyses.Rmd")
