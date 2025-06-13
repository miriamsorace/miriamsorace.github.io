# loading required libraries
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE, repos="http://cran.rstudio.com/",INSTALL_opts = '--no-lock')
  require(p, character.only = TRUE)
}

usePackage('tidyr')
usePackage('devtools')
if (!("quanteda.corpora" %in% installed.packages())) {
  devtools::install_github("quanteda/quanteda.corpora")
}
require(quanteda.corpora)
usePackage('readtext')
usePackage('dplyr')
usePackage('sjmisc')
usePackage('modeest')
usePackage('ggplot2')
usePackage('psych')
usePackage('irrCAC')
usePackage('data.table')
usePackage('xtable')
usePackage('stargazer')
if (!("stargazer" %in% installed.packages())) {
  ## Quick fix for stargazer <= 5.2.3 is.na() issue with long model names in R >= 4.2
  # Unload stargazer if loaded
  detach("package:stargazer",unload=T)
  # Delete it
  remove.packages("stargazer")
  # Download the source
  download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
  # Unpack
  untar("stargazer_5.2.3.tar.gz")
  # Read the sourcefile with .inside.bracket fun
  stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
  # Move the length check 5 lines up so it precedes is.na(.)
  stargazer_src[1990] <- stargazer_src[1995]
  stargazer_src[1995] <- ""
  # Save back
  writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
  # Compile and install the patched package
  install.packages("stargazer", repos = NULL, type="source")
}
usePackage('haven')
usePackage("openxlsx")
usePackage("readxl")
usePackage("readr")
usePackage("doBy")
usePackage("sandwich")
usePackage("lmtest")
usePackage("QuantPsyc")
usePackage("plm")
usePackage("collapse")
usePackage("marginaleffects")
usePackage("ggpubr")
usePackage("margins")
usePackage("cowplot")
usePackage("sf")
usePackage("foreign")
usePackage("survey")
usePackage('lme4')
usePackage("ggrepel")
usePackage("stringr")
usePackage("emdist")
usePackage("grid")
usePackage("sfsmisc")
usePackage("clusterGeneration")
usePackage("reshape")
usePackage("emdbook")
usePackage("gridExtra")
usePackage("tidyverse")
usePackage("caret")
usePackage("glmnet")
usePackage("mirt")
usePackage("randomForest")
usePackage("xgboost")
usePackage("pROC")
usePackage("purrr")
usePackage("foreach")
usePackage("doParallel")
usePackage("tictoc")
if (!("dyadRobust" %in% installed.packages())) {
  devtools::install_github("jbisbee1/dyadRobust")
}
require(dyadRobust)

# Check necessary libraries
usePackage("tidyverse")
usePackage("caret")
usePackage("glmnet")
usePackage("randomForest")
usePackage("xgboost")
usePackage("pROC")
usePackage("purrr")
usePackage("foreach")
usePackage("doParallel")
usePackage("tictoc")

