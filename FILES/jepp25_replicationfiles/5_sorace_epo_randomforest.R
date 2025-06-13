load("ees14clean.RData")
load("ees19clean.RData")
load("ees24clean.RData")

# 2014 ------------------------------------------------
# Define the predictors and the outcome variables
predictors <- c("edu", "age_cat", "female", "ruralurban", 
                "socclass", "religion", "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c(  "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Define the issue names
issue_names <- c(
  issue_state_int = "Attitudes on State Intervention in the Economy",
  issue_redistr = "Attitudes on Redistribution",
  issue_samesex = "Attitudes on Same-Sex Marriage",
  issue_immig = "Attitudes on Immigration",
  issue_clim = "Attitudes on Environmental Protection"
)

# Function to filter and mutate model_summary
process_model_summary <- function(model_summary) {
  model_summary <- model_summary %>%
    filter(term != "(Intercept)") %>%
    mutate(term = factor(term, levels = c("eduless than 15 years",
                                          "edustill studying",
                                          "edu16-19 years",
                                          "edu20+ years",
                                          "age_cat25-34",
                                          "age_cat35-44",
                                          "age_cat45-54",
                                          "age_cat55-64",
                                          "age_cat65+",
                                          "femaleFemale",
                                          "ruralurbanSmall/Mid-sized town",
                                          "ruralurbanLarge town/city",
                                          "socclassMiddle class",
                                          "socclassUpper class",
                                          "socclassOther",
                                          "religionOther Christian",
                                          "religionAtheist/Agnostic",
                                          "religionOther",
                                          "workstat_catStudent/Unemployed",
                                          "workstat_catHousehold worker",
                                          "workstat_catSelf-employed",
                                          "workstat_catRetired",
                                          "workstat_catOther",
                                          "livingstdrd",
                                          "polint",
                                          "ptyfamidgreen","ptyfamidleft","ptyfamidliberals","ptyfamidright",
                                          # "ptyfamidsocial democrats",
                                          # "ptyfamidliberals",
                                          # "ptyfamidchristian democrats/centre right",
                                          # "ptyfamidconservative",
                                          # "ptyfamidradical right",
                                          "ptyfamidniche/fringe",
                                          "ptyfamidno party id")))
  return(model_summary)
}

# Function to create custom labels
create_custom_labels <- function(term) {
  term <- gsub("female", "Gender: ", term)
  term <- gsub("age_cat", "Age: ", term)
  term <- gsub("edu", "Education: ", term)
  term <- gsub("ruralurban", "Rural/Urban: ", term)
  term <- gsub("socclass", "Social Class: ", term)
  term <- gsub("religion", "Religion: ", term)
  term <- gsub("workstat_cat", "Work Status: ", term)
  term <- gsub("livingstdrd", "Living Standard", term)
  term <- gsub("polint", "Political Interest", term)
  term <- gsub("ptyfamid", "Party Block: ", term)
  return(term)
}

# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", 
                "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c("issue_state_int", "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Dictionary for cleaning outcome names
issue_names <- c(
  issue_state_int = "Attitudes on State Intervention in the Economy",
  issue_redistr = "Attitudes on Redistribution",
  issue_clim = "Attitudes on Environmental Protection",
  issue_samesex = "Attitudes on Same-Sex Marriage",
  issue_immig = "Attitudes on Immigration"
)

# Convert categorical variables to factors if needed (check and preprocess)
data <- ees14  # Assuming ees14 is your main dataset

# Remove any rows with missing data
data <- drop_na(data)

# Convert outcome variables to factors in the dataset
for (outcome in outcomes) {
  data[[outcome]] <- factor(data[[outcome]], levels = 0:10)
}

# Split the data into 80% training and 20% testing
set.seed(123)  # Set seed for reproducibility
train_index <- createDataPartition(data[[outcomes[1]]], p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


# -------------------------
# Tuning Random Forest for All Outcomes
# -------------------------

# Grid of parameters to tune
ntree_vals <- c(100, 200, 400, 800)
maxnodes_vals <- c(10, 20, 40, 80)

# Store results in a data frame
all_results <- data.frame(
  outcome = character(),
  ntree = integer(),
  maxnodes = integer(),
  accuracy = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each outcome
for (outcome_var in outcomes) {
  print(paste("Tuning Random Forest for outcome:", outcome_var))
  
  # Loop through combinations of ntree and maxnodes
  for (ntree in ntree_vals) {
    for (maxnodes in maxnodes_vals) {
      
      # Train Random Forest model
      rf_model <- randomForest(
        as.formula(paste(outcome_var, "~", paste(predictors, collapse = "+"))),
        data = train_data,
        ntree = ntree,
        maxnodes = maxnodes
      )
      
      # Predict on test data
      preds <- predict(rf_model, newdata = test_data)
      
      # Calculate accuracy
      acc <- mean(preds == test_data[[outcome_var]])
      
      # Store results
      all_results <- rbind(all_results, data.frame(
        outcome = outcome_var,
        ntree = ntree,
        maxnodes = maxnodes,
        accuracy = acc
      ))
    }
  }
}

# View results
print("Best Parameters for Each Outcome - 2014:")
best_results <- all_results %>%
  group_by(outcome) %>%
  filter(accuracy == max(accuracy)) %>%
  ungroup()

print(best_results)

## Overall Model Performance
# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", 
                "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c("issue_state_int", "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Dictionary for cleaning outcome names
issue_names <- c(
  issue_state_int = "Attitudes on State Intervention in the Economy",
  issue_redistr = "Attitudes on Redistribution",
  issue_clim = "Attitudes on Environmental Protection",
  issue_samesex = "Attitudes on Same-Sex Marriage",
  issue_immig = "Attitudes on Immigration"
)

# Convert categorical variables to factors if needed (check and preprocess)
data <- ees14  # Assuming ees14 is your main dataset

# Remove any rows with missing data
data <- drop_na(data)

# Convert outcome variables to factors in the dataset
for (outcome in outcomes) {
  data[[outcome]] <- factor(data[[outcome]], levels = 0:10)
}

# Split the data into 80% training and 20% testing
set.seed(123)  # Set seed for reproducibility
train_index <- createDataPartition(data[[outcomes[1]]], p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Initialize a list to store the models, variable importances, and OOB R-squared values
rf_models <- list()
variable_importances <- list()
oob_r_squared <- list()

# Loop over each outcome and train a random forest model
for (outcome in outcomes) {
  
  #tuned_params
  best_params <- best_results %>%
    filter(outcome == outcome_var) %>%
    dplyr::select(ntree, maxnodes) %>%
    unlist() %>%
    as.list()
  
  # Train the random forest model for classification
  rf_model <- randomForest(
    as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
    data = train_data,
    na.action = na.omit,
    ntree = best_params$ntree,          # Number of trees
    maxnodes = best_params$maxnodes,        # Max nodes
    importance = TRUE,    # Enable variable importance calculation
    keep.inbag = TRUE     # Keep OOB predictions for OOB R-squared
  )
  
  # Store the model in the list
  rf_models[[outcome]] <- rf_model
  
  # Calculate variable importance and store it
  importance_scores <- importance(rf_model)
  variable_importances[[outcome]] <- importance_scores
  
  # Calculate OOB R-squared
  oob_error_rate <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
  r_squared_oob <- 1 - oob_error_rate
  oob_r_squared[[outcome]] <- r_squared_oob
  
  # Print OOB R-squared
  cat("OOB R-squared for", outcome, ":", r_squared_oob, "\n")
}

# Plot and save Variable Importance for each outcome
for (outcome in outcomes) {
  cat("\nVariable Importance for", outcome, ":\n")
  
  # Define the file name for saving the plot
  file_name <- paste0("vip_", outcome, "_14.pdf")
  
  # Open a PDF device to save the plot
  pdf(file = file_name)
  
  # Plot the variable importance using base R plot
  varImpPlot(rf_models[[outcome]], main = paste("Variable Importance for", issue_names[[outcome]], "EES 2014"))
  
  # Close the PDF device
  dev.off()
}

cat("Variable importance plots have been saved.\n")

# Detect cores and register parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Parallel loop over each outcome to predict and calculate performance
test_performance <- foreach(outcome = outcomes, .combine = 'rbind', .packages = c('randomForest', 'caret')) %dopar% {
  
  # Predict on test data
  test_pred <- predict(rf_models[[outcome]], newdata = test_data, type = "response")
  
  # Debugging Step: Print the first few predictions
  cat("Predictions for outcome", outcome, ":", head(test_pred), "\n")
  
  # Remove rows with missing actual outcomes in test_data, keep the rest of the data intact
  test_data_clean <- test_data[!is.na(test_data[[outcome]]), ]
  test_preds_clean <- test_pred[!is.na(test_data[[outcome]])]
  
  # Debugging Step: Check how many rows are left after cleaning
  cat("Rows left for outcome", outcome, "after cleaning:", nrow(test_data_clean), "\n")
  cat("Predictions for outcome", outcome, "after cleaning:", length(test_preds_clean), "\n")
  
  # Ensure lengths match
  if (length(test_preds_clean) != nrow(test_data_clean)) {
    stop(paste("Mismatch in number of predictions and actual outcomes for outcome:", outcome))
  }
  
  # Check levels of actual and predicted before factor conversion
  actual_levels <- levels(factor(test_data_clean[[outcome]]))
  pred_levels <- unique(test_preds_clean)
  
  # Debugging Step: Check if the levels of actual and predicted match
  cat("Levels of actual for", outcome, ":", actual_levels, "\n")
  cat("Unique predictions for", outcome, ":", pred_levels, "\n")
  
  # If the levels don't match, ensure both are aligned
  if (!all(actual_levels %in% pred_levels)) {
    cat("Warning: Levels of actual values and predictions don't match for outcome:", outcome, "\n")
    actual_levels <- unique(c(actual_levels, pred_levels))
  }
  
  # Convert both actual and predicted to factors with matching levels
  actual <- factor(test_data_clean[[outcome]], levels = actual_levels)
  test_pred <- factor(test_preds_clean, levels = actual_levels)
  
  # Check if lengths match
  cat("Length of actual values for", outcome, ":", length(actual), "\n")
  cat("Length of predicted values for", outcome, ":", length(test_pred), "\n")
  
  # Calculate accuracy
  accuracy <- mean(test_pred == actual, na.rm = TRUE)  # Ignore NA values
  
  # Debugging Step: Output accuracy for the current outcome
  cat("Accuracy for outcome", outcome, ":", accuracy, "\n")
  
  # Return the accuracy for this outcome
  data.frame(Outcome = outcome, Accuracy = accuracy)
}

# Stop the parallel cluster
stopCluster(cl)

# Print test accuracy for all outcomes
cat("Test Accuracy for all outcomes:\n")
print(test_performance)

# Optionally, you can save it as an HTML or LaTeX file
stargazer(test_performance, type = "latex", 
          summary = FALSE, 
          rownames = FALSE, 
          title = "Accuracies for Random Forest Models - EES14", 
          out = "accuracies_table_14.tex")  # Save as LaTeX file

stargazer(test_performance, type = "html", 
          summary = FALSE, 
          rownames = FALSE, 
          title = "OOB R-Squared for Random Forest Models - EES14", 
          out = "accuracies_table_14.html")  # Save as HTML file

# Prepare OOB R-squared as a data frame and print it
oob_r_squared_df <- data.frame(
  Outcome = outcomes,
  OOB_R_Squared = unlist(oob_r_squared)
)
print("OOB R-squared for all outcomes:")
print(oob_r_squared_df)


# Optionally, you can save it as an HTML or LaTeX file
stargazer(oob_r_squared_df, type = "latex", 
          summary = FALSE, 
          rownames = FALSE, 
          title = "OOB R-Squared for Random Forest Models - EES14", 
          out = "oob_r_squared_table_14.tex")  # Save as LaTeX file

stargazer(oob_r_squared_df, type = "html", 
          summary = FALSE, 
          rownames = FALSE, 
          title = "OOB R-Squared for Random Forest Models - EES14", 
          out = "oob_r_squared_table_14.html")  # Save as HTML file


# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", 
                "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c("issue_state_int", "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Convert categorical variables to factors and check for missing values
data <- ees14
# data <- ees14 %>%
#   drop_na() %>%  # drop rows with NA
#   group_by(countryname) %>%
#   slice_sample(n = 500) %>%  # 500 obs for each country
#   ungroup()


# List of countries
countries <- unique(data$countryname)

# Function to train and test random forest model
train_and_predict <- function(train_data, test_data, outcomes) {
  results <- list()
  
  for (outcome in outcomes) {
    
    #tuned_params
    best_params <- best_results %>%
      filter(outcome == outcome_var) %>%
      dplyr::select(ntree, maxnodes) %>%
      unlist() %>%
      as.list()
    
    rf_model <- randomForest(
      as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
      data = train_data,
      na.action = na.omit,
      ntree = best_params$ntree,         
      maxnodes = best_params$maxnodes      
    )
    
    predictions <- predict(rf_model, newdata = test_data)
    predictions <- round(predictions)
    results[[paste(outcome, "pred", sep = "_")]] <- predictions
  }
  
  return(results)
}

# Function to calculate precision, recall, and F1-score for multi-class classification
calculate_performance_metrics <- function(confusion) {
  precision <- confusion$byClass[, "Pos Pred Value"]
  recall <- confusion$byClass[, "Sensitivity"]
  
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  
  fscore <- 2 * (precision * recall) / (precision + recall)
  fscore[is.na(fscore)] <- 0
  
  avg_precision <- mean(precision)
  avg_recall <- mean(recall)
  avg_fscore <- mean(fscore)
  
  accuracy <- confusion$overall["Accuracy"]
  
  return(data.frame(
    Accuracy = accuracy,
    Precision = avg_precision,
    Recall = avg_recall,
    FScore = avg_fscore
  ))
}

# Initialize parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Initialize list to store performance metrics
performance_metrics_all_countries <- list()

# Parallel loop over each country
tictoc::tic("Total Time")  # Start timing the whole process
performance_metrics_all_countries <- foreach(country = countries, .combine = 'rbind',
                                             .packages = c('dplyr', 'randomForest', 'caret', 'stringr', 'tictoc')) %dopar% {
                                               
                                               # Time each country
                                               tictoc::tic(paste("Country:", country))
                                               
                                               train_data <- data %>% filter(countryname != country)
                                               test_data <- data %>% filter(countryname == country)
                                               
                                               for (outcome in outcomes) {
                                                 test_data[[outcome]] <- factor(test_data[[outcome]], levels = 0:10)
                                               }
                                               
                                               predictions <- train_and_predict(train_data, test_data, outcomes)
                                               
                                               performance_metrics <- list()
                                               for (outcome in outcomes) {
                                                 actual <- factor(test_data[[outcome]], levels = 0:10)
                                                 predicted <- factor(predictions[[paste(outcome, "pred", sep = "_")]], levels = 0:10)
                                                 
                                                 confusion <- confusionMatrix(predicted, actual)
                                                 performance_metrics[[outcome]] <- calculate_performance_metrics(confusion)
                                               }
                                               
                                               # Stop timing for this country
                                               tictoc::toc(log = TRUE)
                                               
                                               bind_rows(performance_metrics, .id = "Issue") %>%
                                                 mutate(Country = country)
                                             }

# Stop timing the entire process
tictoc::toc()

# Stop parallel backend
stopCluster(cl)

# Convert the list to a data frame
performance_metrics_all_countries_df <- bind_rows(performance_metrics_all_countries)

# Save performance metrics
write.csv(performance_metrics_all_countries_df, "performance_metrics_all_countries14.csv", row.names = FALSE)

print("Performance metrics have been saved.")


# splits by country clusters

nwe_cluster <- c("DK", "FI", "SE", "AT", "BE", "FR", "DE", "IE", "LU", "NL", "UK")
ee_cluster <- c("BG", "HR", "CZ", "EE", "HU", "LV", "LT", "PL", "RO", "SK", "SI")
se_cluster <- c("CY", "EL", "IT", "MT", "PT", "ES")
eu15_cluster <- c("AT", "BE", "DK", "FI", "FR", "DE", "EL", "IE", "IT", "LU", "NL", "PT", "ES", "SE", "UK")

# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c( "issue_state_int", "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Split dataset
data <- ees14 %>% filter(countryname %in% nwe_cluster)

# List of countries
countries <- unique(data$countryname)

# Function to train and test random forest model
train_and_predict <- function(train_data, test_data, outcomes) {
  results <- list()
  
  for (outcome in outcomes) {
    #tuned_params
    best_params <- best_results %>%
      filter(outcome == outcome_var) %>%
      dplyr::select(ntree, maxnodes) %>%
      unlist() %>%
      as.list()
    
    rf_model <- randomForest(
      as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
      data = train_data,
      na.action = na.omit,
      ntree = best_params$ntree,         
      maxnodes = best_params$maxnodes
    )
    
    predictions <- predict(rf_model, newdata = test_data)
    predictions <- round(predictions)
    results[[paste(outcome, "pred", sep = "_")]] <- predictions
  }
  
  return(results)
}

# Function to calculate precision, recall, and F1-score for multi-class classification
calculate_performance_metrics <- function(confusion) {
  precision <- confusion$byClass[, "Pos Pred Value"]
  recall <- confusion$byClass[, "Sensitivity"]
  
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  
  fscore <- 2 * (precision * recall) / (precision + recall)
  fscore[is.na(fscore)] <- 0
  
  avg_precision <- mean(precision)
  avg_recall <- mean(recall)
  avg_fscore <- mean(fscore)
  
  accuracy <- confusion$overall["Accuracy"]
  
  return(data.frame(
    Accuracy = accuracy,
    Precision = avg_precision,
    Recall = avg_recall,
    FScore = avg_fscore
  ))
}

# Initialize parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Initialize list to store performance metrics
performance_metrics_nwe_countries <- list()

# Parallel loop over each country
tictoc::tic("Total Time")  # Start timing the whole process
performance_metrics_nwe_countries <- foreach(country = countries, .combine = 'rbind',
                                             .packages = c('dplyr', 'randomForest', 'caret', 'stringr', 'tictoc')) %dopar% {
                                               
                                               # Time each country
                                               tictoc::tic(paste("Country:", country))
                                               
                                               train_data <- data %>% filter(countryname != country)
                                               test_data <- data %>% filter(countryname == country)
                                               
                                               for (outcome in outcomes) {
                                                 test_data[[outcome]] <- factor(test_data[[outcome]], levels = 0:10)
                                               }
                                               
                                               predictions <- train_and_predict(train_data, test_data, outcomes)
                                               
                                               performance_metrics <- list()
                                               for (outcome in outcomes) {
                                                 actual <- factor(test_data[[outcome]], levels = 0:10)
                                                 predicted <- factor(predictions[[paste(outcome, "pred", sep = "_")]], levels = 0:10)
                                                 
                                                 confusion <- confusionMatrix(predicted, actual)
                                                 performance_metrics[[outcome]] <- calculate_performance_metrics(confusion)
                                               }
                                               
                                               # Stop timing for this country
                                               tictoc::toc(log = TRUE)
                                               
                                               bind_rows(performance_metrics, .id = "Issue") %>%
                                                 mutate(Country = country)
                                             }

# Stop timing the entire process
tictoc::toc()

# Stop parallel backend
stopCluster(cl)

# Convert the list to a data frame
performance_metrics_nwe_countries_df <- bind_rows(performance_metrics_nwe_countries)

# Save performance metrics
write.csv(performance_metrics_nwe_countries_df, "performance_metrics_nwe_countries14.csv", row.names = FALSE)

print("Performance metrics have been saved.")

# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c( "issue_state_int", "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Split dataset
data <- ees14 %>% filter(countryname %in% se_cluster)

# List of countries
countries <- unique(data$countryname)

# Function to train and test random forest model
train_and_predict <- function(train_data, test_data, outcomes) {
  results <- list()
  
  for (outcome in outcomes) {
    
    #tuned_params
    best_params <- best_results %>%
      filter(outcome == outcome_var) %>%
      dplyr::select(ntree, maxnodes) %>%
      unlist() %>%
      as.list()
    
    rf_model <- randomForest(
      as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
      data = train_data,
      na.action = na.omit,
      ntree = best_params$ntree,         
      maxnodes = best_params$maxnodes
    )
    
    predictions <- predict(rf_model, newdata = test_data)
    predictions <- round(predictions)
    results[[paste(outcome, "pred", sep = "_")]] <- predictions
  }
  
  return(results)
}

# Function to calculate precision, recall, and F1-score for multi-class classification
calculate_performance_metrics <- function(confusion) {
  precision <- confusion$byClass[, "Pos Pred Value"]
  recall <- confusion$byClass[, "Sensitivity"]
  
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  
  fscore <- 2 * (precision * recall) / (precision + recall)
  fscore[is.na(fscore)] <- 0
  
  avg_precision <- mean(precision)
  avg_recall <- mean(recall)
  avg_fscore <- mean(fscore)
  
  accuracy <- confusion$overall["Accuracy"]
  
  return(data.frame(
    Accuracy = accuracy,
    Precision = avg_precision,
    Recall = avg_recall,
    FScore = avg_fscore
  ))
}

# Initialize parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Initialize list to store performance metrics
performance_metrics_se_countries <- list()

# Parallel loop over each country
tictoc::tic("Total Time")  # Start timing the whole process
performance_metrics_se_countries <- foreach(country = countries, .combine = 'rbind',
                                            .packages = c('dplyr', 'randomForest', 'caret', 'stringr', 'tictoc')) %dopar% {
                                              
                                              # Time each country
                                              tictoc::tic(paste("Country:", country))
                                              
                                              train_data <- data %>% filter(countryname != country)
                                              test_data <- data %>% filter(countryname == country)
                                              
                                              for (outcome in outcomes) {
                                                test_data[[outcome]] <- factor(test_data[[outcome]], levels = 0:10)
                                              }
                                              
                                              predictions <- train_and_predict(train_data, test_data, outcomes)
                                              
                                              performance_metrics <- list()
                                              for (outcome in outcomes) {
                                                actual <- factor(test_data[[outcome]], levels = 0:10)
                                                predicted <- factor(predictions[[paste(outcome, "pred", sep = "_")]], levels = 0:10)
                                                
                                                confusion <- confusionMatrix(predicted, actual)
                                                performance_metrics[[outcome]] <- calculate_performance_metrics(confusion)
                                              }
                                              
                                              # Stop timing for this country
                                              tictoc::toc(log = TRUE)
                                              
                                              bind_rows(performance_metrics, .id = "Issue") %>%
                                                mutate(Country = country)
                                            }

# Stop timing the entire process
tictoc::toc()

# Stop parallel backend
stopCluster(cl)

# Convert the list to a data frame
performance_metrics_se_countries_df <- bind_rows(performance_metrics_se_countries)

# Save performance metrics
write.csv(performance_metrics_se_countries_df, "performance_metrics_se_countries14.csv", row.names = FALSE)

print("Performance metrics have been saved.")

# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c( "issue_state_int", "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Split dataset
data <- ees14 %>% filter(countryname %in% ee_cluster)

# List of countries
countries <- unique(data$countryname)

# Function to train and test random forest model
train_and_predict <- function(train_data, test_data, outcomes) {
  results <- list()
  
  for (outcome in outcomes) {
    #tuned_params
    best_params <- best_results %>%
      filter(outcome == outcome_var) %>%
      dplyr::select(ntree, maxnodes) %>%
      unlist() %>%
      as.list()
    
    rf_model <- randomForest(
      as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
      data = train_data,
      na.action = na.omit,
      ntree = best_params$ntree,         
      maxnodes = best_params$maxnodes
    )
    
    predictions <- predict(rf_model, newdata = test_data)
    predictions <- round(predictions)
    results[[paste(outcome, "pred", sep = "_")]] <- predictions
  }
  
  return(results)
}

# Function to calculate precision, recall, and F1-score for multi-class classification
calculate_performance_metrics <- function(confusion) {
  precision <- confusion$byClass[, "Pos Pred Value"]
  recall <- confusion$byClass[, "Sensitivity"]
  
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  
  fscore <- 2 * (precision * recall) / (precision + recall)
  fscore[is.na(fscore)] <- 0
  
  avg_precision <- mean(precision)
  avg_recall <- mean(recall)
  avg_fscore <- mean(fscore)
  
  accuracy <- confusion$overall["Accuracy"]
  
  return(data.frame(
    Accuracy = accuracy,
    Precision = avg_precision,
    Recall = avg_recall,
    FScore = avg_fscore
  ))
}

# Initialize parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Initialize list to store performance metrics
performance_metrics_ee_countries <- list()

# Parallel loop over each country
tictoc::tic("Total Time")  # Start timing the whole process
performance_metrics_ee_countries <- foreach(country = countries, .combine = 'rbind',
                                            .packages = c('dplyr', 'randomForest', 'caret', 'stringr', 'tictoc')) %dopar% {
                                              
                                              # Time each country
                                              tictoc::tic(paste("Country:", country))
                                              
                                              train_data <- data %>% filter(countryname != country)
                                              test_data <- data %>% filter(countryname == country)
                                              
                                              for (outcome in outcomes) {
                                                test_data[[outcome]] <- factor(test_data[[outcome]], levels = 0:10)
                                              }
                                              
                                              predictions <- train_and_predict(train_data, test_data, outcomes)
                                              
                                              performance_metrics <- list()
                                              for (outcome in outcomes) {
                                                actual <- factor(test_data[[outcome]], levels = 0:10)
                                                predicted <- factor(predictions[[paste(outcome, "pred", sep = "_")]], levels = 0:10)
                                                
                                                confusion <- confusionMatrix(predicted, actual)
                                                performance_metrics[[outcome]] <- calculate_performance_metrics(confusion)
                                              }
                                              
                                              # Stop timing for this country
                                              tictoc::toc(log = TRUE)
                                              
                                              bind_rows(performance_metrics, .id = "Issue") %>%
                                                mutate(Country = country)
                                            }

# Stop timing the entire process
tictoc::toc()

# Stop parallel backend
stopCluster(cl)

# Convert the list to a data frame
performance_metrics_ee_countries_df <- bind_rows(performance_metrics_ee_countries)

# Save performance metrics
write.csv(performance_metrics_ee_countries_df, "performance_metrics_ee_countries14.csv", row.names = FALSE)

print("Performance metrics have been saved.")

# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c("issue_state_int",  "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Split dataset
data <- ees14 %>% filter(countryname %in% eu15_cluster)

# List of countries
countries <- unique(data$countryname)

# Function to train and test random forest model
train_and_predict <- function(train_data, test_data, outcomes) {
  results <- list()
  
  for (outcome in outcomes) {
    #tuned_params
    best_params <- best_results %>%
      filter(outcome == outcome_var) %>%
      dplyr::select(ntree, maxnodes) %>%
      unlist() %>%
      as.list()
    
    rf_model <- randomForest(
      as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
      data = train_data,
      na.action = na.omit,
      ntree = best_params$ntree,         
      maxnodes = best_params$maxnodes
    )
    
    predictions <- predict(rf_model, newdata = test_data)
    predictions <- round(predictions)
    results[[paste(outcome, "pred", sep = "_")]] <- predictions
  }
  
  return(results)
}

# Function to calculate precision, recall, and F1-score for multi-class classification
calculate_performance_metrics <- function(confusion) {
  precision <- confusion$byClass[, "Pos Pred Value"]
  recall <- confusion$byClass[, "Sensitivity"]
  
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  
  fscore <- 2 * (precision * recall) / (precision + recall)
  fscore[is.na(fscore)] <- 0
  
  avg_precision <- mean(precision)
  avg_recall <- mean(recall)
  avg_fscore <- mean(fscore)
  
  accuracy <- confusion$overall["Accuracy"]
  
  return(data.frame(
    Accuracy = accuracy,
    Precision = avg_precision,
    Recall = avg_recall,
    FScore = avg_fscore
  ))
}

# Initialize parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Initialize list to store performance metrics
performance_metrics_eu15_countries <- list()

# Parallel loop over each country
tictoc::tic("Total Time")  # Start timing the whole process
performance_metrics_eu15_countries <- foreach(country = countries, .combine = 'rbind',
                                              .packages = c('dplyr', 'randomForest', 'caret', 'stringr', 'tictoc')) %dopar% {
                                                
                                                # Time each country
                                                tictoc::tic(paste("Country:", country))
                                                
                                                train_data <- data %>% filter(countryname != country)
                                                test_data <- data %>% filter(countryname == country)
                                                
                                                for (outcome in outcomes) {
                                                  test_data[[outcome]] <- factor(test_data[[outcome]], levels = 0:10)
                                                }
                                                
                                                predictions <- train_and_predict(train_data, test_data, outcomes)
                                                
                                                performance_metrics <- list()
                                                for (outcome in outcomes) {
                                                  actual <- factor(test_data[[outcome]], levels = 0:10)
                                                  predicted <- factor(predictions[[paste(outcome, "pred", sep = "_")]], levels = 0:10)
                                                  
                                                  confusion <- confusionMatrix(predicted, actual)
                                                  performance_metrics[[outcome]] <- calculate_performance_metrics(confusion)
                                                }
                                                
                                                # Stop timing for this country
                                                tictoc::toc(log = TRUE)
                                                
                                                bind_rows(performance_metrics, .id = "Issue") %>%
                                                  mutate(Country = country)
                                              }

# Stop timing the entire process
tictoc::toc()

# Stop parallel backend
stopCluster(cl)

# Convert the list to a data frame
performance_metrics_eu15_countries_df <- bind_rows(performance_metrics_eu15_countries)

# Save performance metrics
write.csv(performance_metrics_eu15_countries_df, "performance_metrics_eu15_countries14.csv", row.names = FALSE)

print("Performance metrics have been saved.")

# 2019 ------------------------------------------------

# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", 
                "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c("issue_state_int", "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Dictionary for cleaning outcome names
issue_names <- c(
  issue_state_int = "Attitudes on State Intervention in the Economy",
  issue_redistr = "Attitudes on Redistribution",
  issue_clim = "Attitudes on Environmental Protection",
  issue_samesex = "Attitudes on Same-Sex Marriage",
  issue_immig = "Attitudes on Immigration"
)

# Convert categorical variables to factors if needed (check and preprocess)
data <- ees19  

# Remove any rows with missing data
data <- drop_na(data)

# Convert outcome variables to factors in the dataset
for (outcome in outcomes) {
  data[[outcome]] <- factor(data[[outcome]], levels = 0:10)
}

# Split the data into 80% training and 20% testing
set.seed(123)  # Set seed for reproducibility
train_index <- createDataPartition(data[[outcomes[1]]], p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


# -------------------------
# Tuning Random Forest for All Outcomes
# -------------------------

# Grid of parameters to tune
ntree_vals <- c(100, 200, 400, 800)
maxnodes_vals <- c(10, 20, 40, 80)

# Store results in a data frame
all_results <- data.frame(
  outcome = character(),
  ntree = integer(),
  maxnodes = integer(),
  accuracy = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each outcome
for (outcome_var in outcomes) {
  print(paste("Tuning Random Forest for outcome:", outcome_var))
  
  # Loop through combinations of ntree and maxnodes
  for (ntree in ntree_vals) {
    for (maxnodes in maxnodes_vals) {
      
      # Train Random Forest model
      rf_model <- randomForest(
        as.formula(paste(outcome_var, "~", paste(predictors, collapse = "+"))),
        data = train_data,
        ntree = ntree,
        maxnodes = maxnodes
      )
      
      # Predict on test data
      preds <- predict(rf_model, newdata = test_data)
      
      # Calculate accuracy
      acc <- mean(preds == test_data[[outcome_var]])
      
      # Store results
      all_results <- rbind(all_results, data.frame(
        outcome = outcome_var,
        ntree = ntree,
        maxnodes = maxnodes,
        accuracy = acc
      ))
    }
  }
}

# View results
print("Best Parameters for Each Outcome - 2019:")
best_results <- all_results %>%
  group_by(outcome) %>%
  filter(accuracy == max(accuracy)) %>%
  ungroup()

print(best_results)

## Overall Model Performance
# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", 
                "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c("issue_state_int", "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Convert categorical variables to factors if needed (check and preprocess)
data <- ees19  # Assuming ees14 is your main dataset

# # Remove any rows with missing data
# data <- drop_na(data)

# Convert outcome variables to factors in the dataset
for (outcome in outcomes) {
  data[[outcome]] <- factor(data[[outcome]], levels = 0:10)
}

# Split the data into 80% training and 20% testing
set.seed(123)  # Set seed for reproducibility
train_index <- createDataPartition(data[[outcomes[1]]], p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Initialize a list to store the models, variable importances, and OOB R-squared values
rf_models <- list()
variable_importances <- list()
oob_r_squared <- list()

# Loop over each outcome and train a random forest model
for (outcome in outcomes) {
  #tuned_params
  best_params <- best_results %>%
    filter(outcome == outcome_var) %>%
    dplyr::select(ntree, maxnodes) %>%
    unlist() %>%
    as.list()
  
  # Train the random forest model for classification
  rf_model <- randomForest(
    as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
    data = train_data,
    na.action = na.omit,
    ntree = best_params$ntree,          # Number of trees
    maxnodes = best_params$maxnodes,        # Max nodes
    importance = TRUE,    # Enable variable importance calculation
    keep.inbag = TRUE     # Keep OOB predictions for OOB R-squared
  )
  
  # Store the model in the list
  rf_models[[outcome]] <- rf_model
  
  # Calculate variable importance and store it
  importance_scores <- importance(rf_model)
  variable_importances[[outcome]] <- importance_scores
  
  # Calculate OOB R-squared
  oob_error_rate <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
  r_squared_oob <- 1 - oob_error_rate
  oob_r_squared[[outcome]] <- r_squared_oob
  
  # Print OOB R-squared
  cat("OOB R-squared for", outcome, ":", r_squared_oob, "\n")
}

# Plot and save Variable Importance for each outcome
for (outcome in outcomes) {
  cat("\nVariable Importance for", outcome, ":\n")
  
  # Define the file name for saving the plot
  file_name <- paste0("vip_", outcome, "_19.pdf")
  
  # Open a PDF device to save the plot
  pdf(file = file_name)
  
  # Plot the variable importance using base R plot
  varImpPlot(rf_models[[outcome]], main = paste("Variable Importance for", issue_names[[outcome]], "EES 2019"))
  
  # Close the PDF device
  dev.off()
}

cat("Variable importance plots have been saved.\n")

# Detect cores and register parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Parallel loop over each outcome to predict and calculate performance
test_performance <- foreach(outcome = outcomes, .combine = 'rbind', .packages = c('randomForest', 'caret')) %dopar% {
  
  # Predict on test data
  test_pred <- predict(rf_models[[outcome]], newdata = test_data, type = "response")
  
  # Debugging Step: Print the first few predictions
  cat("Predictions for outcome", outcome, ":", head(test_pred), "\n")
  
  # Remove rows with missing actual outcomes in test_data, keep the rest of the data intact
  test_data_clean <- test_data[!is.na(test_data[[outcome]]), ]
  test_preds_clean <- test_pred[!is.na(test_data[[outcome]])]
  
  # Debugging Step: Check how many rows are left after cleaning
  cat("Rows left for outcome", outcome, "after cleaning:", nrow(test_data_clean), "\n")
  cat("Predictions for outcome", outcome, "after cleaning:", length(test_preds_clean), "\n")
  
  # Ensure lengths match
  if (length(test_preds_clean) != nrow(test_data_clean)) {
    stop(paste("Mismatch in number of predictions and actual outcomes for outcome:", outcome))
  }
  
  # Check levels of actual and predicted before factor conversion
  actual_levels <- levels(factor(test_data_clean[[outcome]]))
  pred_levels <- unique(test_preds_clean)
  
  # Debugging Step: Check if the levels of actual and predicted match
  cat("Levels of actual for", outcome, ":", actual_levels, "\n")
  cat("Unique predictions for", outcome, ":", pred_levels, "\n")
  
  # If the levels don't match, ensure both are aligned
  if (!all(actual_levels %in% pred_levels)) {
    cat("Warning: Levels of actual values and predictions don't match for outcome:", outcome, "\n")
    actual_levels <- unique(c(actual_levels, pred_levels))
  }
  
  # Convert both actual and predicted to factors with matching levels
  actual <- factor(test_data_clean[[outcome]], levels = actual_levels)
  test_pred <- factor(test_preds_clean, levels = actual_levels)
  
  # Check if lengths match
  cat("Length of actual values for", outcome, ":", length(actual), "\n")
  cat("Length of predicted values for", outcome, ":", length(test_pred), "\n")
  
  # Calculate accuracy
  accuracy <- mean(test_pred == actual, na.rm = TRUE)  # Ignore NA values
  
  # Debugging Step: Output accuracy for the current outcome
  cat("Accuracy for outcome", outcome, ":", accuracy, "\n")
  
  # Return the accuracy for this outcome
  data.frame(Outcome = outcome, Accuracy = accuracy)
}

# Stop the parallel cluster
stopCluster(cl)

# Print test accuracy for all outcomes
cat("Test Accuracy for all outcomes:\n")
print(test_performance)

# Optionally, you can save it as an HTML or LaTeX file
stargazer(test_performance, type = "latex", 
          summary = FALSE, 
          rownames = FALSE, 
          title = "Accuracies for Random Forest Models - EES19", 
          out = "accuracies_table_19.tex")  # Save as LaTeX file

stargazer(test_performance, type = "html", 
          summary = FALSE, 
          rownames = FALSE, 
          title = "OOB R-Squared for Random Forest Models - EES19", 
          out = "accuracies_table_19.html")  # Save as HTML file

# Prepare OOB R-squared as a data frame and print it
oob_r_squared_df <- data.frame(
  Outcome = outcomes,
  OOB_R_Squared = unlist(oob_r_squared)
)
print("OOB R-squared for all outcomes:")
print(oob_r_squared_df)


# Optionally, you can save it as an HTML or LaTeX file
stargazer(oob_r_squared_df, type = "latex", 
          summary = FALSE, 
          rownames = FALSE, 
          title = "OOB R-Squared for Random Forest Models - EES19", 
          out = "oob_r_squared_table_19.tex")  # Save as LaTeX file

stargazer(oob_r_squared_df, type = "html", 
          summary = FALSE, 
          rownames = FALSE, 
          title = "OOB R-Squared for Random Forest Models - EES19", 
          out = "oob_r_squared_table_19.html")  # Save as HTML file

# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", 
                "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c( "issue_state_int", "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Convert categorical variables to factors and check for missing values
data <- ees19
# data <- ees19 %>%
#   drop_na() %>%  # drop rows with NA
#   group_by(countryname) %>%
#   slice_sample(n = 500) %>%  # 500 obs for each country
#   ungroup()


# List of countries
countries <- unique(data$countryname)

# Function to train and test random forest model
train_and_predict <- function(train_data, test_data, outcomes) {
  results <- list()
  
  for (outcome in outcomes) {
    #tuned_params
    best_params <- best_results %>%
      filter(outcome == outcome_var) %>%
      dplyr::select(ntree, maxnodes) %>%
      unlist() %>%
      as.list()
    
    rf_model <- randomForest(
      as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
      data = train_data,
      na.action = na.omit,
      ntree = best_params$ntree,         
      maxnodes = best_params$maxnodes
    )
    
    predictions <- predict(rf_model, newdata = test_data)
    predictions <- round(predictions)
    results[[paste(outcome, "pred", sep = "_")]] <- predictions
  }
  
  return(results)
}

# Function to calculate precision, recall, and F1-score for multi-class classification
calculate_performance_metrics <- function(confusion) {
  precision <- confusion$byClass[, "Pos Pred Value"]
  recall <- confusion$byClass[, "Sensitivity"]
  
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  
  fscore <- 2 * (precision * recall) / (precision + recall)
  fscore[is.na(fscore)] <- 0
  
  avg_precision <- mean(precision)
  avg_recall <- mean(recall)
  avg_fscore <- mean(fscore)
  
  accuracy <- confusion$overall["Accuracy"]
  
  return(data.frame(
    Accuracy = accuracy,
    Precision = avg_precision,
    Recall = avg_recall,
    FScore = avg_fscore
  ))
}

# Initialize parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Initialize list to store performance metrics
performance_metrics_all_countries <- list()

# Parallel loop over each country
tictoc::tic("Total Time")  # Start timing the whole process
performance_metrics_all_countries <- foreach(country = countries, .combine = 'rbind',
                                             .packages = c('dplyr', 'randomForest', 'caret', 'stringr', 'tictoc')) %dopar% {
                                               
                                               # Time each country
                                               tictoc::tic(paste("Country:", country))
                                               
                                               train_data <- data %>% filter(countryname != country)
                                               test_data <- data %>% filter(countryname == country)
                                               
                                               for (outcome in outcomes) {
                                                 test_data[[outcome]] <- factor(test_data[[outcome]], levels = 0:10)
                                               }
                                               
                                               predictions <- train_and_predict(train_data, test_data, outcomes)
                                               
                                               performance_metrics <- list()
                                               for (outcome in outcomes) {
                                                 actual <- factor(test_data[[outcome]], levels = 0:10)
                                                 predicted <- factor(predictions[[paste(outcome, "pred", sep = "_")]], levels = 0:10)
                                                 
                                                 confusion <- confusionMatrix(predicted, actual)
                                                 performance_metrics[[outcome]] <- calculate_performance_metrics(confusion)
                                               }
                                               
                                               # Stop timing for this country
                                               tictoc::toc(log = TRUE)
                                               
                                               bind_rows(performance_metrics, .id = "Issue") %>%
                                                 mutate(Country = country)
                                             }

# Stop timing the entire process
tictoc::toc()

# Stop parallel backend
stopCluster(cl)

# Convert the list to a data frame
performance_metrics_all_countries_df <- bind_rows(performance_metrics_all_countries)

# Save performance metrics
write.csv(performance_metrics_all_countries_df, "performance_metrics_all_countries19.csv", row.names = FALSE)

print("Performance metrics have been saved.")

# splits by country clusters

nwe_cluster <- c("DK", "FI", "SE", "AT", "BE", "FR", "DE", "IE", "LU", "NL", "UK")
ee_cluster <- c("BG", "HR", "CZ", "EE", "HU", "LV", "LT", "PL", "RO", "SK", "SI")
se_cluster <- c("CY", "EL", "IT", "MT", "PT", "ES")
eu15_cluster <- c("AT", "BE", "DK", "FI", "FR", "DE", "EL", "IE", "IT", "LU", "NL", "PT", "ES", "SE", "UK")

# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c( "issue_state_int", "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Split dataset
data <- ees19 %>% filter(countryname %in% nwe_cluster)

# List of countries
countries <- unique(data$countryname)

# Function to train and test random forest model
train_and_predict <- function(train_data, test_data, outcomes) {
  results <- list()
  
  for (outcome in outcomes) {
    #tuned_params
    best_params <- best_results %>%
      filter(outcome == outcome_var) %>%
      dplyr::select(ntree, maxnodes) %>%
      unlist() %>%
      as.list()
    
    rf_model <- randomForest(
      as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
      data = train_data,
      na.action = na.omit,
      ntree = best_params$ntree,         
      maxnodes = best_params$maxnodes
      
    )
    
    predictions <- predict(rf_model, newdata = test_data)
    predictions <- round(predictions)
    results[[paste(outcome, "pred", sep = "_")]] <- predictions
  }
  
  return(results)
}

# Function to calculate precision, recall, and F1-score for multi-class classification
calculate_performance_metrics <- function(confusion) {
  precision <- confusion$byClass[, "Pos Pred Value"]
  recall <- confusion$byClass[, "Sensitivity"]
  
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  
  fscore <- 2 * (precision * recall) / (precision + recall)
  fscore[is.na(fscore)] <- 0
  
  avg_precision <- mean(precision)
  avg_recall <- mean(recall)
  avg_fscore <- mean(fscore)
  
  accuracy <- confusion$overall["Accuracy"]
  
  return(data.frame(
    Accuracy = accuracy,
    Precision = avg_precision,
    Recall = avg_recall,
    FScore = avg_fscore
  ))
}

# Initialize parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Initialize list to store performance metrics
performance_metrics_nwe_countries <- list()

# Parallel loop over each country
tictoc::tic("Total Time")  # Start timing the whole process
performance_metrics_nwe_countries <- foreach(country = countries, .combine = 'rbind',
                                             .packages = c('dplyr', 'randomForest', 'caret', 'stringr', 'tictoc')) %dopar% {
                                               
                                               # Time each country
                                               tictoc::tic(paste("Country:", country))
                                               
                                               train_data <- data %>% filter(countryname != country)
                                               test_data <- data %>% filter(countryname == country)
                                               
                                               for (outcome in outcomes) {
                                                 test_data[[outcome]] <- factor(test_data[[outcome]], levels = 0:10)
                                               }
                                               
                                               predictions <- train_and_predict(train_data, test_data, outcomes)
                                               
                                               performance_metrics <- list()
                                               for (outcome in outcomes) {
                                                 actual <- factor(test_data[[outcome]], levels = 0:10)
                                                 predicted <- factor(predictions[[paste(outcome, "pred", sep = "_")]], levels = 0:10)
                                                 
                                                 confusion <- confusionMatrix(predicted, actual)
                                                 performance_metrics[[outcome]] <- calculate_performance_metrics(confusion)
                                               }
                                               
                                               # Stop timing for this country
                                               tictoc::toc(log = TRUE)
                                               
                                               bind_rows(performance_metrics, .id = "Issue") %>%
                                                 mutate(Country = country)
                                             }

# Stop timing the entire process
tictoc::toc()

# Stop parallel backend
stopCluster(cl)

# Convert the list to a data frame
performance_metrics_nwe_countries_df <- bind_rows(performance_metrics_nwe_countries)

# Save performance metrics
write.csv(performance_metrics_nwe_countries_df, "performance_metrics_nwe_countries19.csv", row.names = FALSE)

print("Performance metrics have been saved.")

# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c("issue_state_int","issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Split dataset
data <- ees19 %>% filter(countryname %in% se_cluster)

# List of countries
countries <- unique(data$countryname)

# Function to train and test random forest model
train_and_predict <- function(train_data, test_data, outcomes) {
  results <- list()
  
  for (outcome in outcomes) {
    #tuned_params
    best_params <- best_results %>%
      filter(outcome == outcome_var) %>%
      dplyr::select(ntree, maxnodes) %>%
      unlist() %>%
      as.list()
    
    rf_model <- randomForest(
      as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
      data = train_data,
      na.action = na.omit,
      ntree = best_params$ntree,         
      maxnodes = best_params$maxnodes
    )
    
    predictions <- predict(rf_model, newdata = test_data)
    predictions <- round(predictions)
    results[[paste(outcome, "pred", sep = "_")]] <- predictions
  }
  
  return(results)
}

# Function to calculate precision, recall, and F1-score for multi-class classification
calculate_performance_metrics <- function(confusion) {
  precision <- confusion$byClass[, "Pos Pred Value"]
  recall <- confusion$byClass[, "Sensitivity"]
  
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  
  fscore <- 2 * (precision * recall) / (precision + recall)
  fscore[is.na(fscore)] <- 0
  
  avg_precision <- mean(precision)
  avg_recall <- mean(recall)
  avg_fscore <- mean(fscore)
  
  accuracy <- confusion$overall["Accuracy"]
  
  return(data.frame(
    Accuracy = accuracy,
    Precision = avg_precision,
    Recall = avg_recall,
    FScore = avg_fscore
  ))
}

# Initialize parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Initialize list to store performance metrics
performance_metrics_se_countries <- list()

# Parallel loop over each country
tictoc::tic("Total Time")  # Start timing the whole process
performance_metrics_se_countries <- foreach(country = countries, .combine = 'rbind',
                                            .packages = c('dplyr', 'randomForest', 'caret', 'stringr', 'tictoc')) %dopar% {
                                              
                                              # Time each country
                                              tictoc::tic(paste("Country:", country))
                                              
                                              train_data <- data %>% filter(countryname != country)
                                              test_data <- data %>% filter(countryname == country)
                                              
                                              for (outcome in outcomes) {
                                                test_data[[outcome]] <- factor(test_data[[outcome]], levels = 0:10)
                                              }
                                              
                                              predictions <- train_and_predict(train_data, test_data, outcomes)
                                              
                                              performance_metrics <- list()
                                              for (outcome in outcomes) {
                                                actual <- factor(test_data[[outcome]], levels = 0:10)
                                                predicted <- factor(predictions[[paste(outcome, "pred", sep = "_")]], levels = 0:10)
                                                
                                                confusion <- confusionMatrix(predicted, actual)
                                                performance_metrics[[outcome]] <- calculate_performance_metrics(confusion)
                                              }
                                              
                                              # Stop timing for this country
                                              tictoc::toc(log = TRUE)
                                              
                                              bind_rows(performance_metrics, .id = "Issue") %>%
                                                mutate(Country = country)
                                            }

# Stop timing the entire process
tictoc::toc()

# Stop parallel backend
stopCluster(cl)

# Convert the list to a data frame
performance_metrics_se_countries_df <- bind_rows(performance_metrics_se_countries)

# Save performance metrics
write.csv(performance_metrics_se_countries_df, "performance_metrics_se_countries19.csv", row.names = FALSE)

print("Performance metrics have been saved.")

# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c( "issue_state_int", "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Split dataset
data <- ees19 %>% filter(countryname %in% ee_cluster)

# List of countries
countries <- unique(data$countryname)

# Function to train and test random forest model
train_and_predict <- function(train_data, test_data, outcomes) {
  results <- list()
  
  for (outcome in outcomes) {
    #tuned_params
    best_params <- best_results %>%
      filter(outcome == outcome_var) %>%
      dplyr::select(ntree, maxnodes) %>%
      unlist() %>%
      as.list()
    
    
    rf_model <- randomForest(
      as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
      data = train_data,
      na.action = na.omit,
      ntree = best_params$ntree,         
      maxnodes = best_params$maxnodes
    )
    
    predictions <- predict(rf_model, newdata = test_data)
    predictions <- round(predictions)
    results[[paste(outcome, "pred", sep = "_")]] <- predictions
  }
  
  return(results)
}

# Function to calculate precision, recall, and F1-score for multi-class classification
calculate_performance_metrics <- function(confusion) {
  precision <- confusion$byClass[, "Pos Pred Value"]
  recall <- confusion$byClass[, "Sensitivity"]
  
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  
  fscore <- 2 * (precision * recall) / (precision + recall)
  fscore[is.na(fscore)] <- 0
  
  avg_precision <- mean(precision)
  avg_recall <- mean(recall)
  avg_fscore <- mean(fscore)
  
  accuracy <- confusion$overall["Accuracy"]
  
  return(data.frame(
    Accuracy = accuracy,
    Precision = avg_precision,
    Recall = avg_recall,
    FScore = avg_fscore
  ))
}

# Initialize parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Initialize list to store performance metrics
performance_metrics_ee_countries <- list()

# Parallel loop over each country
tictoc::tic("Total Time")  # Start timing the whole process
performance_metrics_ee_countries <- foreach(country = countries, .combine = 'rbind',
                                            .packages = c('dplyr', 'randomForest', 'caret', 'stringr', 'tictoc')) %dopar% {
                                              
                                              # Time each country
                                              tictoc::tic(paste("Country:", country))
                                              
                                              train_data <- data %>% filter(countryname != country)
                                              test_data <- data %>% filter(countryname == country)
                                              
                                              for (outcome in outcomes) {
                                                test_data[[outcome]] <- factor(test_data[[outcome]], levels = 0:10)
                                              }
                                              
                                              predictions <- train_and_predict(train_data, test_data, outcomes)
                                              
                                              performance_metrics <- list()
                                              for (outcome in outcomes) {
                                                actual <- factor(test_data[[outcome]], levels = 0:10)
                                                predicted <- factor(predictions[[paste(outcome, "pred", sep = "_")]], levels = 0:10)
                                                
                                                confusion <- confusionMatrix(predicted, actual)
                                                performance_metrics[[outcome]] <- calculate_performance_metrics(confusion)
                                              }
                                              
                                              # Stop timing for this country
                                              tictoc::toc(log = TRUE)
                                              
                                              bind_rows(performance_metrics, .id = "Issue") %>%
                                                mutate(Country = country)
                                            }

# Stop timing the entire process
tictoc::toc()

# Stop parallel backend
stopCluster(cl)

# Convert the list to a data frame
performance_metrics_ee_countries_df <- bind_rows(performance_metrics_ee_countries)

# Save performance metrics
write.csv(performance_metrics_ee_countries_df, "performance_metrics_ee_countries19.csv", row.names = FALSE)

print("Performance metrics have been saved.")

# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c("issue_state_int","issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Split dataset
data <- ees19 %>% filter(countryname %in% eu15_cluster)

# List of countries
countries <- unique(data$countryname)

# Function to train and test random forest model
train_and_predict <- function(train_data, test_data, outcomes) {
  results <- list()
  
  for (outcome in outcomes) {
    #tuned_params
    best_params <- best_results %>%
      filter(outcome == outcome_var) %>%
      dplyr::select(ntree, maxnodes) %>%
      unlist() %>%
      as.list()
    
    rf_model <- randomForest(
      as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
      data = train_data,
      na.action = na.omit,
      ntree = best_params$ntree,         
      maxnodes = best_params$maxnodes
    )
    
    predictions <- predict(rf_model, newdata = test_data)
    predictions <- round(predictions)
    results[[paste(outcome, "pred", sep = "_")]] <- predictions
  }
  
  return(results)
}

# Function to calculate precision, recall, and F1-score for multi-class classification
calculate_performance_metrics <- function(confusion) {
  precision <- confusion$byClass[, "Pos Pred Value"]
  recall <- confusion$byClass[, "Sensitivity"]
  
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  
  fscore <- 2 * (precision * recall) / (precision + recall)
  fscore[is.na(fscore)] <- 0
  
  avg_precision <- mean(precision)
  avg_recall <- mean(recall)
  avg_fscore <- mean(fscore)
  
  accuracy <- confusion$overall["Accuracy"]
  
  return(data.frame(
    Accuracy = accuracy,
    Precision = avg_precision,
    Recall = avg_recall,
    FScore = avg_fscore
  ))
}

# Initialize parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Initialize list to store performance metrics
performance_metrics_eu15_countries <- list()

# Parallel loop over each country
tictoc::tic("Total Time")  # Start timing the whole process
performance_metrics_eu15_countries <- foreach(country = countries, .combine = 'rbind',
                                              .packages = c('dplyr', 'randomForest', 'caret', 'stringr', 'tictoc')) %dopar% {
                                                
                                                # Time each country
                                                tictoc::tic(paste("Country:", country))
                                                
                                                train_data <- data %>% filter(countryname != country)
                                                test_data <- data %>% filter(countryname == country)
                                                
                                                for (outcome in outcomes) {
                                                  test_data[[outcome]] <- factor(test_data[[outcome]], levels = 0:10)
                                                }
                                                
                                                predictions <- train_and_predict(train_data, test_data, outcomes)
                                                
                                                performance_metrics <- list()
                                                for (outcome in outcomes) {
                                                  actual <- factor(test_data[[outcome]], levels = 0:10)
                                                  predicted <- factor(predictions[[paste(outcome, "pred", sep = "_")]], levels = 0:10)
                                                  
                                                  confusion <- confusionMatrix(predicted, actual)
                                                  performance_metrics[[outcome]] <- calculate_performance_metrics(confusion)
                                                }
                                                
                                                # Stop timing for this country
                                                tictoc::toc(log = TRUE)
                                                
                                                bind_rows(performance_metrics, .id = "Issue") %>%
                                                  mutate(Country = country)
                                              }

# Stop timing the entire process
tictoc::toc()

# Stop parallel backend
stopCluster(cl)

# Convert the list to a data frame
performance_metrics_eu15_countries_df <- bind_rows(performance_metrics_eu15_countries)

# Save performance metrics
write.csv(performance_metrics_eu15_countries_df, "performance_metrics_eu15_countries19.csv", row.names = FALSE)

print("Performance metrics have been saved.")

# 2024 -------------------------------------------------------------------------

# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", 
                "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c("issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Convert categorical variables to factors and check for missing values
data <- ees24

## Remove rows with any missing predictor values only (outcomes + predictors lead to all 0s)
data <- data %>% drop_na(all_of(predictors))

# Convert outcome variables to factors in the dataset
for (outcome in outcomes) {
  data[[outcome]] <- factor(data[[outcome]], levels = 0:10)
}

# Split the data into 80% training and 20% testing
set.seed(123)  # Set seed for reproducibility
train_index <- createDataPartition(data[[outcomes[1]]], p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# -------------------------
# Tuning Random Forest for All Outcomes
# -------------------------

# Grid of parameters to tune
ntree_vals <- c(100, 200, 400, 800)
maxnodes_vals <- c(10, 20, 40, 80)

# Store results in a data frame
all_results <- data.frame(
  outcome = character(),
  ntree = integer(),
  maxnodes = integer(),
  accuracy = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each outcome
for (outcome_var in outcomes) {
  print(paste("Tuning Random Forest for outcome:", outcome_var))
  
  # Loop through combinations of ntree and maxnodes
  for (ntree in ntree_vals) {
    for (maxnodes in maxnodes_vals) {
      
      # Train Random Forest model
      rf_model <- randomForest(
        as.formula(paste(outcome_var, "~", paste(predictors, collapse = "+"))),
        data = train_data,
        na.action = na.omit,
        ntree = ntree,      
        maxnodes = maxnodes
      )
      
      # Predict on test data
      preds <- predict(rf_model, newdata = test_data)
      
      # Remove rows with missing actual outcomes in test_data
      test_data_clean <- test_data[!is.na(test_data[[outcome_var]]), ]
      preds_clean <- preds[!is.na(test_data[[outcome_var]])]
      
      # Calculate accuracy using the cleaned data
      acc <- mean(preds_clean == test_data_clean[[outcome_var]])
      
      # Store results
      all_results <- rbind(all_results, data.frame(
        outcome = outcome_var,
        ntree = ntree,
        maxnodes = maxnodes,
        accuracy = acc
      ))
    }
  }
}

# View results
print("Best Parameters for Each Outcome - 2024:")
best_results <- all_results %>%
  group_by(outcome) %>%
  filter(accuracy == max(accuracy, na.rm = TRUE)) %>%
  ungroup()

print(best_results)

## Overall Model Performance

# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", 
                "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c("issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Convert categorical variables to factors and check for missing values
data <- ees24

# # Remove any rows with missing data
# data <- drop_na(data)

# Convert outcome variables to factors in the dataset
for (outcome in outcomes) {
  data[[outcome]] <- factor(data[[outcome]], levels = 0:10)
}

# Split the data into 80% training and 20% testing
set.seed(123)  # Set seed for reproducibility
train_index <- createDataPartition(data[[outcomes[1]]], p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Initialize a list to store the models, variable importances, and OOB R-squared values
rf_models <- list()
variable_importances <- list()
oob_r_squared <- list()

# Loop over each outcome and train a random forest model
for (outcome in outcomes) {
  
  # Train the random forest model for classification
  rf_model <- randomForest(
    as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
    data = train_data,
    na.action = na.omit,
    ntree = best_params$ntree,          # Number of trees
    maxnodes = best_params$maxnodes,        # Max nodes
    importance = TRUE,    # Enable variable importance calculation
    keep.inbag = TRUE     # Keep OOB predictions for OOB R-squared
  )
  
  # Store the model in the list
  rf_models[[outcome]] <- rf_model
  
  # Calculate variable importance and store it
  importance_scores <- importance(rf_model)
  variable_importances[[outcome]] <- importance_scores
  
  # Calculate OOB R-squared
  oob_error_rate <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
  r_squared_oob <- 1 - oob_error_rate
  oob_r_squared[[outcome]] <- r_squared_oob
  
  # Print OOB R-squared
  cat("OOB R-squared for", outcome, ":", r_squared_oob, "\n")
}

# Plot and save Variable Importance for each outcome
for (outcome in outcomes) {
  cat("\nVariable Importance for", outcome, ":\n")
  
  # Define the file name for saving the plot
  file_name <- paste0("vip_", outcome, "_24.pdf")
  
  # Open a PDF device to save the plot
  pdf(file = file_name)
  
  # Plot the variable importance using base R plot
  varImpPlot(rf_models[[outcome]], main = paste("Variable Importance for", issue_names[[outcome]], "EES 2024"))
  
  # Close the PDF device
  dev.off()
}

cat("Variable importance plots have been saved.\n")

# Detect cores and register parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Parallel loop over each outcome to predict and calculate performance
test_performance <- foreach(outcome = outcomes, .combine = 'rbind', .packages = c('randomForest', 'caret')) %dopar% {
  
  # Predict on test data
  test_pred <- predict(rf_models[[outcome]], newdata = test_data, type = "response")
  
  # Debugging Step: Print the first few predictions
  cat("Predictions for outcome", outcome, ":", head(test_pred), "\n")
  
  # Remove rows with missing actual outcomes in test_data, keep the rest of the data intact
  test_data_clean <- test_data[!is.na(test_data[[outcome]]), ]
  test_preds_clean <- test_pred[!is.na(test_data[[outcome]])]
  
  # Debugging Step: Check how many rows are left after cleaning
  cat("Rows left for outcome", outcome, "after cleaning:", nrow(test_data_clean), "\n")
  cat("Predictions for outcome", outcome, "after cleaning:", length(test_preds_clean), "\n")
  
  # Ensure lengths match
  if (length(test_preds_clean) != nrow(test_data_clean)) {
    stop(paste("Mismatch in number of predictions and actual outcomes for outcome:", outcome))
  }
  
  # Check levels of actual and predicted before factor conversion
  actual_levels <- levels(factor(test_data_clean[[outcome]]))
  pred_levels <- unique(test_preds_clean)
  
  # Debugging Step: Check if the levels of actual and predicted match
  cat("Levels of actual for", outcome, ":", actual_levels, "\n")
  cat("Unique predictions for", outcome, ":", pred_levels, "\n")
  
  # If the levels don't match, ensure both are aligned
  if (!all(actual_levels %in% pred_levels)) {
    cat("Warning: Levels of actual values and predictions don't match for outcome:", outcome, "\n")
    actual_levels <- unique(c(actual_levels, pred_levels))
  }
  
  # Convert both actual and predicted to factors with matching levels
  actual <- factor(test_data_clean[[outcome]], levels = actual_levels)
  test_pred <- factor(test_preds_clean, levels = actual_levels)
  
  # Check if lengths match
  cat("Length of actual values for", outcome, ":", length(actual), "\n")
  cat("Length of predicted values for", outcome, ":", length(test_pred), "\n")
  
  # Calculate accuracy
  accuracy <- mean(test_pred == actual, na.rm = TRUE)  # Ignore NA values
  
  # Debugging Step: Output accuracy for the current outcome
  cat("Accuracy for outcome", outcome, ":", accuracy, "\n")
  
  # Return the accuracy for this outcome
  data.frame(Outcome = outcome, Accuracy = accuracy)
}

# Stop the parallel cluster
stopCluster(cl)

# Print test accuracy for all outcomes
cat("Test Accuracy for all outcomes:\n")
print(test_performance)

# Optionally, save as LaTeX or HTML
stargazer(test_performance, type = "latex", 
          summary = FALSE, 
          rownames = FALSE, 
          title = "Accuracies for Random Forest Models - EES24", 
          out = "accuracies_table_24.tex")

stargazer(test_performance, type = "html", 
          summary = FALSE, 
          rownames = FALSE, 
          title = "Accuracies for Random Forest Models - EES24", 
          out = "accuracies_table_24.html") # Save as HTML file

# Prepare OOB R-squared as a data frame and print it
oob_r_squared_df <- data.frame(
  Outcome = outcomes,
  OOB_R_Squared = unlist(oob_r_squared)
)
print("OOB R-squared for all outcomes:")
print(oob_r_squared_df)


# Optionally, you can save it as an HTML or LaTeX file
stargazer(oob_r_squared_df, type = "latex", 
          summary = FALSE, 
          rownames = FALSE, 
          title = "OOB R-Squared for Random Forest Models - EES24", 
          out = "oob_r_squared_table_24.tex")  # Save as LaTeX file

stargazer(oob_r_squared_df, type = "html", 
          summary = FALSE, 
          rownames = FALSE, 
          title = "OOB R-Squared for Random Forest Models - EES24", 
          out = "oob_r_squared_table_24.html")  # Save as HTML file


# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", 
                "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c("issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Convert categorical variables to factors and check for missing values
data <- ees24
# data <- ees14 %>%
#   drop_na() %>%  # drop rows with NA
#   group_by(countryname) %>%
#   slice_sample(n = 500) %>%  # 500 obs for each country
#   ungroup()


# List of countries
countries <- unique(data$countryname)

# Function to train and test random forest model
train_and_predict <- function(train_data, test_data, outcomes) {
  results <- list()
  
  for (outcome in outcomes) {
    best_params <- best_results %>%
      filter(outcome == outcome_var) %>%
      dplyr::select(ntree, maxnodes) %>%
      unlist() %>%
      as.list()
    
    rf_model <- randomForest(
      as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
      data = train_data,
      na.action = na.omit,
      ntree = best_params$ntree,         
      maxnodes = best_params$maxnodes
    )
    
    predictions <- predict(rf_model, newdata = test_data)
    predictions <- round(predictions)
    results[[paste(outcome, "pred", sep = "_")]] <- predictions
  }
  
  return(results)
}

# Function to calculate precision, recall, and F1-score for multi-class classification
calculate_performance_metrics <- function(confusion) {
  precision <- confusion$byClass[, "Pos Pred Value"]
  recall <- confusion$byClass[, "Sensitivity"]
  
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  
  fscore <- 2 * (precision * recall) / (precision + recall)
  fscore[is.na(fscore)] <- 0
  
  avg_precision <- mean(precision)
  avg_recall <- mean(recall)
  avg_fscore <- mean(fscore)
  
  accuracy <- confusion$overall["Accuracy"]
  
  return(data.frame(
    Accuracy = accuracy,
    Precision = avg_precision,
    Recall = avg_recall,
    FScore = avg_fscore
  ))
}

# Initialize parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Initialize list to store performance metrics
performance_metrics_all_countries <- list()

# Parallel loop over each country
tictoc::tic("Total Time")  # Start timing the whole process
performance_metrics_all_countries <- foreach(country = countries, .combine = 'rbind',
                                             .packages = c('dplyr', 'randomForest', 'caret', 'stringr', 'tictoc')) %dopar% {
                                               
                                               # Time each country
                                               tictoc::tic(paste("Country:", country))
                                               
                                               train_data <- data %>% filter(countryname != country)
                                               test_data <- data %>% filter(countryname == country)
                                               
                                               for (outcome in outcomes) {
                                                 test_data[[outcome]] <- factor(test_data[[outcome]], levels = 0:10)
                                               }
                                               
                                               predictions <- train_and_predict(train_data, test_data, outcomes)
                                               
                                               performance_metrics <- list()
                                               for (outcome in outcomes) {
                                                 actual <- factor(test_data[[outcome]], levels = 0:10)
                                                 predicted <- factor(predictions[[paste(outcome, "pred", sep = "_")]], levels = 0:10)
                                                 
                                                 confusion <- confusionMatrix(predicted, actual)
                                                 performance_metrics[[outcome]] <- calculate_performance_metrics(confusion)
                                               }
                                               
                                               # Stop timing for this country
                                               tictoc::toc(log = TRUE)
                                               
                                               bind_rows(performance_metrics, .id = "Issue") %>%
                                                 mutate(Country = country)
                                             }

# Stop timing the entire process
tictoc::toc()

# Stop parallel backend
stopCluster(cl)

# Convert the list to a data frame
performance_metrics_all_countries_df <- bind_rows(performance_metrics_all_countries)

# Save performance metrics
write.csv(performance_metrics_all_countries_df, "performance_metrics_all_countries24.csv", row.names = FALSE)

print("Performance metrics have been saved.")


# splits by country clusters

nwe_cluster <- c("DK", "FI", "SE", "AT", "BE", "FR", "DE", "IE", "LU", "NL", "UK")
ee_cluster <- c("BG", "HR", "CZ", "EE", "HU", "LV", "LT", "PL", "RO", "SK", "SI")
se_cluster <- c("CY", "EL", "IT", "MT", "PT", "ES")
eu15_cluster <- c("AT", "BE", "DK", "FI", "FR", "DE", "EL", "IE", "IT", "LU", "NL", "PT", "ES", "SE", "UK")


# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c(  "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Split dataset
data <- ees24 %>% filter(countryname %in% nwe_cluster)

# List of countries
countries <- unique(data$countryname)

# Function to train and test random forest model
train_and_predict <- function(train_data, test_data, outcomes) {
  results <- list()
  
  for (outcome in outcomes) {
    best_params <- best_results %>%
      filter(outcome == outcome_var) %>%
      dplyr::select(ntree, maxnodes) %>%
      unlist() %>%
      as.list()
    
    rf_model <- randomForest(
      as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
      data = train_data,
      na.action = na.omit,
      ntree = best_params$ntree,         
      maxnodes = best_params$maxnodes
    )
    
    predictions <- predict(rf_model, newdata = test_data)
    predictions <- round(predictions)
    results[[paste(outcome, "pred", sep = "_")]] <- predictions
  }
  
  return(results)
}

# Function to calculate precision, recall, and F1-score for multi-class classification
calculate_performance_metrics <- function(confusion) {
  precision <- confusion$byClass[, "Pos Pred Value"]
  recall <- confusion$byClass[, "Sensitivity"]
  
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  
  fscore <- 2 * (precision * recall) / (precision + recall)
  fscore[is.na(fscore)] <- 0
  
  avg_precision <- mean(precision)
  avg_recall <- mean(recall)
  avg_fscore <- mean(fscore)
  
  accuracy <- confusion$overall["Accuracy"]
  
  return(data.frame(
    Accuracy = accuracy,
    Precision = avg_precision,
    Recall = avg_recall,
    FScore = avg_fscore
  ))
}

# Initialize parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Initialize list to store performance metrics
performance_metrics_nwe_countries <- list()

# Parallel loop over each country
tictoc::tic("Total Time")  # Start timing the whole process
performance_metrics_nwe_countries <- foreach(country = countries, .combine = 'rbind',
                                             .packages = c('dplyr', 'randomForest', 'caret', 'stringr', 'tictoc')) %dopar% {
                                               
                                               # Time each country
                                               tictoc::tic(paste("Country:", country))
                                               
                                               train_data <- data %>% filter(countryname != country)
                                               test_data <- data %>% filter(countryname == country)
                                               
                                               for (outcome in outcomes) {
                                                 test_data[[outcome]] <- factor(test_data[[outcome]], levels = 0:10)
                                               }
                                               
                                               predictions <- train_and_predict(train_data, test_data, outcomes)
                                               
                                               performance_metrics <- list()
                                               for (outcome in outcomes) {
                                                 actual <- factor(test_data[[outcome]], levels = 0:10)
                                                 predicted <- factor(predictions[[paste(outcome, "pred", sep = "_")]], levels = 0:10)
                                                 
                                                 confusion <- confusionMatrix(predicted, actual)
                                                 performance_metrics[[outcome]] <- calculate_performance_metrics(confusion)
                                               }
                                               
                                               # Stop timing for this country
                                               tictoc::toc(log = TRUE)
                                               
                                               bind_rows(performance_metrics, .id = "Issue") %>%
                                                 mutate(Country = country)
                                             }

# Stop timing the entire process
tictoc::toc()

# Stop parallel backend
stopCluster(cl)

# Convert the list to a data frame
performance_metrics_nwe_countries_df <- bind_rows(performance_metrics_nwe_countries)

# Save performance metrics
write.csv(performance_metrics_nwe_countries_df, "performance_metrics_nwe_countries24.csv", row.names = FALSE)

print("Performance metrics have been saved.")

# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c(  "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Split dataset
data <- ees24 %>% filter(countryname %in% se_cluster)

# List of countries
countries <- unique(data$countryname)

# Function to train and test random forest model
train_and_predict <- function(train_data, test_data, outcomes) {
  results <- list()
  
  for (outcome in outcomes) {
    best_params <- best_results %>%
      filter(outcome == outcome_var) %>%
      dplyr::select(ntree, maxnodes) %>%
      unlist() %>%
      as.list()
    
    rf_model <- randomForest(
      as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
      data = train_data,
      na.action = na.omit,
      ntree = best_params$ntree,         
      maxnodes = best_params$maxnodes
    )
    
    predictions <- predict(rf_model, newdata = test_data)
    predictions <- round(predictions)
    results[[paste(outcome, "pred", sep = "_")]] <- predictions
  }
  
  return(results)
}

# Function to calculate precision, recall, and F1-score for multi-class classification
calculate_performance_metrics <- function(confusion) {
  precision <- confusion$byClass[, "Pos Pred Value"]
  recall <- confusion$byClass[, "Sensitivity"]
  
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  
  fscore <- 2 * (precision * recall) / (precision + recall)
  fscore[is.na(fscore)] <- 0
  
  avg_precision <- mean(precision)
  avg_recall <- mean(recall)
  avg_fscore <- mean(fscore)
  
  accuracy <- confusion$overall["Accuracy"]
  
  return(data.frame(
    Accuracy = accuracy,
    Precision = avg_precision,
    Recall = avg_recall,
    FScore = avg_fscore
  ))
}

# Initialize parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Initialize list to store performance metrics
performance_metrics_se_countries <- list()

# Parallel loop over each country
tictoc::tic("Total Time")  # Start timing the whole process
performance_metrics_se_countries <- foreach(country = countries, .combine = 'rbind',
                                            .packages = c('dplyr', 'randomForest', 'caret', 'stringr', 'tictoc')) %dopar% {
                                              
                                              # Time each country
                                              tictoc::tic(paste("Country:", country))
                                              
                                              train_data <- data %>% filter(countryname != country)
                                              test_data <- data %>% filter(countryname == country)
                                              
                                              for (outcome in outcomes) {
                                                test_data[[outcome]] <- factor(test_data[[outcome]], levels = 0:10)
                                              }
                                              
                                              predictions <- train_and_predict(train_data, test_data, outcomes)
                                              
                                              performance_metrics <- list()
                                              for (outcome in outcomes) {
                                                actual <- factor(test_data[[outcome]], levels = 0:10)
                                                predicted <- factor(predictions[[paste(outcome, "pred", sep = "_")]], levels = 0:10)
                                                
                                                confusion <- confusionMatrix(predicted, actual)
                                                performance_metrics[[outcome]] <- calculate_performance_metrics(confusion)
                                              }
                                              
                                              # Stop timing for this country
                                              tictoc::toc(log = TRUE)
                                              
                                              bind_rows(performance_metrics, .id = "Issue") %>%
                                                mutate(Country = country)
                                            }

# Stop timing the entire process
tictoc::toc()

# Stop parallel backend
stopCluster(cl)

# Convert the list to a data frame
performance_metrics_se_countries_df <- bind_rows(performance_metrics_se_countries)

# Save performance metrics
write.csv(performance_metrics_se_countries_df, "performance_metrics_se_countries24.csv", row.names = FALSE)

print("Performance metrics have been saved.")

# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c(  "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Split dataset
data <- ees24 %>% filter(countryname %in% ee_cluster)

# List of countries
countries <- unique(data$countryname)

# Function to train and test random forest model
train_and_predict <- function(train_data, test_data, outcomes) {
  results <- list()
  
  for (outcome in outcomes) {
    best_params <- best_results %>%
      filter(outcome == outcome_var) %>%
      dplyr::select(ntree, maxnodes) %>%
      unlist() %>%
      as.list()
    
    rf_model <- randomForest(
      as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
      data = train_data,
      na.action = na.omit,
      ntree = best_params$ntree,         
      maxnodes = best_params$maxnodes
    )
    
    predictions <- predict(rf_model, newdata = test_data)
    predictions <- round(predictions)
    results[[paste(outcome, "pred", sep = "_")]] <- predictions
  }
  
  return(results)
}

# Function to calculate precision, recall, and F1-score for multi-class classification
calculate_performance_metrics <- function(confusion) {
  precision <- confusion$byClass[, "Pos Pred Value"]
  recall <- confusion$byClass[, "Sensitivity"]
  
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  
  fscore <- 2 * (precision * recall) / (precision + recall)
  fscore[is.na(fscore)] <- 0
  
  avg_precision <- mean(precision)
  avg_recall <- mean(recall)
  avg_fscore <- mean(fscore)
  
  accuracy <- confusion$overall["Accuracy"]
  
  return(data.frame(
    Accuracy = accuracy,
    Precision = avg_precision,
    Recall = avg_recall,
    FScore = avg_fscore
  ))
}

# Initialize parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Initialize list to store performance metrics
performance_metrics_ee_countries <- list()

# Parallel loop over each country
tictoc::tic("Total Time")  # Start timing the whole process
performance_metrics_ee_countries <- foreach(country = countries, .combine = 'rbind',
                                            .packages = c('dplyr', 'randomForest', 'caret', 'stringr', 'tictoc')) %dopar% {
                                              
                                              # Time each country
                                              tictoc::tic(paste("Country:", country))
                                              
                                              train_data <- data %>% filter(countryname != country)
                                              test_data <- data %>% filter(countryname == country)
                                              
                                              for (outcome in outcomes) {
                                                test_data[[outcome]] <- factor(test_data[[outcome]], levels = 0:10)
                                              }
                                              
                                              predictions <- train_and_predict(train_data, test_data, outcomes)
                                              
                                              performance_metrics <- list()
                                              for (outcome in outcomes) {
                                                actual <- factor(test_data[[outcome]], levels = 0:10)
                                                predicted <- factor(predictions[[paste(outcome, "pred", sep = "_")]], levels = 0:10)
                                                
                                                confusion <- confusionMatrix(predicted, actual)
                                                performance_metrics[[outcome]] <- calculate_performance_metrics(confusion)
                                              }
                                              
                                              # Stop timing for this country
                                              tictoc::toc(log = TRUE)
                                              
                                              bind_rows(performance_metrics, .id = "Issue") %>%
                                                mutate(Country = country)
                                            }

# Stop timing the entire process
tictoc::toc()

# Stop parallel backend
stopCluster(cl)

# Convert the list to a data frame
performance_metrics_ee_countries_df <- bind_rows(performance_metrics_ee_countries)

# Save performance metrics
write.csv(performance_metrics_ee_countries_df, "performance_metrics_ee_countries24.csv", row.names = FALSE)

print("Performance metrics have been saved.")

# Define the predictors and outcomes
predictors <- c("edu", "age_cat", "female", "ruralurban", "socclass", "religion", "workstat_cat", "livingstdrd", "polint", "ptyfamid")
outcomes <- c(  "issue_redistr", "issue_clim", "issue_samesex", "issue_immig")

# Split dataset
data <- ees24 %>% filter(countryname %in% eu15_cluster)

# List of countries
countries <- unique(data$countryname)

# Function to train and test random forest model
train_and_predict <- function(train_data, test_data, outcomes) {
  results <- list()
  
  for (outcome in outcomes) {
    best_params <- best_results %>%
      filter(outcome == outcome_var) %>%
      dplyr::select(ntree, maxnodes) %>%
      unlist() %>%
      as.list()
    
    rf_model <- randomForest(
      as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
      data = train_data,
      na.action = na.omit,
      ntree = best_params$ntree,         
      maxnodes = best_params$maxnodes
    )
    
    predictions <- predict(rf_model, newdata = test_data)
    predictions <- round(predictions)
    results[[paste(outcome, "pred", sep = "_")]] <- predictions
  }
  
  return(results)
}

# Function to calculate precision, recall, and F1-score for multi-class classification
calculate_performance_metrics <- function(confusion) {
  precision <- confusion$byClass[, "Pos Pred Value"]
  recall <- confusion$byClass[, "Sensitivity"]
  
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  
  fscore <- 2 * (precision * recall) / (precision + recall)
  fscore[is.na(fscore)] <- 0
  
  avg_precision <- mean(precision)
  avg_recall <- mean(recall)
  avg_fscore <- mean(fscore)
  
  accuracy <- confusion$overall["Accuracy"]
  
  return(data.frame(
    Accuracy = accuracy,
    Precision = avg_precision,
    Recall = avg_recall,
    FScore = avg_fscore
  ))
}

# Initialize parallel backend
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Initialize list to store performance metrics
performance_metrics_eu15_countries <- list()

# Parallel loop over each country
tictoc::tic("Total Time")  # Start timing the whole process
performance_metrics_eu15_countries <- foreach(country = countries, .combine = 'rbind',
                                              .packages = c('dplyr', 'randomForest', 'caret', 'stringr', 'tictoc')) %dopar% {
                                                
                                                # Time each country
                                                tictoc::tic(paste("Country:", country))
                                                
                                                train_data <- data %>% filter(countryname != country)
                                                test_data <- data %>% filter(countryname == country)
                                                
                                                for (outcome in outcomes) {
                                                  test_data[[outcome]] <- factor(test_data[[outcome]], levels = 0:10)
                                                }
                                                
                                                predictions <- train_and_predict(train_data, test_data, outcomes)
                                                
                                                performance_metrics <- list()
                                                for (outcome in outcomes) {
                                                  actual <- factor(test_data[[outcome]], levels = 0:10)
                                                  predicted <- factor(predictions[[paste(outcome, "pred", sep = "_")]], levels = 0:10)
                                                  
                                                  confusion <- confusionMatrix(predicted, actual)
                                                  performance_metrics[[outcome]] <- calculate_performance_metrics(confusion)
                                                }
                                                
                                                # Stop timing for this country
                                                tictoc::toc(log = TRUE)
                                                
                                                bind_rows(performance_metrics, .id = "Issue") %>%
                                                  mutate(Country = country)
                                              }

# Stop timing the entire process
tictoc::toc()

# Stop parallel backend
stopCluster(cl)

# Convert the list to a data frame
performance_metrics_eu15_countries_df <- bind_rows(performance_metrics_eu15_countries)

# Save performance metrics
write.csv(performance_metrics_eu15_countries_df, "performance_metrics_eu15_countries24.csv", row.names = FALSE)

print("Performance metrics have been saved.")