load("ees14clean.RData")
load("ees19clean.RData")
load("ees24clean.RData")

##### Helper functions for computing EMD #####
# Difference in means
dmeans <- function(data){
  df <- data
  x <- as.matrix(df$samps[which(df$pop == "x")])
  y <- as.matrix(df$samps[which(df$pop == "y")])
  abs(mean(x,na.rm=T)-mean(y,na.rm=T))
}
# CDF overlap
cdf.overlap <- function(data, q = 512) { 
  # no default length for sequences, but we'll use density's 512
  df <- data
  x <- as.matrix(na.omit(df$samps[which(df$pop == "x")]))
  y <- as.matrix(na.omit(df$samps[which(df$pop == "y")]))
  Fx <- ecdf(x); Fy <- ecdf(y)
  lower <- min(x,y) 
  upper <- max(x,y)
  z <- seq(lower,upper,((upper-lower)/q))
  Fxz <- Fx(z); Fyz <- Fy(z) 
  sum(abs(Fxz-Fyz))
}
# PDF overlap
pdf.overlap <- function(data, q = 512){ # q = 512 is the default for density()
  df <- data
  fx <- as.vector(na.omit(df$samps[which(df$pop == "x")]))
  fy <- as.vector(na.omit(df$samps[which(df$pop == "y")]))
  lower <- min(c(fx, fy))
  upper <- max(c(fx, fy))
  dx <- density(fx, from=lower, to=upper, n = q)
  dy <- density(fy, from=lower, to=upper, n = q)
  d <- data.frame(location=dx$x, x.den=dx$y, y.den=dy$y)
  d$intersect <- pmin(d$x.den, d$y.den)
  integrate.xy(d$location, d$intersect)
}
# EMD
emd.dis <- function(data, metric = "manhattan", iterations=100000){
  df <- data
  x <- as.matrix(df$samps[which(df$pop == "x")])
  y <- as.matrix(df$samps[which(df$pop == "y")])
  weight.x <- rep(1/nrow(x),nrow(x))
  weight.y <- rep(1/nrow(y),nrow(y))
  emdw(x,weight.x,y,weight.y,dist=metric,max.iter=iterations)
}


# Define the testing function
calculate_emd_test <- function(data, issue_variables, country_column = "countryname", filter_na = TRUE) {
  
  # Initialize an empty list to store all results
  all_results <- list()
  
  # Loop through each issue variable
  for (issue_var in issue_variables) {
    
    # Prepare dataset for the current issue variable
    issue_data <- data %>%
      mutate(issue_value = .[[issue_var]]) %>%  # Dynamically reference the issue variable
      dplyr::select({{country_column}}, issue_value) %>%
      dplyr::rename(pop = {{country_column}},
                    samps = issue_value)
    
    if (filter_na) {
      issue_data <- issue_data %>% filter(!is.na(samps))
    }
    
    # Get a list of unique countries
    countries <- unique(issue_data$pop)
    
    # Create all dyadic pairs of countries
    pairs <- expand.grid(country1 = countries, country2 = countries) %>%
      filter(country1 != country2)  # Remove self-pairs
    
    #to test: keep only subset of pairs
    pairs<-pairs %>% head(25)
    
    # Initialize a list to store results for this issue variable
    issue_results <- list()
    
    # Loop through each pair to calculate EMD
    for (i in 1:nrow(pairs)) {
      country1 <- pairs$country1[i]
      country2 <- pairs$country2[i]
      
      # Prepare data for the two countries
      subset_data <- issue_data %>%
        filter(pop %in% c(country1, country2)) %>%
        mutate(pop = case_when(pop == country1 ~ "x",
                               pop == country2 ~ "y"))
      
      # Compute EMD
      emd_value <- emd.dis(subset_data)
      
      # Store results
      issue_results[[i]] <- data.frame(issue = issue_var,
                                       country1 = country1,
                                       country2 = country2,
                                       emd = emd_value)
    }
    
    # Combine results for this issue variable and add to all results
    all_results[[issue_var]] <- bind_rows(issue_results)
  }
  
  # Combine all results into a single data frame
  final_results <- bind_rows(all_results)
  return(final_results)
}


# Define the function
calculate_emd <- function(data, issue_variables, country_column = "countryname", filter_na = TRUE) {
  
  # Initialize an empty list to store all results
  all_results <- list()
  
  # Loop through each issue variable
  for (issue_var in issue_variables) {
    
    # Prepare dataset for the current issue variable
    issue_data <- data %>%
      mutate(issue_value = .[[issue_var]]) %>%  # Dynamically reference the issue variable
      dplyr::select({{country_column}}, issue_value) %>%
      dplyr::rename(pop = {{country_column}},
                    samps = issue_value)
    
    if (filter_na) {
      issue_data <- issue_data %>% filter(!is.na(samps))
    }
    
    # Get a list of unique countries
    countries <- unique(issue_data$pop)
    
    # Create all dyadic pairs of countries
    pairs <- expand.grid(country1 = countries, country2 = countries) %>%
      filter(country1 != country2)  # Remove self-pairs
    
    
    # Initialize a list to store results for this issue variable
    issue_results <- list()
    
    # Loop through each pair to calculate EMD
    for (i in 1:nrow(pairs)) {
      country1 <- pairs$country1[i]
      country2 <- pairs$country2[i]
      
      # Prepare data for the two countries
      subset_data <- issue_data %>%
        filter(pop %in% c(country1, country2)) %>%
        mutate(pop = case_when(pop == country1 ~ "x",
                               pop == country2 ~ "y"))
      
      # Compute EMD
      emd_value <- emd.dis(subset_data)
      
      # Store results
      issue_results[[i]] <- data.frame(issue = issue_var,
                                       country1 = country1,
                                       country2 = country2,
                                       emd = emd_value)
    }
    
    # Combine results for this issue variable and add to all results
    all_results[[issue_var]] <- bind_rows(issue_results)
  }
  
  # Combine all results into a single data frame
  final_results <- bind_rows(all_results)
  return(final_results)
}


#apply function
# Specify the dataset and issue variables to process
issue_variables <- c("issue_redistr", "issue_state_int", "issue_samesex", "issue_immig", "issue_clim")

# Call the function
emd_results14 <- calculate_emd(data = ees14, issue_variables = issue_variables) %>%
  mutate(yr=2014)

# View the results
print(emd_results14)
save(emd_results14, file = "emd_results14.RData")

#apply function
# Specify the dataset and issue variables to process
issue_variables <- c("issue_redistr", "issue_state_int", "issue_samesex", "issue_immig", "issue_clim")

# Call the function
emd_results19 <- calculate_emd(data = ees19, issue_variables = issue_variables) %>%
  mutate(yr=2019)

# View the results
print(emd_results19)
save(emd_results19, file = "emd_results19.RData")

#apply function
# Specify the dataset and issue variables to process
issue_variables <- c("issue_redistr", "issue_samesex", "issue_immig", "issue_clim")

# Call the function
emd_results24 <- calculate_emd(data = ees24, issue_variables = issue_variables) %>%
  mutate(yr=2024)

# View the results
print(emd_results24)
save(emd_results24, file = "emd_results24.RData")