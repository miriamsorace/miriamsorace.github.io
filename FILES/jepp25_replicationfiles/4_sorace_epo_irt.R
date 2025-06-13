load("ees14clean.RData")
load("ees19clean.RData")
load("ees24clean.RData")

# reshape ees file so that datafile has respondent-PVV_pty_no as unit of observation
ees14resh <- ees14 %>%
  tidyr::pivot_longer(cols = starts_with("qpp8"),  # Select all columns that start with "q9"
                      names_to = "PTVno",   # Create a new column 'q9_variable' for the names
                      values_to = "PTVval")        # Create a new column 'PTVno' for the values

# Modify PTVno to only contain the numbers after "_"
ees14resh$PTVno <- sub("qpp8_", "", ees14resh$PTVno)

# View the reshaped dataset
head(ees14resh)


# add in EES codes from party list csv file and create a EPG identifier of the PPV & take average of parties in same EPG:

ptycodes14 <- read.csv("ees_datasets/ZA5160_ptycodes.csv") %>%
  filter(Country.code != "" & Country.code != "country-code" & !is.na(Country.code)) %>%
  dplyr::select(X, Party.name.in.English, Probability.to.Vote, Party.code.for.vote.choice.in.EP.2014) %>%
  dplyr::rename(countryname = X,
                EES_code = Party.code.for.vote.choice.in.EP.2014,
                PTVno = Probability.to.Vote) %>%
  dplyr::mutate(countryname = case_when(countryname == "GR" ~ "EL", TRUE ~ countryname))

# Modify PTVno to only contain the numbers after "_"
ptycodes14$PTVno <- sub("qpp8_", "", ptycodes14$PTVno)


ptyfile<-ptycodes14 %>%
  dplyr::select(countryname, PTVno, EES_code) %>%
  filter(!EES_code ==20)


ptyfile <- ptyfile %>%
  mutate(EES_code = as.character(EES_code),  # Treat EES_code as character to avoid numeric precision issues
         last_three = substr(EES_code, nchar(EES_code) - 2, nchar(EES_code)),  # Extract last three digits
         first_digit = substr(last_three, 1, 1),  # Extract first digit of last three
         ptyfam = case_when(
           first_digit == "1" ~ "green",
           first_digit == "3" ~ "social democrats",
           first_digit == "2" ~ "left",
           first_digit == "4" ~ "liberals",
           first_digit == "5" ~ "christian democrats/centre right",
           first_digit == "6" ~ "conservative",
           as.numeric(first_digit) >= 7 & as.numeric(first_digit) < 9 ~ "radical right",
           first_digit == "9" ~ "niche/fringe",
           TRUE ~ "no party id"
         ),
         ptyfam = factor(ptyfam, levels = c("left", "green", "social democrats","liberals", "christian democrats/centre right", "conservative", "radical right", "niche/fringe", "no party id"))
  )

ptyfile <- ptyfile %>%
  dplyr::select(countryname, PTVno, EES_code, ptyfam)

#merge
ees14resh2 <- merge(ees14resh, ptyfile, by = c("countryname", "PTVno"), all.x = TRUE)

# Compute the average PTVval by ptyfam and respid
average_PTV <- ees14resh2 %>%
  group_by(ptyfam, respid) %>%
  summarise(PTVval_avg = mean(PTVval, na.rm = TRUE)) %>%
  filter(!is.na(ptyfam))

# View the result
head(average_PTV)


# re-shape back so PVV_EPG columns

PTVrespdata <- average_PTV %>%
  pivot_wider(names_from = ptyfam,
              values_from = PTVval_avg,
              names_prefix = "PTV_")

# merge to original ees file

ees14 <- ees14 %>% left_join(PTVrespdata, by = "respid")

#PTV aggregation functions
#econ
gen_agg_ptv_econ <- function(data){
  data <- data %>% 
    rowwise() %>%
    mutate(
      PTV_proeconright = weighted.mean(
        c(PTV_conservative, PTV_liberals, `PTV_christian democrats/centre right`), 
        c(3, 2, 1), 
        na.rm = TRUE
      )
    ) %>%
    ungroup()
  
  data <- data %>%
    rowwise() %>%
    mutate(
      PTV_againsteconleft = 10-(weighted.mean(
        c(PTV_left, `PTV_social democrats`), 
        c(3,2), 
        na.rm = TRUE
      ))
    ) %>%
    ungroup()
}

#social
gen_agg_ptv_soc <- function(data){
  data <- data %>% 
    rowwise() %>%
    mutate(
      PTV_prosocright = weighted.mean(
        c(`PTV_radical right`, PTV_conservative, `PTV_christian democrats/centre right`), 
        c(3, 2, 1), 
        na.rm = TRUE
      )
    ) %>%
    ungroup()
  
  data<-data %>%
    rowwise() %>%
    mutate(
      PTV_againstsocleft = 10-(weighted.mean(
        c(`PTV_social democrats`, PTV_green), 
        c(3,2), 
        na.rm = TRUE
      ))
    ) %>%
    ungroup()
}

#apply to ees data
ees14<-gen_agg_ptv_econ(ees14)
ees14<-gen_agg_ptv_soc(ees14)


# Fit the multigroup IRT model without slope/intercept constraints but using PTVs as anchor columns in invariance specs: so those columsn set to be invariant by country
irt_data <- ees14[, c("countryname","issue_redistr", "PTV_proeconright", "PTV_againsteconleft")]
nms<-colnames(irt_data)

model_multi <- multipleGroup(irt_data[, c("issue_redistr",  "PTV_proeconright", "PTV_againsteconleft")], 
                             1, itemtype = 'graded', group = irt_data$countryname, invariance = c(nms[c(3:4)]))

# Check the model summary
summary(model_multi)

plot(model_multi)

# Edit plot

library(lattice)

# Assuming `model_multi` is already fitted and the plot is stored in `p`
p <- plot(model_multi)

# Modify the trellis plot with the fixes
p <- update(p, 
            main = "Expected Total Score - EES14",  # Title
            sub = "Economic Dimension",  # Subtitle
            layout.heights = list(
              top = 3,        # Increase space for the main title
              sub = -10         # Adjust subtitle space
            ),
            panel = function(x, y, groups, ...) {
              # Default plotting of lines
              panel.xyplot(x, y, groups = groups, ...)
              
              # Add country names at the end of each line with jitter
              country_names <- levels(groups)  # Get country names from the groups
              for (i in seq_along(country_names)) {
                # Find the last point of the line for each country
                max_x <- max(x[groups == country_names[i]])  # Maximum value of theta for each country
                max_y <- y[which(x == max_x & groups == country_names[i])]  # Corresponding y value for that country
                
                # # Add jittered country names at the end of the lines
                # jitter_x <- max_x + runif(1, -2, 0)  # Jitter in the x direction
                # jitter_y <- max_y + runif(1, -0.5, 0.8)  # Jitter in the y direction
                
                # Add jittered text at the end of the line
                # panel.text(jitter_x, jitter_y, labels = country_names[i], pos = 4, cex = 0.7)  
                # Add text at the end of the line
                panel.text(max_x, max_y, labels = country_names[i], pos = 4, cex = 0.5)  
              }
            })

# Print the modified plot
print(p)
pdf("DIFecon14.pdf")
print(p)  # Use print() to display the plot
dev.off() 


# Fit the fully constrained model (item parameters are equal across groups):  constrains all parameters to be equal across groups, not just anchoring vars.
model_constrained <- multipleGroup(irt_data[, c("issue_redistr",  "PTV_proeconright", "PTV_againsteconleft")], 
                                   1, itemtype = 'graded', group = irt_data$countryname, invariance = c('free_means', 'free_var', 'intercepts', 'slopes'))

anova_result<-anova(model_constrained, model_multi)

# Convert the ANOVA result to a LaTeX table
anova_tex <- xtable(anova_result)

# Save the LaTeX table to a .tex file in the working directory
file_name <- "anova_result_econ14.tex"
capture.output(print(anova_tex, type = "latex"), file = file_name)


# create DIF-free score in EES14 dataset
latent_scores <- fscores(model_constrained)
ees14$IRT_econ <- latent_scores

# anchored scores 
latent_scores2 <- fscores(model_multi)
ees14$IRT_econ2 <- latent_scores2

# Fit the multigroup IRT model  using PTVs as anchor columns 
irt_data <- ees14[, c("countryname","issue_immig", "issue_samesex", "PTV_prosocright", "PTV_againstsocleft")]
nms<-colnames(irt_data)

model_multi <- multipleGroup(irt_data[, c("issue_immig", "issue_samesex", "PTV_prosocright", "PTV_againstsocleft")], 
                             1, itemtype = 'graded', group = irt_data$countryname, invariance = c(nms[c(4:5)]))

# Check the model summary
summary(model_multi)

# DIF diagnostic plot
plot(model_multi)

# Edit plot

library(lattice)

# Assuming `model_multi` is already fitted and the plot is stored in `p`
p <- plot(model_multi)

# Modify the trellis plot with the fixes
p <- update(p, 
            main = "Expected Total Score - EES14",  # Title
            sub = "Socio-Cultural Dimension",  # Subtitle
            layout.heights = list(
              top = 3,        # Increase space for the main title
              sub = -10         # Adjust subtitle space
            ),
            panel = function(x, y, groups, ...) {
              # Default plotting of lines
              panel.xyplot(x, y, groups = groups, ...)
              
              # Add country names at the end of each line with jitter
              country_names <- levels(groups)  # Get country names from the groups
              for (i in seq_along(country_names)) {
                # Find the last point of the line for each country
                max_x <- max(x[groups == country_names[i]])  # Maximum value of theta for each country
                max_y <- y[which(x == max_x & groups == country_names[i])]  # Corresponding y value for that country
                
                # # Add jittered country names at the end of the lines
                # jitter_x <- max_x + runif(1, -2, 0)  # Jitter in the x direction
                # jitter_y <- max_y + runif(1, -0.5, 0.8)  # Jitter in the y direction
                
                # Add jittered text at the end of the line
                # panel.text(jitter_x, jitter_y, labels = country_names[i], pos = 4, cex = 0.7)  
                # Add text at the end of the line
                panel.text(max_x, max_y, labels = country_names[i], pos = 4, cex = 0.5)  
              }
            })

# Print the modified plot
print(p)
pdf("DIFsoc14.pdf")
print(p)  # Use print() to display the plot
dev.off() 


# Fit the fully constrained model
model_constrained <- multipleGroup(irt_data[, c("issue_immig", "issue_samesex", "PTV_prosocright", "PTV_againstsocleft")], 
                                   1, itemtype = 'graded', group = irt_data$countryname, invariance = c('free_means', 'free_var', 'intercepts', 'slopes'))

#compare unconstrained and constrained models
anova_result<-anova(model_constrained, model_multi)

# Convert the ANOVA result to a LaTeX table
anova_tex <- xtable(anova_result)

# Save the LaTeX table to a .tex file in the working directory
file_name <- "anova_result_soc14.tex"
capture.output(print(anova_tex, type = "latex"), file = file_name)


# create DIF-free score in EES14 dataset
latent_scores <- fscores(model_constrained)
ees14$IRT_soc <- latent_scores

# anchored scores 
latent_scores2 <- fscores(model_multi)
ees14$IRT_soc2 <- latent_scores2

# reshape ees file so that datafile has respondent-PVV_pty_no as unit of observation
ees19resh <- ees19 %>%
  tidyr::pivot_longer(cols = starts_with("q10"),  # Select all columns that start with "q9"
                      names_to = "PTVno",   # Create a new column 'q9_variable' for the names
                      values_to = "PTVval")        # Create a new column 'PTVno' for the values

# Modify PTVno to only contain the numbers after "_"
ees19resh$PTVno <- sub("q10_", "", ees19resh$PTVno)

# View the reshaped dataset
head(ees19resh)


# add in EES codes from party list csv file and create a EPG identifier of the PPV & take average of parties in same EPG:

ptycodes19<-read.csv("ees_datasets/ZA7581_ptycodes.csv") %>%
  dplyr::select(Coutnry_short, English.name, Q10_PTV, Q7_EES) %>%
  dplyr::rename(countryname = Coutnry_short,
                EES_code = Q7_EES,
                PTVno = Q10_PTV) %>%
  mutate(countryname = case_when(countryname == "GR" ~ "EL", TRUE ~ countryname))

# Modify PTVno to only contain the numbers after "_"
ptycodes19$PTVno <- sub("Q10_", "", ptycodes19$PTVno)

ptyfile<-ptycodes19 %>%
  dplyr::select(countryname, PTVno, EES_code) %>%
  filter(!EES_code ==20)


ptyfile <- ptyfile %>%
  mutate(EES_code = as.character(EES_code),  # Treat EES_code as character to avoid numeric precision issues
         last_three = substr(EES_code, nchar(EES_code) - 2, nchar(EES_code)),  # Extract last three digits
         first_digit = substr(last_three, 1, 1),  # Extract first digit of last three
         ptyfam = case_when(
           first_digit == "1" ~ "green",
           first_digit == "3" ~ "social democrats",
           first_digit == "2" ~ "left",
           first_digit == "4" ~ "liberals",
           first_digit == "5" ~ "christian democrats/centre right",
           first_digit == "6" ~ "conservative",
           as.numeric(first_digit) >= 7 & as.numeric(first_digit) < 9 ~ "radical right",
           first_digit == "9" ~ "niche/fringe",
           TRUE ~ "no party id"
         ),
         ptyfam = factor(ptyfam, levels = c("left", "green", "social democrats","liberals", "christian democrats/centre right", "conservative", "radical right", "niche/fringe", "no party id"))
  )


ptyfile <- ptyfile  %>%
  dplyr::select(countryname, PTVno, EES_code, ptyfam)

#merge
ees19resh2 <- merge(ees19resh, ptyfile, by = c("countryname", "PTVno"), all.x = TRUE)

# Compute the average PTVval by ptyfam and respid (and country as EES19 has same respids values in some countries)
average_PTV <- ees19resh2 %>%
  group_by(countryname,ptyfam, respid) %>%
  summarise(PTVval_avg = mean(PTVval, na.rm = TRUE)) %>%
  filter(!is.na(ptyfam))

# View the result
head(average_PTV)


# re-shape back so PVV_EPG columns

PTVrespdata <- average_PTV %>%
  pivot_wider(names_from = ptyfam,
              values_from = PTVval_avg,
              names_prefix = "PTV_")

# merge to original ees file

ees19 <- ees19 %>% left_join(PTVrespdata, by = c("respid", "countryname"))

#apply to ees data
ees19<-gen_agg_ptv_econ(ees19)
ees19<-gen_agg_ptv_soc(ees19)

# Fit the multigroup IRT model using PTVs as anchor columns
irt_data <- ees19[, c("countryname","issue_redistr", "PTV_proeconright", "PTV_againsteconleft")]
nms<-colnames(irt_data)

model_multi <- multipleGroup(irt_data[, c("issue_redistr",  "PTV_proeconright", "PTV_againsteconleft")], 
                             1, itemtype = 'graded', group = irt_data$countryname, invariance = c(nms[c(3:4)]))

# Check the model summary
summary(model_multi)

# DIF diagnostic plot
plot(model_multi)

# Edit plot

library(lattice)

# Assuming `model_multi` is already fitted and the plot is stored in `p`
p <- plot(model_multi)

# Modify the trellis plot with the fixes
p <- update(p, 
            main = "Expected Total Score - EES19",  # Title
            sub = "Economic Dimension",  # Subtitle
            layout.heights = list(
              top = 3,        # Increase space for the main title
              sub = -10         # Adjust subtitle space
            ),
            panel = function(x, y, groups, ...) {
              # Default plotting of lines
              panel.xyplot(x, y, groups = groups, ...)
              
              # Add country names at the end of each line with jitter
              country_names <- levels(groups)  # Get country names from the groups
              for (i in seq_along(country_names)) {
                # Find the last point of the line for each country
                max_x <- max(x[groups == country_names[i]])  # Maximum value of theta for each country
                max_y <- y[which(x == max_x & groups == country_names[i])]  # Corresponding y value for that country
                
                # # Add jittered country names at the end of the lines
                # jitter_x <- max_x + runif(1, -2, 0)  # Jitter in the x direction
                # jitter_y <- max_y + runif(1, -0.5, 0.8)  # Jitter in the y direction
                
                # Add jittered text at the end of the line
                # panel.text(jitter_x, jitter_y, labels = country_names[i], pos = 4, cex = 0.7)  
                # Add text at the end of the line
                panel.text(max_x, max_y, labels = country_names[i], pos = 4, cex = 0.5)  
              }
            })

# Print the modified plot
print(p)
pdf("DIFecon19.pdf")
print(p)  # Use print() to display the plot
dev.off() 


# Fit the fully constrained model 
model_constrained <- multipleGroup(irt_data[, c("issue_redistr",  "PTV_proeconright", "PTV_againsteconleft")], 
                                   1, itemtype = 'graded', group = irt_data$countryname, invariance = c('free_means', 'free_var', 'intercepts', 'slopes'))

#compare unconstrained and constrained models
anova_result<-anova(model_constrained, model_multi)

# Convert the ANOVA result to a LaTeX table
anova_tex <- xtable(anova_result)

# Save the LaTeX table to a .tex file in the working directory
file_name <- "anova_result_econ19.tex"
capture.output(print(anova_tex, type = "latex"), file = file_name)


# create DIF-free score in EES19 dataset
latent_scores <- fscores(model_constrained)
ees19$IRT_econ <- latent_scores

# anchored scores 
latent_scores2 <- fscores(model_multi)
ees19$IRT_econ2 <- latent_scores2

# Fit the multigroup IRT model  using PTVs as anchor columns 
irt_data <- ees19[, c("countryname","issue_immig", "issue_samesex", "PTV_prosocright", "PTV_againstsocleft")]
nms<-colnames(irt_data)

model_multi <- multipleGroup(irt_data[, c("issue_immig", "issue_samesex", "PTV_prosocright", "PTV_againstsocleft")], 
                             1, itemtype = 'graded', group = irt_data$countryname, invariance = c(nms[c(4:5)]))

# Check the model summary
summary(model_multi)

# DIF diagnostic plot
plot(model_multi)

# Edit plot

library(lattice)

# Assuming `model_multi` is already fitted and the plot is stored in `p`
p <- plot(model_multi)

# Modify the trellis plot with the fixes
p <- update(p, 
            main = "Expected Total Score - EES19",  # Title
            sub = "Socio-Cultural Dimension",  # Subtitle
            layout.heights = list(
              top = 3,        # Increase space for the main title
              sub = -10         # Adjust subtitle space
            ),
            panel = function(x, y, groups, ...) {
              # Default plotting of lines
              panel.xyplot(x, y, groups = groups, ...)
              
              # Add country names at the end of each line with jitter
              country_names <- levels(groups)  # Get country names from the groups
              for (i in seq_along(country_names)) {
                # Find the last point of the line for each country
                max_x <- max(x[groups == country_names[i]])  # Maximum value of theta for each country
                max_y <- y[which(x == max_x & groups == country_names[i])]  # Corresponding y value for that country
                
                # # Add jittered country names at the end of the lines
                # jitter_x <- max_x + runif(1, -2, 0)  # Jitter in the x direction
                # jitter_y <- max_y + runif(1, -0.5, 0.8)  # Jitter in the y direction
                
                # Add jittered text at the end of the line
                # panel.text(jitter_x, jitter_y, labels = country_names[i], pos = 4, cex = 0.7)  
                # Add text at the end of the line
                panel.text(max_x, max_y, labels = country_names[i], pos = 4, cex = 0.5)  
              }
            })

# Print the modified plot
print(p)
pdf("DIFsoc19.pdf")
print(p)  # Use print() to display the plot
dev.off() 


# Fit the fully constrained model
model_constrained <- multipleGroup(irt_data[, c("issue_immig", "issue_samesex", "PTV_prosocright", "PTV_againstsocleft")], 
                                   1, itemtype = 'graded', group = irt_data$countryname, invariance = c('free_means', 'free_var', 'intercepts', 'slopes'))

#compare unconstrained and constrained models
anova_result<-anova(model_constrained, model_multi)

# Convert the ANOVA result to a LaTeX table
anova_tex <- xtable(anova_result)

# Save the LaTeX table to a .tex file in the working directory
file_name <- "anova_result_soc19.tex"
capture.output(print(anova_tex, type = "latex"), file = file_name)


# create DIF-free score in EES19 dataset
latent_scores <- fscores(model_constrained)
ees19$IRT_soc <- latent_scores

# anchored scores 
latent_scores2 <- fscores(model_multi)
ees19$IRT_soc2 <- latent_scores2

# reshape ees file so that datafile has respondent-PVV_pty_no as unit of observation
ees24resh <- ees24 %>%
  tidyr::pivot_longer(cols = starts_with("q9"),  # Select all columns that start with "q9"
                      names_to = "PTVno",   # Create a new column 'q9_variable' for the names
                      values_to = "PTVval")        # Create a new column 'PTVno' for the values

# Modify PTVno to only contain the numbers after "_"
ees24resh$PTVno <- sub("q9_", "", ees24resh$PTVno)

# View the reshaped dataset
head(ees24resh)


# add in EES codes from party list csv file and create a EPG identifier of the PPV & take average of parties in same EPG:

ptycodes24<-read.csv("ees_datasets/ZA8868_ptycodes.csv") %>%
  dplyr::select(Country, Acronym, PARTY.in.Q9, EES_code) %>%
  dplyr::rename(PTVno = PARTY.in.Q9)


country_lookup <- data.frame(
  countryname = c("AT", "BE", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR",
                  "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK"),
  CountryCode = c("Austria", "Belgium-Flanders", "Belgium-Wallonia", "Bulgaria", "Cyprus", "Czechia",
                  "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland", "France", "Croatia",
                  "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands",
                  "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia")
)

# Join the ptycodes24 dataframe with the lookup table based on the 'Country' column
ptycodes24 <- ptycodes24 %>%
  left_join(country_lookup, by = c("Country" = "CountryCode")) %>%
  mutate(countryname = coalesce(countryname, Country))  # If no match, keep original code

head(ptycodes24)

ptyfile<-ptycodes24 %>%
  dplyr::select(countryname, PTVno, EES_code) %>%
  filter(!EES_code ==20)


ptyfile <- ptyfile %>%
  mutate(EES_code = as.character(EES_code),  # Treat EES_code as character to avoid numeric precision issues
         last_three = substr(EES_code, nchar(EES_code) - 2, nchar(EES_code)),  # Extract last three digits
         first_digit = substr(last_three, 1, 1),  # Extract first digit of last three
         ptyfam = case_when(
           first_digit == "1" ~ "green",
           first_digit == "3" ~ "social democrats",
           first_digit == "2" ~ "left",
           first_digit == "4" ~ "liberals",
           first_digit == "5" ~ "christian democrats/centre right",
           first_digit == "6" ~ "conservative",
           as.numeric(first_digit) >= 7 & as.numeric(first_digit) < 9 ~ "radical right",
           first_digit == "9" ~ "niche/fringe",
           TRUE ~ "no party id"
         ),
         ptyfam = factor(ptyfam, levels = c("left", "green", "social democrats","liberals", "christian democrats/centre right", "conservative", "radical right", "niche/fringe", "no party id"))
  )

#correct for mistaken EE party & select rel columns
ptyfile <- ptyfile %>% mutate(ptyfam = case_when(countryname == "EE" & PTVno == 3 ~ "radical right", TRUE ~ ptyfam)) %>%
  dplyr::select(countryname, PTVno, EES_code, ptyfam)

#merge
ees24resh2 <- merge(ees24resh, ptyfile, by = c("countryname", "PTVno"), all.x = TRUE)

# Compute the average PTVval by ptyfam and respid
average_PTV <- ees24resh2 %>%
  group_by(ptyfam, respid) %>%
  summarise(PTVval_avg = mean(PTVval, na.rm = TRUE)) %>%
  filter(!is.na(ptyfam))

# View the result
head(average_PTV)


# re-shape back so PVV_EPG columns

PTVrespdata <- average_PTV %>%
  pivot_wider(names_from = ptyfam,
              values_from = PTVval_avg,
              names_prefix = "PTV_")

# merge to original ees file

ees24 <- ees24 %>% left_join(PTVrespdata, by = "respid")

#apply to ees data
ees24<-gen_agg_ptv_econ(ees24)
ees24<-gen_agg_ptv_soc(ees24)

# Fit the multigroup IRT model using PTVs as anchor columns
irt_data <- ees24[, c("countryname","issue_redistr", "PTV_proeconright", "PTV_againsteconleft")]
nms<-colnames(irt_data)

model_multi <- multipleGroup(irt_data[, c("issue_redistr",  "PTV_proeconright", "PTV_againsteconleft")], 
                             1, itemtype = 'graded', group = irt_data$countryname, invariance = c(nms[c(3:4)]))

# Check the model summary
summary(model_multi)

# DIF diagnostic plot
plot(model_multi)

# Edit plot

library(lattice)

# Assuming `model_multi` is already fitted and the plot is stored in `p`
p <- plot(model_multi)

# Modify the trellis plot with the fixes
p <- update(p, 
            main = "Expected Total Score - EES24",  # Title
            sub = "Economic Dimension",  # Subtitle
            layout.heights = list(
              top = 3,        # Increase space for the main title
              sub = -10         # Adjust subtitle space
            ),
            panel = function(x, y, groups, ...) {
              # Default plotting of lines
              panel.xyplot(x, y, groups = groups, ...)
              
              # Add country names at the end of each line with jitter
              country_names <- levels(groups)  # Get country names from the groups
              for (i in seq_along(country_names)) {
                # Find the last point of the line for each country
                max_x <- max(x[groups == country_names[i]])  # Maximum value of theta for each country
                max_y <- y[which(x == max_x & groups == country_names[i])]  # Corresponding y value for that country
                
                # # Add jittered country names at the end of the lines
                # jitter_x <- max_x + runif(1, -2, 0)  # Jitter in the x direction
                # jitter_y <- max_y + runif(1, -0.5, 0.8)  # Jitter in the y direction
                
                # Add jittered text at the end of the line
                # panel.text(jitter_x, jitter_y, labels = country_names[i], pos = 4, cex = 0.7)  
                # Add text at the end of the line
                panel.text(max_x, max_y, labels = country_names[i], pos = 4, cex = 0.5)  
              }
            })

# Print the modified plot
print(p)
pdf("DIFecon24.pdf")
print(p)  # Use print() to display the plot
dev.off() 


# Fit the fully constrained model 
model_constrained <- multipleGroup(irt_data[, c("issue_redistr",  "PTV_proeconright", "PTV_againsteconleft")], 
                                   1, itemtype = 'graded', group = irt_data$countryname, invariance = c('free_means', 'free_var', 'intercepts', 'slopes'))

#compare unconstrained and constrained models
anova_result<-anova(model_constrained, model_multi)

# Convert the ANOVA result to a LaTeX table
anova_tex <- xtable(anova_result)

# Save the LaTeX table to a .tex file in the working directory
file_name <- "anova_result_econ24.tex"
capture.output(print(anova_tex, type = "latex"), file = file_name)


# create DIF-free score in EES24 dataset
latent_scores <- fscores(model_constrained)
ees24$IRT_econ <- latent_scores

# anchored scores 
latent_scores2 <- fscores(model_multi)
ees24$IRT_econ2 <- latent_scores2


# Fit the multigroup IRT model  using PTVs as anchor columns 
irt_data <- ees24[, c("countryname","issue_immig", "issue_samesex", "PTV_prosocright", "PTV_againstsocleft")]
nms<-colnames(irt_data)

model_multi <- multipleGroup(irt_data[, c("issue_immig", "issue_samesex", "PTV_prosocright", "PTV_againstsocleft")], 
                             1, itemtype = 'graded', group = irt_data$countryname, invariance = c(nms[c(4:5)]))

# Check the model summary
summary(model_multi)

# DIF diagnostic plot
plot(model_multi)

# Edit plot

library(lattice)

# Assuming `model_multi` is already fitted and the plot is stored in `p`
p <- plot(model_multi)

# Modify the trellis plot with the fixes
p <- update(p, 
            main = "Expected Total Score - EES24",  # Title
            sub = "Socio-Cultural Dimension",  # Subtitle
            layout.heights = list(
              top = 3,        # Increase space for the main title
              sub = -10         # Adjust subtitle space
            ),
            panel = function(x, y, groups, ...) {
              # Default plotting of lines
              panel.xyplot(x, y, groups = groups, ...)
              
              # Add country names at the end of each line with jitter
              country_names <- levels(groups)  # Get country names from the groups
              for (i in seq_along(country_names)) {
                # Find the last point of the line for each country
                max_x <- max(x[groups == country_names[i]])  # Maximum value of theta for each country
                max_y <- y[which(x == max_x & groups == country_names[i])]  # Corresponding y value for that country
                
                # # Add jittered country names at the end of the lines
                # jitter_x <- max_x + runif(1, -2, 0)  # Jitter in the x direction
                # jitter_y <- max_y + runif(1, -0.5, 0.8)  # Jitter in the y direction
                
                # Add jittered text at the end of the line
                # panel.text(jitter_x, jitter_y, labels = country_names[i], pos = 4, cex = 0.7)  
                # Add text at the end of the line
                panel.text(max_x, max_y, labels = country_names[i], pos = 4, cex = 0.5)  
              }
            })

# Print the modified plot
print(p)
pdf("DIFsoc24.pdf")
print(p)  # Use print() to display the plot
dev.off() 


# Fit the fully constrained model
model_constrained <- multipleGroup(irt_data[, c("issue_immig", "issue_samesex", "PTV_prosocright", "PTV_againstsocleft")], 
                                   1, itemtype = 'graded', group = irt_data$countryname, invariance = c('free_means', 'free_var', 'intercepts', 'slopes'))

#compare unconstrained and constrained models
anova_result<-anova(model_constrained, model_multi)

# Convert the ANOVA result to a LaTeX table
anova_tex <- xtable(anova_result)

# Save the LaTeX table to a .tex file in the working directory
file_name <- "anova_result_soc24.tex"
capture.output(print(anova_tex, type = "latex"), file = file_name)


# create DIF-free score in EES24 dataset
latent_scores <- fscores(model_constrained)
ees24$IRT_soc <- latent_scores

# anchored scores 
latent_scores2 <- fscores(model_multi)
ees24$IRT_soc2 <- latent_scores2

ees14IRT<- ees14 %>% dplyr::select(respid, countryname, IRT_econ, IRT_econ2, IRT_soc, IRT_soc2)
save(ees14IRT, file = "ees14IRT.RData")

ees19IRT<- ees19 %>% dplyr::select(respid, countryname, IRT_econ, IRT_econ2, IRT_soc, IRT_soc2)
save(ees19IRT, file = "ees19IRT.RData")

ees24IRT<- ees24 %>% dplyr::select(respid, countryname, IRT_econ, IRT_econ2, IRT_soc, IRT_soc2)
save(ees24IRT, file = "ees24IRT.RData")


load("ees14IRT.RData")
load("ees19IRT.RData")
load("ees24IRT.RData")

# Combined Data ----------------------------------------------------------------
ees14IRT$year <- 2014
ees19IRT$year <- 2019
ees24IRT$year <- 2024

eesIRT_all <- bind_rows(ees14IRT, ees19IRT, ees24IRT)
save(eesIRT_all, file = "eesIRT_all.RData")

#create weighted cntry avg yearly dataset
issue_vars <- c("IRT_econ", "IRT_soc")

#function
compute_avg <- function(df, year, countryname) {
  df %>%
    group_by(countryname) %>%
    summarize(across(all_of(issue_vars), 
                     ~ mean(.x, na.rm = TRUE), 
                     .names = "avg_{.col}")) %>%
    mutate(year = year)
}

# For ees14
avg_eesIRT14 <- compute_avg(ees14IRT, 2014)

# For ees19
avg_eesIRT19 <- compute_avg(ees19IRT, 2019)

# For ees24
avg_eesIRT24 <- compute_avg(ees24IRT, 2024)

combdataIRT <- bind_rows(avg_eesIRT14, avg_eesIRT19, avg_eesIRT24)

save(combdataIRT, file = "combdataIRT.RData")

# intermediate step: generate country 2 columns
dyads <- combdataIRT %>%
  dplyr::rename_with(~ paste0(.x, "_1"), -c(countryname, year)) %>%
  dplyr::rename(country1 = countryname) %>%
  inner_join(
    combdataIRT %>%
      dplyr::rename_with(~ paste0(.x, "_2"), -c(countryname, year)) %>%
      dplyr::rename(country2 = countryname),
    by = "year"
  )

# filter out same-country pairs 
dyads <- dyads %>%
  filter(country1 != country2)

# calculate absolute differences for each issue
issue_vars <- c("IRT_econ", "IRT_soc")


# create a long-format dyadic dataset with distances
dyaddataIRT <- dyads %>%
  mutate(
    dist_avg_IRTecon = abs(avg_IRT_econ_1 - avg_IRT_econ_2),
    dist_avg_IRTsoc   = abs(avg_IRT_soc_1 - avg_IRT_soc_2)
  ) %>%
  dplyr::select(year, country1, country2, starts_with("dist_")) %>%
  mutate(dyad = paste0(pmin(country1, country2), pmax(country1, country2)))


save(dyaddataIRT, file = "dyaddataIRT.RData")


# Regressions
# remove duplicates by dyad-year (e.g. ATBE dyad appears six times instead of three, as BE can be country 1 too ... but always recorded as ATBE luckily in original dataframe. Remove dyad-year duplicates) 
dyaddataIRT <- dyaddataIRT %>%
  distinct(year, dyad, .keep_all = TRUE)

# --- Model 1: IRT ECONOMY ---
model_IRTecon <- lm(dist_avg_IRTecon ~ year + factor(dyad), data = dyaddataIRT)

# --- Model 2: IRT SOC ---
model_IRTsoc <- lm(dist_avg_IRTsoc ~ year + factor(dyad), data = dyaddataIRT)

stargazer(model_IRTecon, model_IRTsoc, ord.intercepts = TRUE, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="IRTmeandiffmods_wdyads.tex")
stargazer(model_IRTecon, model_IRTsoc, ord.intercepts = TRUE, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="IRTmeandiffmods_wdyads.htm")

stargazer(model_IRTecon, model_IRTsoc, ord.intercepts = TRUE, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="IRTmeandiffmods_woutdyads.tex", 
          omit = "factor\\(dyad\\)")
stargazer(model_IRTecon, model_IRTsoc, ord.intercepts = TRUE, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="IRTmeandiffmods_woutdyads.htm", 
          omit = "factor\\(dyad\\)")
