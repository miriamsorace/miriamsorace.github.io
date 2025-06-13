
# EES 2014 ------------------------------------------------------------------------------

ees14 <- foreign::read.spss("ees_datasets/ZA5160_v4-1-0.sav",
                            use.value.labels = FALSE,
                            to.data.frame = TRUE,
                            max.value.labels = Inf,
                            trim.factor.names = FALSE,
                            trim_values = FALSE,
                            reencode = NA) %>% 
  mutate(wave = 2014) %>%
  mutate(wt = case_when(
    countrycode == 1826 ~ w4,
    countrycode == 1276 ~ w3,
    TRUE ~ w1
  )) %>%
  mutate(pvv_conservative = case_when(
    countrycode == 1040 ~ qpp8_6,
    countrycode == 1056 ~ qpp8_5,
    countrycode == 1100 ~ qpp8_1,
    countrycode == 1196 ~ qpp8_1,
    countrycode == 1203 ~ qpp8_2,
    countrycode == 1276 ~ qpp8_6,
    countrycode == 1208 ~ qpp8_7,
    countrycode == 1233 ~ qpp8_1,
    countrycode == 1300 ~ qpp8_4,
    countrycode == 1724 ~ qpp8_1,
    countrycode == 1246 ~ qpp8_1,
    countrycode == 1250 ~ qpp8_1,
    countrycode == 1348 ~ qpp8_1,
    countrycode == 1372 ~ qpp8_3,
    countrycode == 1380 ~ qpp8_8,
    countrycode == 1440 ~ qpp8_1,
    countrycode == 1442 ~ qpp8_6,
    countrycode == 1428 ~ qpp8_1,
    countrycode == 1470 ~ qpp8_2,
    countrycode == 1528 ~ qpp8_3,
    countrycode == 1616 ~ qpp8_2,
    countrycode == 1620 ~ qpp8_2,
    countrycode == 1642 ~ qpp8_2,
    countrycode == 1752 ~ qpp8_2,
    countrycode == 1705 ~ qpp8_8,
    countrycode == 1703 ~ qpp8_7,
    countrycode == 1826 ~ qpp8_1,
    countrycode == 1191 ~ qpp8_4
  )) %>%
  dplyr::rename(
    nuts1 = p7_region_nuts1,
    nuts2 = p7r_region_nuts2,
    ptyid = qpp21_ees,
    eunotgood = qp7,
    eifurther = qpp18,
    self_rile = qpp13,
    issue_state_int = qpp17_1,
    issue_redistr = qpp17_2,
    issue_samesex = qpp17_4,
    issue_immig = qpp17_6,
    issue_clim = qpp17_8,
    polint = qp6_9,
    age_cat = d11r2,
    gender = d10,
    ruralurban = d25,
    socclass = d63, 
    religion = d74,
    workstat_full = d15a,
    workstat_cat = d15ar,
    livingstdrd = d61) 

#deal with (-) 9, 97,98,99 values in all columns
ees14 <- ees14 %>%
  mutate(across(everything(), ~ ifelse(. %in% c(-99, -98, -97, -96,  -9, -8, 999, 998, 997, 98, 97, 96, 99), NA, .)))

#fix ptv columns
ees14 <- ees14 %>%
  mutate(across(starts_with("qpp8"), 
                ~ ifelse(. == -7, NA, .))) %>%  # Step 1: replace -7 with NA
  mutate(across(starts_with("qpp8"), 
                ~ . - 1))  # Step 2: subtract 1 to rescale from 1-11 to 0-10

ees14 <- ees14 %>%
  mutate(last_three = substr(ptyid, nchar(ptyid) - 2, nchar(ptyid)),  # Extract last three digits
         first_digit = substr(last_three, 1, 1),  # Extract first digit of last three
         ptyfamid_full = case_when(
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
         ptyfamid_full = factor(ptyfamid_full, levels = c("left", "green", "social democrats","liberals", "christian democrats/centre right", "conservative", "radical right", "niche/fringe", "no party id"))
  )    


ees14 <- ees14 %>%
  mutate(last_three = substr(ptyid, nchar(ptyid) - 2, nchar(ptyid)),  # Extract last three digits
         first_digit = substr(last_three, 1, 1),  # Extract first digit of last three
         ptyfamid = case_when(
           first_digit == "1" ~ "green",
           first_digit == "3" ~ "left",
           first_digit == "2" ~ "left",
           first_digit == "4" ~ "liberals",
           first_digit == "5" ~ "right",
           first_digit == "6" ~ "right",
           as.numeric(first_digit) >= 7 & as.numeric(first_digit) < 9 ~ "right",
           first_digit == "9" ~ "niche/fringe",
           TRUE ~ "no party id"
         ),
         ptyfamid = factor(ptyfamid, levels = c("left", "green", "liberals", "right", "niche/fringe", "no party id"))
  )    

ees14 <- ees14 %>%
  mutate(edu = case_when(
    d8 == 1 ~ "less than 15 years",
    d8 == 2 ~ "16-19 years",
    d8 == 3 ~ "20+ years",
    d8 == 4 ~ "still studying",
    d8 == 5 ~ "none",
    TRUE ~ NA_character_
  )) %>%
  mutate(edu = factor(edu, levels = c("none", "less than 15 years", "still studying", "16-19 years", "20+ years")))

ees14 <- ees14 %>%
  mutate(female = case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female",
    TRUE ~ NA_character_
  )) %>%
  mutate(female = factor(female, levels = c("Male", "Female")))

ees14 <- ees14 %>%
  mutate(ruralurban = case_when(
    ruralurban == 1 ~ "Rural area/village",
    ruralurban == 2 ~ "Small/Mid-sized town",
    ruralurban == 3 ~ "Large town/city",
    TRUE ~ NA_character_
  )) %>%
  mutate(ruralurban = factor(ruralurban, levels = c("Rural area/village", "Small/Mid-sized town", "Large town/city")))


ees14 <- ees14 %>%
  mutate(socclass = case_when(
    socclass == 1 ~ "Working class",
    socclass == 2 ~ "Middle class",
    socclass == 3 ~ "Upper class",
    socclass == 4 ~ "Other",
    TRUE ~ NA_character_
  )) %>%
  mutate(socclass = factor(socclass, levels = c("Working class", "Middle class", "Upper class", "Other")))


ees14 <- ees14 %>%
  dplyr::select(respid, wt, countrycode, nuts1, nuts2, pvv_conservative, ptyid, ptyfamid, ptyfamid_full, eunotgood, eifurther, self_rile,
                starts_with("issue_"), polint, edu, age_cat, female, ruralurban, socclass, religion, workstat_full, workstat_cat, livingstdrd, qpp8_1, qpp8_2, qpp8_3, qpp8_4, qpp8_5, qpp8_6, qpp8_7, qpp8_8)

# ees '14 scales always start at 1. Subtract 1 to make them comparable to ees 19
ees14 <- ees14 %>% mutate(pvv_conservative = pvv_conservative -1)
ees14 <- ees14 %>% mutate(eifurther = eifurther -1)
ees14 <- ees14 %>% mutate(self_rile = self_rile -1)
# ees14 <- ees14 %>% mutate_at(vars(starts_with("issue_")), ~. - 1)
ees14 <- ees14 %>% mutate(across(starts_with("issue_"), ~ . - 1))


# EES 2019 ---------------------------------------------------------------------------

ees19 <- foreign::read.spss("ees_datasets/ZA7581_v2-0-1.sav",
                            use.value.labels = FALSE,
                            to.data.frame = TRUE,
                            max.value.labels = Inf,
                            trim.factor.names = FALSE,
                            trim_values = FALSE,
                            reencode = NA) %>% 
  mutate(wave = 2019) %>%
  mutate(D2_1 = case_when(D2_1 >= 97 ~ -1, TRUE ~ D2_1))

#deal with (-) 9, 97,98,99 values in all columns
ees19 <- ees19 %>%
  mutate(across(everything(), ~ ifelse(. %in% c(-99, -98, -97, -96,  -9, -8, 999, 998, 997, 98, 97, 96, 99), NA, .)))


ees19 <- ees19 %>%
  mutate(age = 2019-D4_1) %>%
  mutate(age_cat = factor(case_when(
    age <= 24 ~ "16/18-24",
    age > 24 & age <= 34 ~ "25-34",
    age > 34 & age <= 44 ~ "35-44",
    age > 44 & age <= 54 ~ "45-54",
    age > 54 & age <= 64 ~ "55-64",
    age > 64 ~ "65+"
  ), levels = c("16/18-24", "25-34", "35-44", "45-54", "55-64", "65+"))) %>%
  mutate(pvv_conservative = case_when(
    countrycode == 1040 ~ q10_6,
    countrycode == 1056 ~ q10_5,
    countrycode == 1100 ~ q10_1,
    countrycode == 1196 ~ q10_2,
    countrycode == 1203 ~ q10_8,
    countrycode == 1276 ~ q10_6,
    countrycode == 1208 ~ q10_7,
    countrycode == 1233 ~ q10_4,
    countrycode == 1300 ~ q10_7,
    countrycode == 1724 ~ q10_2,
    countrycode == 1246 ~ q10_3,
    countrycode == 1250 ~ q10_1,
    countrycode == 1348 ~ q10_3,
    countrycode == 1372 ~ q10_3,
    countrycode == 1380 ~ q10_7,
    countrycode == 1440 ~ q10_1,
    countrycode == 1442 ~ q10_6,
    countrycode == 1428 ~ q10_7,
    countrycode == 1470 ~ q10_2,
    countrycode == 1528 ~ q10_2,
    countrycode == 1616 ~ q10_2,
    countrycode == 1620 ~ q10_2,
    countrycode == 1642 ~ q10_5,
    countrycode == 1752 ~ q10_2,
    countrycode == 1705 ~ q10_6,
    countrycode == 1703 ~ q10_6,
    countrycode == 1826 ~ q10_1,
    countrycode == 1191 ~ q10_5
  )) %>%
  dplyr::rename(
    wt = WGT3,
    nuts1 = region_NUTS1,
    nuts2 = region_NUTS2,
    ptyid = Q25_EES,
    eunotgood = Q22,
    eifurther = Q23,
    self_rile = Q11,
    issue_state_int = Q14_1,
    issue_redistr = Q14_2,
    issue_samesex = Q14_3,
    issue_immig = Q14_5,
    issue_clim = Q14_6,
    polint = Q21,
    gender = D3,
    ruralurban = D8,
    socclass = D7, 
    religion = D9,
    workstat_cat = D6,
    livingstdrd = D11) 

ees19 <- ees19 %>%
  mutate(last_three = substr(ptyid, nchar(ptyid) - 2, nchar(ptyid)),  # Extract last three digits
         first_digit = substr(last_three, 1, 1),  # Extract first digit of last three
         ptyfamid_full = case_when(
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
         ptyfamid_full = factor(ptyfamid_full, levels = c("left", "green", "social democrats", "liberals", "christian democrats/centre right", "conservative", "radical right", "niche/fringe", "no party id"))
  ) 

ees19 <- ees19 %>%
  mutate(last_three = substr(ptyid, nchar(ptyid) - 2, nchar(ptyid)),  # Extract last three digits
         first_digit = substr(last_three, 1, 1),  # Extract first digit of last three
         ptyfamid = case_when(
           first_digit == "1" ~ "green",
           first_digit == "3" ~ "left",
           first_digit == "2" ~ "left",
           first_digit == "4" ~ "liberals",
           first_digit == "5" ~ "right",
           first_digit == "6" ~ "right",
           as.numeric(first_digit) >= 7 & as.numeric(first_digit) < 9 ~ "right",
           first_digit == "9" ~ "niche/fringe",
           TRUE ~ "no party id"
         ),
         ptyfamid = factor(ptyfamid, levels = c("left", "green", "liberals", "right", "niche/fringe", "no party id"))
  ) 

ees19 <- ees19 %>% mutate(
  edu = case_when(
    D2_1 == 0 ~ "none",
    D2_1 == -1 ~ "still studying",
    (D2_1 > 0 & D2_1 <= 15) ~ "less than 15 years",
    (D2_1 >= 16 & D2_1 <= 19) ~ "16-19 years",
    (D2_1 >= 20) ~ "20+ years",
    TRUE ~ NA_character_
  )) %>%
  mutate(edu = factor(edu, levels = c("none", "less than 15 years", "still studying", "16-19 years", "20+ years")))

ees19 <- ees19 %>%
  mutate(female = case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female",
    TRUE ~ NA_character_
  )) %>%
  mutate(female = factor(female, levels = c("Male", "Female")))

ees19 <- ees19 %>%
  mutate(ruralurban = case_when(
    ruralurban == 1 ~ "Rural area/village",
    ruralurban == 2 ~ "Small/Mid-sized town",
    ruralurban == 3 ~ "Large town/city",
    TRUE ~ NA_character_
  )) %>%
  mutate(ruralurban = factor(ruralurban, levels = c("Rural area/village", "Small/Mid-sized town", "Large town/city")))

ees19 <- ees19 %>%
  mutate(socclass = case_when(
    socclass %in% c(1,2) ~ "Working class",
    socclass %in% c(3,4) ~ "Middle class",
    socclass == 5 ~ "Upper class",
    socclass == 6 ~ "Other",
    TRUE ~ NA_character_
  )) %>%
  mutate(socclass = factor(socclass, levels = c("Working class", "Middle class", "Upper class", "Other")))


ees19 <- ees19 %>%
  dplyr::select(respid, wt, countrycode, nuts1, nuts2, ptyid, ptyfamid, ptyfamid_full,pvv_conservative, eunotgood, eifurther, self_rile,
                starts_with("issue_"), polint, edu, age_cat, female, ruralurban, socclass, religion,  workstat_cat, livingstdrd, q10_1, q10_2, q10_3, q10_4, q10_5, q10_6, q10_7, q10_8)

#additional data clean

## geographies
ees19$nuts1 <- trimws(ees19$nuts1)
ees19$nuts2 <- trimws(ees19$nuts2)
ees14$nuts2 <- trimws(ees14$nuts2)


## age
ees14 <- ees14 %>%
  mutate(age_cat = factor(case_when(
    age_cat == 1 ~ "16/18-24",
    age_cat == 2 ~ "25-34",
    age_cat == 3 ~ "35-44",
    age_cat == 4 ~ "45-54",
    age_cat == 5 ~ "55-64",
    age_cat == 6 ~ "65+"
  ), levels = c("16/18-24", "25-34", "35-44", "45-54", "55-64", "65+")))



# ideological scales: reverse order to match variable name (typically give right-wing end in higher values)

ees14 <- ees14 %>%
  mutate(eunotgood = case_when(
    eunotgood == 1 ~ 1,
    eunotgood == 2 ~ 3,
    is.na(eunotgood) ~ NA_real_,
    TRUE ~ 2
  ),
  issue_immig = case_when(
    issue_immig == 0 ~ 10,
    issue_immig == 1 ~ 9,
    issue_immig == 2 ~ 8,
    issue_immig == 3 ~ 7,
    issue_immig == 4 ~ 6,
    issue_immig == 5 ~ 5,
    issue_immig == 6 ~ 4,
    issue_immig == 7 ~ 3,
    issue_immig == 8 ~ 2,
    issue_immig == 9 ~ 1,
    issue_immig == 10 ~ 0,
    TRUE ~ NA_real_
  ))


ees19 <- ees19 %>%
  mutate(eunotgood = case_when(
    eunotgood == 1 ~ 1,
    eunotgood == 2 ~ 3,
    is.na(eunotgood) ~ NA_real_,
    TRUE ~ 2
  ),
  issue_immig = case_when(
    issue_immig == 0 ~ 10,
    issue_immig == 1 ~ 9,
    issue_immig == 2 ~ 8,
    issue_immig == 3 ~ 7,
    issue_immig == 4 ~ 6,
    issue_immig == 5 ~ 5,
    issue_immig == 6 ~ 4,
    issue_immig == 7 ~ 3,
    issue_immig == 8 ~ 2,
    issue_immig == 9 ~ 1,
    issue_immig == 10 ~ 0,
    TRUE ~ NA_real_
  ))

# political interest
ees14 <- ees14 %>%
  mutate(polint = case_when(
    polint == 1 ~ 4,
    polint == 2 ~ 3,
    polint == 3 ~ 2,
    polint == 4 ~ 1,
    TRUE ~ NA_real_
  ))

ees19 <- ees19 %>%
  mutate(polint = case_when(
    polint == 1 ~ 4,
    polint == 2 ~ 3,
    polint == 3 ~ 2,
    polint == 4 ~ 1,
    TRUE ~ NA_real_
  ))

# social class

ees14 <- ees14 %>%
  mutate(religion = case_when(
    religion == 1 ~ "Catholic",
    religion %in% c(2, 3, 4) ~ "Other Christian",
    religion %in% c(5, 6, 7, 8, 9) ~ "Other",
    religion %in% c(10, 11) ~ "Atheist/Agnostic",
    religion == 12 ~ "Other"
  )) %>%
  mutate(religion = factor(religion, levels = c(
    "Catholic",
    "Other Christian",
    "Atheist/Agnostic",
    "Other"
  )))

ees19 <- ees19 %>%
  mutate(religion = case_when(
    religion == 1 ~ "Catholic",
    religion %in% c(2, 3, 4) ~ "Other Christian",
    religion %in% c(5, 6, 7, 8, 9) ~ "Other",
    religion %in% c(10, 11) ~ "Atheist/Agnostic",
    religion == 12 ~ "Other"
  )) %>%
  mutate(religion = factor(religion, levels = c(
    "Catholic",
    "Other Christian",
    "Atheist/Agnostic",
    "Other"
  )))

ees14 <- ees14 %>%
  mutate(workstat_cat = case_when(
    workstat_full %in% c(5,6,7,8,9) ~ "Self-employed", # Farmers, Fisherman, Professionals, Shop owners
    workstat_full %in% c(2,3) ~ "Student/Unemployed", # Unemployed
    workstat_full == 1 ~ "Household worker", # Those responsible for shopping/household
    workstat_full == 4 ~ "Retired", # Retired
    TRUE ~ "Employed"
  )) %>%
  mutate(workstat_cat = factor(workstat_cat, levels = c(
    "Employed","Student/Unemployed", "Household worker", "Self-employed", "Retired"
  )))

ees19 <- ees19 %>%
  mutate(workstat_cat = case_when(
    workstat_cat == 1 ~ "Self-employed", # Farmers, Fisherman, Professionals, Shop owners
    workstat_cat %in% c(6,3) ~ "Student/Unemployed", # Unemployed
    workstat_cat == 4 ~ "Household worker", # Those responsible for shopping/household
    workstat_cat == 5 ~ "Retired", # Retired
    workstat_cat == 7 ~ "Other",
    TRUE ~ "Employed"
  )) %>%
  mutate(workstat_cat = factor(workstat_cat, levels = c(
    "Employed","Student/Unemployed", "Household worker", "Self-employed", "Retired", "Other"
  )))

# living standard
ees14 <- ees14 %>% mutate(
  goodlivingstd = case_when(
    livingstdrd %in% c(1,2) ~ 1,
    livingstdrd == 3 ~ 2,
    livingstdrd == 4 ~ 3,
    livingstdrd == 5 ~ 4,
    livingstdrd %in% c(6,7) ~ 5,
    livingstdrd %in% c(8, 9) ~ 6,
    livingstdrd == 10 ~ 7
  )
)

ees19 <- ees19 %>% mutate(goodlivingstd = livingstdrd)

# add country label
ees14 <- mutate(ees14, countryname = ifelse(nuts2 == "-9", "UK", substr(nuts2, 1, 2)))
ees19 <- mutate(ees19, countryname = ifelse(nuts2 == "96", "UK", substr(nuts2, 1, 2)))
ees14 <- mutate(ees14, countryname = case_when(countrycode == 1276 ~ "DE",
                                               countrycode == 1250 ~ "FR", 
                                               TRUE ~ countryname))

ees19 <- mutate(ees19, countryname = case_when(countrycode == 1276 ~ "DE",
                                               countrycode == 1250 ~ "FR", 
                                               countryname == "Mt" ~ "MT",
                                               TRUE ~ countryname))

# EES 2024 -----------------------------------------------------------------------------------

ees24 <- read_dta("ees_datasets/ZA8868_v1-0-0.dta") %>%
  mutate(wave = 2024) %>%
  mutate(wt = Weight2) %>%
  mutate(nuts1 = NA,
         nuts2 = NA,
         issue_state_int = NA) %>%
  dplyr::rename(
    respid = resp_id,
    ptyid = q19_ees,
    eunotgood = q16,
    eifurther = q17,
    self_rile = q10,
    issue_redistr = q12_2,
    issue_samesex = q12_3,
    issue_immig = q12_4,
    issue_clim = q12_5,
    polint = q15,
    age = d4_age,
    gender = d3,
    ruralurban = d8,
    socclass = d7, 
    religion = d9,
    workstat_cat = d6,
    livingstdrd = d11) 

#deal with (-) 9, 97,98,99 values in all columns
ees24 <- ees24 %>%
  mutate(across(everything(), ~ ifelse(. %in% c(-99, -98, -97, -96,  -9, -8, 999, 998, 997, 98, 97, 96, 99), NA, .)))

# #add EES party codes to ptyid to allow party family classification
# ptycodes<-read.csv("/Users/miriamsorace/Desktop/WORK/4. MY RESEARCH/1. EU /5. EP24/1. No Demos?/wd/Numerical_Party_Codes_09042024.csv") %>%
#   dplyr::rename(ptyid = Q19,
#          prevcode = EES.2019.2014.2009.Code) %>%
#   dplyr::select(ptyid, prevcode) %>%
#   filter(!is.na(ptyid) & ptyid != "") %>%
#   filter(!is.na(prevcode)) %>%
#   separate_rows(ptyid, sep = " & ") %>%
#   mutate(ptyid = as.numeric(ptyid)) %>% distinct(ptyid, .keep_all = TRUE)
# 
# ees24 <- ees24 %>% left_join(ptycodes, by="ptyid")

ees24 <- ees24 %>%
  mutate(last_three = substr(ptyid, nchar(ptyid) - 2, nchar(ptyid)),  # Extract last three digits
         first_digit = substr(last_three, 1, 1),  # Extract first digit of last three
         ptyfamid_full = case_when(
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
         ptyfamid_full = factor(ptyfamid_full, levels = c("left", "green", "social democrats","liberals", "christian democrats/centre right", "conservative", "radical right", "niche/fringe", "no party id"))
  ) 

ees24 <- ees24 %>%
  mutate(last_three = substr(ptyid, nchar(ptyid) - 2, nchar(ptyid)),  # Extract last three digits
         first_digit = substr(last_three, 1, 1),  # Extract first digit of last three
         ptyfamid = case_when(
           first_digit == "1" ~ "green",
           first_digit == "3" ~ "left",
           first_digit == "2" ~ "left",
           first_digit == "4" ~ "liberals",
           first_digit == "5" ~ "right",
           first_digit == "6" ~ "right",
           as.numeric(first_digit) >= 7 & as.numeric(first_digit) < 9 ~ "right",
           first_digit == "9" ~ "niche/fringe",
           TRUE ~ "no party id"
         ),
         ptyfamid = factor(ptyfamid, levels = c("left", "green", "liberals", "right", "niche/fringe", "no party id"))
  ) 

ees24 <- ees24 %>%
  mutate(edu = case_when(
    d2 <= 2 ~ "less than 15 years",
    d2 == 3 ~ "16-19 years",
    d2 == 4 ~ "16-19 years",
    d2 > 4 & d2 < 97 ~ "20+ years",
    d2 == 97 ~ NA_character_
  )) %>%
  mutate(edu = factor(edu, levels = c("less than 15 years", "16-19 years", "20+ years")))


ees24 <- ees24 %>%
  mutate(female = case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female",
    TRUE ~ NA_character_
  )) %>%
  mutate(female = factor(female, levels = c("Male", "Female")))

ees24 <- ees24 %>%
  mutate(ruralurban = case_when(
    ruralurban == 1 ~ "Rural area/village",
    ruralurban == 2 ~ "Small/Mid-sized town",
    ruralurban == 3 ~ "Large town/city",
    TRUE ~ NA_character_
  )) %>%
  mutate(ruralurban = factor(ruralurban, levels = c("Rural area/village", "Small/Mid-sized town", "Large town/city")))


ees24 <- ees24  %>%
  mutate(socclass = case_when(
    socclass %in% c(1,2) ~ "Working class",
    socclass %in% c(3,4) ~ "Middle class",
    socclass == 5 ~ "Upper class",
    socclass == 6 ~ "Other",
    TRUE ~ NA_character_
  )) %>%
  mutate(socclass = factor(socclass, levels = c("Working class", "Middle class", "Upper class", "Other")))

# ees24 <- ees24 %>% #based on language variable and table in tech report, but need ISO countrycode variable in future
#   mutate(countryname = str_sub(language, -2)) %>%   # Extract the last 2 characters
#   mutate(countryname = str_to_upper(countryname)) %>%
#   mutate(countryname = case_when(
#     countryname == "DA" ~ "DK", 
#     countryname == "ET" ~ "EE", 
#     countryname == "SV" ~ "SE",  
#     countryname == "FL" ~ "FI",  
#     countryname == "RU" ~ "LV", 
#     countryname == "SL" ~ "SI", 
#     TRUE ~ countryname  # Keep original if no match
#   ))

ees24 <- ees24 %>% 
  mutate(countryname = case_when(
    country_alpha2 == "AU" ~ "AT",
    TRUE ~ country_alpha2
  ))

ees24 <- ees24 %>%
  mutate(age_cat = factor(case_when(
    age <=24 ~ "16/18-24",
    age >24 & age <= 34 ~ "25-34",
    age >34 & age <= 44 ~ "35-44",
    age >44 & age <= 54 ~ "45-54",
    age >54 & age <= 64 ~ "55-64",
    age >= 65 ~ "65+",
    is.na(age) ~ NA, 
  ), levels = c("16/18-24", "25-34", "35-44", "45-54", "55-64", "65+")))

ees24 <- ees24 %>%
  dplyr::select(respid, wt, countryname, nuts1, nuts2, ptyid, ptyfamid, ptyfamid_full, eunotgood, eifurther, self_rile,
                starts_with("issue_"), polint, edu, age_cat, female, ruralurban, socclass, religion, workstat_cat, livingstdrd, q9_1, q9_2, q9_3, q9_4, q9_5, q9_6, q9_7, q9_8)

# ideological scales: reverse order to match variable name (typically give right-wing end in higher values)
ees24 <- ees24 %>%
  mutate(eunotgood = case_when(
    eunotgood == 1 ~ 1,
    eunotgood == 2 ~ 3,
    is.na(eunotgood) ~ NA_real_,
    TRUE ~ 2
  ),
  issue_immig = case_when(
    issue_immig == 0 ~ 10,
    issue_immig == 1 ~ 9,
    issue_immig == 2 ~ 8,
    issue_immig == 3 ~ 7,
    issue_immig == 4 ~ 6,
    issue_immig == 5 ~ 5,
    issue_immig == 6 ~ 4,
    issue_immig == 7 ~ 3,
    issue_immig == 8 ~ 2,
    issue_immig == 9 ~ 1,
    issue_immig == 10 ~ 0,
    TRUE ~ NA_real_
  ))


# political interest
ees24 <- ees24 %>%
  mutate(polint = case_when(
    polint == 1 ~ 4,
    polint == 2 ~ 3,
    polint == 3 ~ 2,
    polint == 4 ~ 1,
    TRUE ~ NA_real_
  ))


ees24 <- ees24 %>%
  mutate(religion = case_when(
    religion == 1 ~ "Catholic",
    religion %in% c(2, 3, 4) ~ "Other Christian",
    religion %in% c(5, 6, 7, 8, 9) ~ "Other",
    religion %in% c(10, 11) ~ "Atheist/Agnostic",
    religion == 12 ~ "Other"
  )) %>%
  mutate(religion = factor(religion, levels = c(
    "Catholic",
    "Other Christian",
    "Atheist/Agnostic",
    "Other"
  )))


ees24 <- ees24 %>%
  mutate(workstat_cat = case_when(
    workstat_cat == 1 ~ "Self-employed", # Farmers, Fisherman, Professionals, Shop owners
    workstat_cat %in% c(3,6,7) ~ "Student/Unemployed", # Unemployed
    workstat_cat == 4 ~ "Household worker", # Those responsible for shopping/household
    workstat_cat == 5 ~ "Retired", # Retired
    workstat_cat == 9 ~ "Other",
    TRUE ~ "Employed"
  )) %>%
  mutate(workstat_cat = factor(workstat_cat, levels = c(
    "Employed","Student/Unemployed", "Household worker", "Self-employed", "Retired", "Other"
  )))

ees24 <- ees24 %>% mutate(goodlivingstd = livingstdrd)

# sense checks

# Compute the averages for all 'issue_' variables grouped by 'ptyfamid'
average_table_14 <- ees14 %>%
  dplyr::select(starts_with("issue_"), ptyfamid_full) %>%
  group_by(ptyfamid_full) %>%
  dplyr::summarise(across(starts_with("issue_"), ~ mean(.x, na.rm = TRUE)))

# Print the result
print(average_table_14)


average_table_19 <- ees19 %>%
  dplyr::select(starts_with("issue_"), ptyfamid_full) %>%
  group_by(ptyfamid_full) %>%
  dplyr::summarise(across(starts_with("issue_"), ~ mean(.x, na.rm = TRUE)))

# Print the result
print(average_table_19)

average_table_24 <- ees24 %>%
  dplyr::select(starts_with("issue_"), ptyfamid_full) %>%
  group_by(ptyfamid_full) %>%
  dplyr::summarise(across(starts_with("issue_"), ~ mean(.x, na.rm = TRUE)))

# Print the result
print(average_table_24)

# DROP THE UK

ees14 <- ees14[ees14$countryname != "UK", ]
ees19 <- ees19[ees19$countryname != "UK", ]
ees24 <- ees24[ees24$countryname != "UK", ]


save(ees14, file = "ees14clean.RData")
save(ees19, file = "ees19clean.RData")
save(ees24, file = "ees24clean.RData")

# Combined Data ----------------------------------------------------------------
ees14$year <- 2014
ees19$year <- 2019
ees24$year <- 2024

ees_all <- bind_rows(ees14, ees19, ees24)
save(ees_all, file = "ees_all.RData")


# Combined Avg Data ------------------------------------------------------------
#create weighted cntry avg yearly dataset
issue_vars <- c("issue_state_int", "issue_redistr", "issue_samesex", "issue_immig", "issue_clim")

#function
compute_weighted_avg <- function(df, year, countryname) {
  df %>%
    filter(!is.na(wt)) %>%
    group_by(countryname) %>%
    summarize(across(all_of(issue_vars), 
                     ~ weighted.mean(.x, wt, na.rm = TRUE), 
                     .names = "avg_{.col}")) %>%
    mutate(year = year)
}

# For ees14
avg_ees14 <- compute_weighted_avg(ees14, 2014)

# For ees19
avg_ees19 <- compute_weighted_avg(ees19, 2019)

# For ees24
avg_ees24 <- compute_weighted_avg(ees24, 2024)

combdata <- bind_rows(avg_ees14, avg_ees19, avg_ees24)

save(combdata, file = "combdata.RData")

# Dyadic Mean Diffs Data -------------------------------------------------

# intermediate step: generate country 2 columns
dyads <- combdata %>%
  dplyr::rename_with(~ paste0(.x, "_1"), -c(countryname, year)) %>%
  dplyr::rename(country1 = countryname) %>%
  inner_join(
    combdata %>%
      dplyr::rename_with(~ paste0(.x, "_2"), -c(countryname, year)) %>%
      dplyr::rename(country2 = countryname),
    by = "year"
  )

# filter out same-country pairs 
dyads <- dyads %>%
  filter(country1 != country2)

# calculate absolute differences for each issue
issue_vars <- c("avg_issue_state_int", "avg_issue_redistr", 
                "avg_issue_samesex", "avg_issue_immig", "avg_issue_clim")

# create a long-format dyadic dataset with distances
dyaddata <- dyads %>%
  mutate(
    dist_avg_issue_state_int = abs(avg_issue_state_int_1 - avg_issue_state_int_2),
    dist_avg_issue_redistr   = abs(avg_issue_redistr_1 - avg_issue_redistr_2),
    dist_avg_issue_samesex   = abs(avg_issue_samesex_1 - avg_issue_samesex_2),
    dist_avg_issue_immig     = abs(avg_issue_immig_1 - avg_issue_immig_2),
    dist_avg_issue_clim      = abs(avg_issue_clim_1 - avg_issue_clim_2)
  ) %>%
  dplyr::select(year, country1, country2, starts_with("dist_")) %>%
  mutate(dyad = paste0(pmin(country1, country2), pmax(country1, country2)))


save(dyaddata, file = "dyaddata.RData")