## replication file: Sorace, M. and Zaun, N. "Dimension-Specific Party and Public Opinion Responsiveness in the EU Immigration Acquis"

# Installing Packages  -----------------------------------------------------
## Helper function --------------------------------------------------------
usePackage <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(
      pkg,
      dependencies = TRUE,
      repos = "https://cran.rstudio.com/",
      INSTALL_opts = "--no-lock"
    )
  }
  library(pkg, character.only = TRUE)
}

## CRAN packages ----------------------------------------------------------
cran_packages <- c(
  "quanteda",
  "quanteda.textmodels",
  "quanteda.textplots",
  "quanteda.textstats",
  "tidyr",
  "foreign",
  "readtext",
  "dplyr",
  "sjmisc",
  "modeest",
  "ggplot2",
  "margins",
  "psych",
  "irrCAC",
  "data.table",
  "stargazer",
  "haven",
  "openxlsx",   # Use openxlsx instead of xlsx to avoid Java
  "readxl",
  "readr",
  "doBy",
  "sandwich",
  "lmtest",
  "QuantPsyc",
  "plm",
  "collapse",
  "marginaleffects",
  "ggpubr",
  "cowplot",
  "interactions",
  "scales",
  "lme4",
  "devtools",   # for GitHub packages
  "remotes"     # for ggpattern
)

invisible(lapply(cran_packages, usePackage))

## GitHub packages --------------------------------------------------------
# quanteda corpora
if (!requireNamespace("quanteda.corpora", quietly = TRUE)) {
  devtools::install_github("quanteda/quanteda.corpora")
}
library(quanteda.corpora)

# ggpattern
if (!requireNamespace("ggpattern", quietly = TRUE)) {
  remotes::install_github("coolbutuseless/ggpattern")
}
library(ggpattern)

## Stargazer R >= 4.2 hotfix --------------------------------------------
if (packageVersion("stargazer") <= "5.2.3") {
  
  if ("package:stargazer" %in% search()) detach("package:stargazer", unload = TRUE)
  remove.packages("stargazer")
  
  download.file(
    "https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz",
    destfile = "stargazer_5.2.3.tar.gz"
  )
  
  untar("stargazer_5.2.3.tar.gz")
  src <- readLines("stargazer/R/stargazer-internal.R")
  
  # Apply hotfix
  src[1990] <- src[1995]
  src[1995] <- ""
  
  writeLines(src, "stargazer/R/stargazer-internal.R")
  
  install.packages("stargazer", repos = NULL, type = "source")
  library(stargazer)
}

message("✅ All packages installed and loaded successfully!")

# Set Working Directory to Source File Location -----------------------------------------------------
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  path <- rstudioapi::getActiveDocumentContext()$path
  if (nzchar(path)) {
    setwd(dirname(path))
  } else {
    stop("Save the .R file before running this script.")
  }
}

# Manuscript ----------------------------------------------------------------------------------------

## Descriptives: Figures 1 & 2 ----------------------------------------------------------------------
### Fig 1-------------------
LegScoreData <- read.csv("leg_data_wscores.csv")

LegScoreData$PrType <-
  ifelse(
    LegScoreData$ProcedureType == "" &
      LegScoreData$ID == "31990L0366",
    "Consultation",
    ifelse(
      LegScoreData$ProcedureType == "" &
        LegScoreData$ID == "31993L0096",
      "Cooperation",
      ifelse(
        LegScoreData$ProcedureType == "" &
          LegScoreData$ID == "31995R2317",
        "Consultation",
        ifelse(
          LegScoreData$ProcedureType == "" &
            LegScoreData$ID == "31999R0574",
          "Consultation",
          ifelse(
            LegScoreData$ProcedureType == "" &
              LegScoreData$ID == "32002R0333",
            "Consultation",
            ifelse(
              LegScoreData$ProcedureType == "" &
                LegScoreData$ID == "32008R0587",
              "Council Only",
              ifelse(
                LegScoreData$ProcedureType == "" &
                  LegScoreData$ID == "32013R1417",
                "Co-Decision",
                ifelse(
                  LegScoreData$ProcedureType == "" &
                    LegScoreData$ID == "32016R1953",
                  "Co-Decision",
                  ifelse(
                    LegScoreData$ProcedureType == "" &
                      LegScoreData$ID == "32016R0399",
                    "Co-Decision",
                    ifelse(
                      LegScoreData$ProcedureType == "" &
                        LegScoreData$ID == "32016R1625",
                      "Co-Decision",
                      ifelse(
                        LegScoreData$ProcedureType == "" &
                          LegScoreData$ID == "32016R1624",
                        "Co-Decision",
                        ifelse(
                          LegScoreData$ProcedureType ==
                            "Non-Legislative",
                          "Consultation",
                          LegScoreData$ProcedureType
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

df<-LegScoreData

# Extract the year from the ID column (characters 2 to 5)
df$year <- df$IMPORT_YEAR


p <- ggplot(df, aes(x = year, y = WS)) +
  geom_point() +                          # Dots for the scatter plot
  geom_smooth(method = "loess", se = FALSE, color = "red4") +  # Moving average (Loess smoothing)
  labs(x = "Year", y = "Document Restrictiveness Score") +  # Custom axis labels
  scale_x_continuous(breaks = unique(df$year)) +  # Ensure each unique year is shown once
  scale_y_continuous(limits = c(0, 2)) + 
  theme_minimal() +  # Use a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate labels
  ggtitle("EU Immigration Legislation Restrictiveness by Year")
p

# Save the plot as a PNG
ggsave(filename = "tables&figures/EU_Immigration_Restrictiveness.png",plot = p, width = 10,height = 6,dpi = 300)

### Fig 2 -------------------
p <- ggplot(df, aes(x = year, y = WS_INT)) +
  geom_point() +                          # Dots for the scatter plot
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Moving average (Loess smoothing)
  labs(x = "Year", y = "Document Pro-Sovereignty Score") +  # Custom axis labels
  scale_x_continuous(breaks = unique(df$year)) +  # Ensure each unique year is shown once
  scale_y_continuous(limits = c(0, 2)) + 
  theme_minimal() +  # Use a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate labels
  ggtitle("EU Immigration Legislation Pro-National Sovereignty by Year")
p
# Save the plot as a PNG
ggsave(filename = "tables&figures/EU_Immigration_Sovereignty.png",plot = p, width = 10,height = 6,dpi = 300)


## Baseline Regression  ----------------------------------------------------------------------
LegScoreVotePtyData <- readRDS("LegScoreVotePtyData.rds")

#specify baselines of factor variables
LegScoreVotePtyData <- within(LegScoreVotePtyData, EPG <- relevel(EPG, ref = 2))

#check for duplicates & remove
LegScoreVotePtyData <- LegScoreVotePtyData %>% distinct(NEW_VOTE_ID, NEW_PARTY_ID, .keep_all = TRUE)

# leg ideology effects
legfm <- glm(p_yes ~ WS_INT + WS + EPG + country + as.factor(YEAR) , data = LegScoreVotePtyData, family = quasibinomial(link = "logit"))
summary(legfm)
#for robust SE:
legfmr<-coeftest(legfm, vcov = vcovHC(legfm, type = "HC0"))
legfmr

n_legfmr <- nobs(legfm)

#for aic/bic:
legfm <- glm(p_yes ~ WS_INT + WS + EPG + country + as.factor(YEAR) , data = LegScoreVotePtyData, family = binomial(link = "logit"))
aic_legfmr <- AIC(legfm)
bic_legfmr <- BIC(legfm)

# w/out country FE

# leg ideology effects
legfm2 <- glm(p_yes ~ WS_INT + WS + EPG + as.factor(YEAR) , data = LegScoreVotePtyData, family = quasibinomial(link = "logit"))
summary(legfm2)
#for robust SE:
legfm2r<-coeftest(legfm2, vcov = vcovHC(legfm2, type = "HC0"))
legfm2r

n_legfm2r <- nobs(legfm2)

#for aic/bic
legfm2 <- glm(p_yes ~ WS_INT + WS + EPG + as.factor(YEAR) , data = LegScoreVotePtyData, family = binomial(link = "logit"))
aic_legfm2R<- AIC(legfm2)
bic_legfm2R<- BIC(legfm2)

# w/out EPG fe

# leg ideology effects
legfm3 <- glm(p_yes ~ WS_INT + WS + as.factor(YEAR) , data = LegScoreVotePtyData, family = quasibinomial(link = "logit"))
summary(legfm3)
#for robust SE:
legfm3r<-coeftest(legfm3, vcov = vcovHC(legfm3, type = "HC0"))
legfm3r

n_legfm3r <- nobs(legfm3)

#for aic/bic
legfm3 <- glm(p_yes ~ WS_INT + WS + as.factor(YEAR) , data = LegScoreVotePtyData, family = binomial(link = "logit"))
aic_legfm3R<- AIC(legfm3)
bic_legfm3R<- BIC(legfm3)

stargazer(legfm3r,legfm2r, legfmr,  apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/DescRegMod.htm",
          add.lines = list(c("Country FE", "N", "N", "Y"),
                           c("EPG FE", "N", "Y", "Y"),
                           c("Year FE", "Y", "Y", "Y"),
                           c("Observations", n_legfm3r, n_legfm2r, n_legfmr),
                           c("AIC", round(aic_legfm3R, 2), round(aic_legfm2R, 2), round(aic_legfmr, 2)),
                           c("BIC", round(bic_legfm3R, 2), round(bic_legfm2R, 2), round(bic_legfmr, 2))))


stargazer(legfm3r,legfm2r, legfmr,  apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/DescRegMod.tex",
          add.lines = list(c("Country FE", "N", "N", "Y"),
                           c("EPG FE", "N", "Y", "Y"),
                           c("Year FE", "Y", "Y", "Y"),
                           c("Observations", n_legfm3r, n_legfm2r, n_legfmr),
                           c("AIC", round(aic_legfm3R, 2), round(aic_legfm2R, 2), round(aic_legfmr, 2)),
                           c("BIC", round(bic_legfm3R, 2), round(bic_legfm2R, 2), round(bic_legfmr, 2))))


## Party & PO Regression  ----------------------------------------------------------------------
#using lagged PO
ksm<- glm(p_yes ~ WS_INT*l_PO_EurosceptY + WS*l_IM_saliencePCT + WS_INT*PTY_EUROSC + WS*PTY_IMMIG_RESTR + country + as.factor(YEAR), data = LegScoreVotePtyData, family = quasibinomial(link = "logit"))
summary(ksm)
#with robust SE:
ksmr<-coeftest(ksm, vcov = vcovHC(ksm, type = "HC0"))
ksmr

n_ksmr <- nobs(ksm)


# for aic and bic:
ksm<- glm(p_yes ~ WS_INT*l_PO_EurosceptY + WS*l_IM_saliencePCT + WS_INT*PTY_EUROSC + WS*PTY_IMMIG_RESTR + country + as.factor(YEAR), data = LegScoreVotePtyData, family = binomial(link = "logit"))
aic_ksmr<- AIC(ksm)
bic_ksmr<- BIC(ksm)


### Figs 3 & 4 -------------

## INTERACTION PLOTS WITH MARGINS

medf <- LegScoreVotePtyData[!is.na(LegScoreVotePtyData$PTY_EUROSC) & !is.infinite(LegScoreVotePtyData$PTY_EUROSC), ]

marginal_effects <- margins(ksm, variables = "WS_INT", at = list(PTY_EUROSC = seq(min(medf$PTY_EUROSC), max(medf$PTY_EUROSC), length.out = 10)))
# Convert to a data frame
marginal_effects_df <- summary(marginal_effects)
# View the data frame
head(marginal_effects_df)

p1<-ggplot(marginal_effects_df, aes(x = PTY_EUROSC, y = AME)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = "Supranational Integration",
       subtitle = "Party Model",
       x = "Party Euroscepticism",
       y = "Marginal Effect of Dossier Pro-Sovereignty on % Yes Vote") +
  theme_minimal()
p1



medf <- LegScoreVotePtyData[!is.na(LegScoreVotePtyData$PTY_IMMIG_RESTR) & !is.infinite(LegScoreVotePtyData$PTY_IMMIG_RESTR), ]

marginal_effects <- margins(ksm, variables = "WS", at = list(PTY_IMMIG_RESTR = seq(min(medf$PTY_IMMIG_RESTR), max(medf$PTY_IMMIG_RESTR), length.out = 10)))
# Convert to a data frame
marginal_effects_df <- summary(marginal_effects)
# View the data frame
head(marginal_effects_df)

p2<-ggplot(marginal_effects_df, aes(x = PTY_IMMIG_RESTR, y = AME)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = "Immigration Regulation",
       subtitle = "Party Model",
       x = "Party Anti-Immigration",
       y = "Marginal Effect of Dossier Restrictiveness on % Yes Vote") +
  theme_minimal()
p2


DossierCongruence <- ggarrange(p1, p2,
                               ncol = 2, nrow = 1)
# Adding a title using cowplot
title <- ggdraw() +
  draw_text(" ", x = 0.5, hjust = 0.5, vjust = 1, size = 12, family='serif')
# Combine the title and ggarrange plots
DossierCongruence <- plot_grid(title, DossierCongruence, ncol = 1, rel_heights = c(0.1, 1))
DossierCongruence
ggsave("tables&figures/DossierCongruence.pdf", DossierCongruence)



## INTERACTION PLOTS WITH MARGINS - PO

medf <- LegScoreVotePtyData[!is.na(LegScoreVotePtyData$l_PO_EurosceptY) & !is.infinite(LegScoreVotePtyData$l_PO_EurosceptY), ]

marginal_effects <- margins(ksm, variables = "WS_INT", at = list(l_PO_EurosceptY = seq(min(medf$l_PO_EurosceptY), max(medf$l_PO_EurosceptY), length.out = 10)))
# Convert to a data frame
marginal_effects_df <- summary(marginal_effects)
# View the data frame
head(marginal_effects_df)

p3<-ggplot(marginal_effects_df, aes(x = l_PO_EurosceptY, y = AME)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = "Supranational Integration",
       subtitle = "Public Opinion Model",
       x = "Public Euroscepticism",
       y = "Marginal Effect of Dossier Pro-Sovereignty on % Yes Vote") +
  theme_minimal()
p3



medf <- LegScoreVotePtyData[!is.na(LegScoreVotePtyData$l_IM_saliencePCT) & !is.infinite(LegScoreVotePtyData$l_IM_saliencePCT), ]

marginal_effects <- margins(ksm, variables = "WS", at = list(l_IM_saliencePCT = seq(min(medf$l_IM_saliencePCT), max(medf$l_IM_saliencePCT), length.out = 10)))
# Convert to a data frame
marginal_effects_df <- summary(marginal_effects)
# View the data frame
head(marginal_effects_df)

p4<-ggplot(marginal_effects_df, aes(x = l_IM_saliencePCT , y = AME)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = "Immigration Regulation",
       subtitle = "Public Opinion Model",
       x = "Public Anti-Immigration Sentiment (Salience)",
       y = "Marginal Effect of Dossier Restrictiveness on % Yes Vote") +
  theme_minimal()
p4


DossierCongruencePO <- ggarrange(p3, p4,
                                 ncol = 2, nrow = 1)
# Adding a title using cowplot
title <- ggdraw() +
  draw_text(" ", x = 0.5, hjust = 0.5, vjust = 1, size = 12, family='serif')
# Combine the title and ggarrange plots
DossierCongruencePO <- plot_grid(title, DossierCongruencePO, ncol = 1, rel_heights = c(0.1, 1))
DossierCongruencePO
ggsave("tables&figures/DossierCongruencePO.pdf", DossierCongruencePO)


#using lagged PO
ksmt<- glm(p_yes ~ WS_INT*l_PO_EurosceptY*PTY_EUROSC + WS*l_IM_saliencePCT*PTY_IMMIG_RESTR + country + as.factor(YEAR), data = LegScoreVotePtyData, family = quasibinomial(link = "logit"))
summary(ksmt)
#with robust SE:
ksmtr<-coeftest(ksmt, vcov = vcovHC(ksmt, type = "HC0"))
ksmtr

n_ksmtr <- nobs(ksmt)


# for aic and bic:
ksmt<- glm(p_yes ~ WS_INT*l_PO_EurosceptY*PTY_EUROSC + WS*l_IM_saliencePCT*PTY_IMMIG_RESTR + country + as.factor(YEAR), data = LegScoreVotePtyData, family = binomial(link = "logit"))
aic_ksmtr<- AIC(ksmt)
bic_ksmtr<- BIC(ksmt)


stargazer(ksmr, ksmtr,  apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/All_Pty_PO_mods.htm",
          add.lines = list(c("Country FE", "Y", "Y"),
                           c("Year FE", "Y", "Y"),
                           c("Observations", n_ksmr, n_ksmtr),
                           c("AIC", round(aic_ksmr, 2), round(aic_ksmtr, 2)),
                           c("BIC", round(bic_ksmr, 2), round(bic_ksmtr, 2))))

stargazer(ksmr, ksmtr,  apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/All_Pty_PO_mods.tex",
          add.lines = list(c("Country FE", "Y", "Y"),
                           c("Year FE", "Y", "Y"),
                           c("Observations", n_ksmr, n_ksmtr),
                           c("AIC", round(aic_ksmr, 2), round(aic_ksmtr, 2)),
                           c("BIC", round(bic_ksmr, 2), round(bic_ksmtr, 2))))

## Cross-Pressure Reg  ----------------------------------------------------------------------

#The 'Quadrants'
LegScoreVotePtyData <- LegScoreVotePtyData %>% 
  mutate(Quad = factor(case_when(
    WS < 1 & WS_INT < 1 ~ "Pro-EU Liberal",
    WS >= 1 & WS_INT >= 1 ~ "Anti-EU Restrictive",
    WS >= 1 & WS_INT < 1 ~ "Pro-EU Restrictive",
    WS < 1 & WS_INT >= 1 ~ "Anti-EU Liberal"
  ), levels = c("Pro-EU Liberal", "Anti-EU Restrictive", "Pro-EU Restrictive", "Anti-EU Liberal")))


table(LegScoreVotePtyData$WS,LegScoreVotePtyData$Quad)
table(LegScoreVotePtyData$WS_INT,LegScoreVotePtyData$Quad)
table(LegScoreVotePtyData$Quad)


#datasets for each quadrant
ELdata <- subset(LegScoreVotePtyData, Quad == "Pro-EU Liberal")

ARdata <- subset(LegScoreVotePtyData, Quad == "Anti-EU Restrictive") 

ERdata <- subset(LegScoreVotePtyData, Quad == "Pro-EU Restrictive") #cross-pressure

ALdata <- subset(LegScoreVotePtyData, Quad == "Anti-EU Liberal") #cross-pressure


#Party Categories
ELdata <- ELdata %>% 
  mutate(
    PTY_TYPE = as.factor(case_when(
      PTY_EUROSC >=5 & PTY_IMMIG_RESTR >= 5 ~ "Anti-Immig. & Eurosceptic",
      PTY_EUROSC < 5 & PTY_IMMIG_RESTR >= 5 ~ "Anti-Immig. & Europhile",
      PTY_EUROSC >=5 & PTY_IMMIG_RESTR < 5 ~ "Pro-Immig. & Eurosceptic",
      PTY_EUROSC < 5 & PTY_IMMIG_RESTR < 5 ~ "Pro-Immig. & Europhile",
    )
    )
  )

table(ELdata$PTY_TYPE)

ARdata <- ARdata %>% 
  mutate(
    PTY_TYPE = as.factor(case_when(
      PTY_EUROSC >=5 & PTY_IMMIG_RESTR >= 5 ~ "Anti-Immig. & Eurosceptic",
      PTY_EUROSC < 5 & PTY_IMMIG_RESTR >= 5 ~ "Anti-Immig. & Europhile",
      PTY_EUROSC >= 5 & PTY_IMMIG_RESTR < 5 ~ "Pro-Immig. & Eurosceptic",
      PTY_EUROSC < 5 & PTY_IMMIG_RESTR < 5 ~ "Pro-Immig. & Europhile",
    )
    )
  )
table(ARdata$PTY_TYPE)


ERdata <- ERdata %>% 
  mutate(
    PTY_TYPE = as.factor(case_when(
      PTY_EUROSC >=5 & PTY_IMMIG_RESTR >= 5 ~ "Anti-Immig. & Eurosceptic",
      PTY_EUROSC < 5 & PTY_IMMIG_RESTR >= 5 ~ "Anti-Immig. & Europhile",
      PTY_EUROSC >= 5 & PTY_IMMIG_RESTR < 5 ~ "Pro-Immig. & Eurosceptic",
      PTY_EUROSC < 5 & PTY_IMMIG_RESTR < 5 ~ "Pro-Immig. & Europhile",
    )
    )
  )

table(ERdata$PTY_TYPE)



ALdata <- ALdata %>% 
  mutate(
    PTY_TYPE = as.factor(case_when(
      PTY_EUROSC >=5 & PTY_IMMIG_RESTR >= 5 ~ "Anti-Immig. & Eurosceptic",
      PTY_EUROSC < 5 & PTY_IMMIG_RESTR >= 5 ~ "Anti-Immig. & Europhile",
      PTY_EUROSC >= 5 & PTY_IMMIG_RESTR < 5 ~ "Pro-Immig. & Eurosceptic",
      PTY_EUROSC < 5 & PTY_IMMIG_RESTR < 5 ~ "Pro-Immig. & Europhile",
    )
    )
  )

table(ALdata$PTY_TYPE)


#cross-pressure datasets: identify legislation and parties at odds
#then check whether pty Eurosc or pty immig matters most



LegScoreVotePtyData <- LegScoreVotePtyData %>% 
  mutate(CrossP = factor(case_when(
    WS < 1 & WS_INT < 1 & PTY_EUROSC >=5 & PTY_IMMIG_RESTR < 5  ~ "Cross-Pressured (on EU)",
    WS < 1 & WS_INT < 1 & PTY_EUROSC <5 & PTY_IMMIG_RESTR >= 5  ~ "Cross-Pressured (on IM)",
    WS >= 1 & WS_INT >= 1 & PTY_EUROSC <5 & PTY_IMMIG_RESTR >= 5  ~ "Cross-Pressured (on EU)",
    WS >= 1 & WS_INT >= 1 & PTY_EUROSC >=5 & PTY_IMMIG_RESTR < 5  ~ "Cross-Pressured (on IM)",
    WS >= 1 & WS_INT < 1 & PTY_EUROSC >=5 & PTY_IMMIG_RESTR >= 5 ~ "Cross-Pressured (on EU)",
    WS >= 1 & WS_INT < 1 & PTY_EUROSC <5 & PTY_IMMIG_RESTR < 5 ~ "Cross-Pressured (on IM)",
    WS < 1 & WS_INT >= 1 & PTY_EUROSC <5 & PTY_IMMIG_RESTR < 5 ~ "Cross-Pressured (on EU)",
    WS < 1 & WS_INT >= 1 & PTY_EUROSC >=5 & PTY_IMMIG_RESTR >= 5 ~ "Cross-Pressured (on IM)",
    TRUE ~ "Either Fully Aligned or Misaligned"
  ), levels = c("Cross-Pressured (on EU)", "Cross-Pressured (on IM)", "Either Fully Aligned or Misaligned")))

table(LegScoreVotePtyData$CrossP, useNA = 'ifany')


#CrossPressure Data

CPdata <- subset(LegScoreVotePtyData, CrossP != "Either Fully Aligned or Misaligned")
CPdataIM <- subset(LegScoreVotePtyData, CrossP == "Cross-Pressured (on IM)")
CPdataEU <- subset(LegScoreVotePtyData, CrossP == "Cross-Pressured (on EU)")


##### Supranational Integration position effects
##### Pro-EU Liberal

cpmEL <- glm(p_yes ~ PTY_EUROSC + country + as.factor(YEAR), data = ELdata, family = quasibinomial(link = "logit"))
summary(cpmEL)

# Extract the number of observations
num_obs_cpmEL <- nobs(cpmEL)

#with robust SE:
cpmELr<-coeftest(cpmEL, vcov = vcovHC(cpmEL, type = "HC0"))
cpmELr


##### Anti-EU Liberal


cpmAL <- glm(p_yes ~  PTY_EUROSC + country + as.factor(YEAR), data = ALdata, family = quasibinomial(link = "logit"))
summary(cpmAL)

# Extract the number of observations
num_obs_cpmAL <- nobs(cpmAL)

#with robust SE:
cpmALr<-coeftest(cpmAL, vcov = vcovHC(cpmAL, type = "HC0"))
cpmALr


##### Anti-EU Restrictive


cpmAR <- glm(p_yes ~ PTY_EUROSC + country + as.factor(YEAR), data = ARdata, family = quasibinomial(link = "logit"))
summary(cpmAR)

# Extract the number of observations
num_obs_cpmAR <- nobs(cpmAR)

#with robust SE:
cpmARr<-coeftest(cpmAR, vcov = vcovHC(cpmAR, type = "HC0"))
cpmARr


##### Pro-EU Restrictive


cpmER <- glm(p_yes ~ PTY_EUROSC + country + as.factor(YEAR), data = ERdata, family = quasibinomial(link = "logit"))
summary(cpmER)

# Extract the number of observations
num_obs_cpmER <- nobs(cpmER)

#with robust SE:
cpmERr<-coeftest(cpmER, vcov = vcovHC(cpmER, type = "HC0"))
cpmERr



stargazer(cpmELr, cpmALr, cpmARr, cpmERr, 
          apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/DossTypeModsEU.htm",
          add.lines = list("N (cpmELr)" = num_obs_cpmEL, 
                           "N (cpmALr)" = num_obs_cpmAL,
                           "N (cpmARr)" = num_obs_cpmAR,
                           "N (cpmERr)" = num_obs_cpmER
          ))
stargazer(cpmELr, cpmALr, cpmARr, cpmERr, 
          apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/DossTypeModsEU.tex",
          add.lines = list("N (cpmELr)" = num_obs_cpmEL, 
                           "N (cpmALr)" = num_obs_cpmAL,
                           "N (cpmARr)" = num_obs_cpmAR,
                           "N (cpmERr)" = num_obs_cpmER
          ))

### COEFPLOT
# Extract coefficients and robust standard errors for each model
tidy_cpmEL <- tidy(cpmELr) %>% filter(term == "PTY_EUROSC")
tidy_cpmAL <- tidy(cpmALr) %>% filter(term == "PTY_EUROSC")
tidy_cpmAR <- tidy(cpmARr) %>% filter(term == "PTY_EUROSC")
tidy_cpmER <- tidy(cpmERr) %>% filter(term == "PTY_EUROSC")

# Create a combined data frame
coef_data <- data.frame(
  Model = c("Pro-EU Liberal", "Anti-EU Liberal", "Anti-EU Restrictive", "Pro-EU Restrictive"),
  Estimate = c(tidy_cpmEL$estimate, tidy_cpmAL$estimate, tidy_cpmAR$estimate, tidy_cpmER$estimate),
  Std.Error = c(tidy_cpmEL$std.error, tidy_cpmAL$std.error, tidy_cpmAR$std.error, tidy_cpmER$std.error)
)

# Calculate confidence intervals
coef_data$CI_Lower <- coef_data$Estimate - 1.96 * coef_data$Std.Error
coef_data$CI_Upper <- coef_data$Estimate + 1.96 * coef_data$Std.Error

P1c<-ggplot(coef_data, aes(x = Estimate, y = Model)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
  labs(title = "Euroscepticism Effect by Dossier Type",
       subtitle = "",
       x = "Beta Coefficient Estimate of Party Euroscepticism",
       y = "") +
  theme_minimal(base_size = 10) +  # Set a base size for scaling
  theme(
    title = element_text(size = rel(1.5)),
    axis.title.x = element_text(size = rel(0.8)),  
    axis.text.x = element_text(size = rel(1.5)),   # Tick labels
    axis.text.y = element_text(size = rel(1.5))    # Tick labels
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")  

P1c

ggsave("tables&figures/CoefPlotEU.pdf", P1c)

##### Immigration position effects
##### Pro-EU Liberal

cpmEL <- glm(p_yes ~ PTY_IMMIG_RESTR + country +  as.factor(YEAR), data = ELdata, family = quasibinomial(link = "logit"))
summary(cpmEL)

# Extract the number of observations
num_obs_cpmEL <- nobs(cpmEL)

#with robust SE:
cpmELr<-coeftest(cpmEL, vcov = vcovHC(cpmEL, type = "HC0"))
cpmELr


##### Anti-EU Liberal


cpmAL <- glm(p_yes ~ PTY_IMMIG_RESTR + country + as.factor(YEAR), data = ALdata, family = quasibinomial(link = "logit"))
summary(cpmAL)

# Extract the number of observations
num_obs_cpmAL <- nobs(cpmAL)

#with robust SE:
cpmALr<-coeftest(cpmAL, vcov = vcovHC(cpmAL, type = "HC0"))
cpmALr


##### Anti-EU Restrictive


cpmAR <- glm(p_yes ~ PTY_IMMIG_RESTR + country + as.factor(YEAR), data = ARdata, family = quasibinomial(link = "logit"))
summary(cpmAR)

# Extract the number of observations
num_obs_cpmAR <- nobs(cpmAR)

#with robust SE:
cpmARr<-coeftest(cpmAR, vcov = vcovHC(cpmAR, type = "HC0"))
cpmARr


##### Pro-EU Restrictive


cpmER <- glm(p_yes ~ PTY_IMMIG_RESTR + country + as.factor(YEAR), data = ERdata, family = quasibinomial(link = "logit"))
summary(cpmER)

# Extract the number of observations
num_obs_cpmER <- nobs(cpmER)

#with robust SE:
cpmERr<-coeftest(cpmER, vcov = vcovHC(cpmER, type = "HC0"))
cpmERr


stargazer(cpmELr, cpmALr, cpmARr, cpmERr, 
          apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/DossTypeModsIM.htm",
          add.lines = list("N (cpmELr)" = num_obs_cpmEL, 
                           "N (cpmALr)" = num_obs_cpmAL,
                           "N (cpmARr)" = num_obs_cpmAR,
                           "N (cpmERr)" = num_obs_cpmER
          ))
stargazer(cpmELr, cpmALr, cpmARr, cpmERr, 
          apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/DossTypeModsIM.tex",
          add.lines = list("N (cpmELr)" = num_obs_cpmEL, 
                           "N (cpmALr)" = num_obs_cpmAL,
                           "N (cpmARr)" = num_obs_cpmAR,
                           "N (cpmERr)" = num_obs_cpmER
          ))

### COEFPLOT
# Extract coefficients and robust standard errors for each model
tidy_cpmEL <- tidy(cpmELr) %>% filter(term == "PTY_IMMIG_RESTR")
tidy_cpmAL <- tidy(cpmALr) %>% filter(term == "PTY_IMMIG_RESTR")
tidy_cpmAR <- tidy(cpmARr) %>% filter(term == "PTY_IMMIG_RESTR")
tidy_cpmER <- tidy(cpmERr) %>% filter(term == "PTY_IMMIG_RESTR")

# Create a combined data frame
coef_data <- data.frame(
  Model = c("Pro-EU Liberal", "Anti-EU Liberal", "Anti-EU Restrictive", "Pro-EU Restrictive"),
  Estimate = c(tidy_cpmEL$estimate, tidy_cpmAL$estimate, tidy_cpmAR$estimate, tidy_cpmER$estimate),
  Std.Error = c(tidy_cpmEL$std.error, tidy_cpmAL$std.error, tidy_cpmAR$std.error, tidy_cpmER$std.error)
)

# Calculate confidence intervals
coef_data$CI_Lower <- coef_data$Estimate - 1.96 * coef_data$Std.Error
coef_data$CI_Upper <- coef_data$Estimate + 1.96 * coef_data$Std.Error

P2c<-ggplot(coef_data, aes(x = Estimate, y = Model)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
  labs(title = "Anti-Immigration Effect by Dossier Type",
       subtitle = "",
       x = "Beta Coefficient Estimate of Party Anti-Immigration",
       y = "") +
  theme_minimal(base_size = 10) +  # Set a base size for scaling
  theme(
    title = element_text(size = rel(1.5)),
    axis.title.x = element_text(size = rel(0.8)),  
    axis.text.x = element_text(size = rel(1.5)),   # Tick labels
    axis.text.y = element_text(size = rel(1.5))    # Tick labels
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")  
P2c



ggsave("tables&figures/CoefPlotIM.pdf", P2c)

# Robustness ---------------------------------------------------------------------
PO_ess<-read.csv("ESS_preference.csv")

## Immigration salience vs. position ---------------------------------------------
EB41.1data <- foreign::read.spss("ZA2491_v1-1-0.sav",
                                 use.value.labels = FALSE,
                                 to.data.frame = TRUE,
                                 max.value.labels = Inf,
                                 trim.factor.names = FALSE,
                                 trim_values = FALSE,
                                 reencode = NA)

# Weight for EU-wide (EU 12 at this time) analyses is v14
# For nation-specific analyses it's v8

data.labels <- as.data.frame(attr(EB41.1data, "variable.labels")) #search for Q58 and Q47

table(EB41.1data$v210, useNA  = 'ifany') #recode so that higher numbers indicate more restrictiveness: in the original datafile 1 is 'extended', 2 is 'restricted' and 3 is 'as they are'. 

EB41.1data <- EB41.1data %>%
  mutate(immig_restr = case_when(
    v210 == 1 ~ 1,  # extended stays the same
    v210 == 2 ~ 3,  # restricted becomes 3 (more restrictive)
    v210 == 3 ~ 2,  # as they are becomes 2
    TRUE ~ NA_real_
  ))

table(EB41.1data$v210,EB41.1data$immig_restr, useNA  = 'ifany')

table(EB41.1data$v173, useNA  = 'ifany')
table(EB41.1data$v174, useNA  = 'ifany')
table(EB41.1data$v175, useNA  = 'ifany')

EB41.1data <- EB41.1data %>%
  mutate(immig_sal = case_when(
    v175 == 6 ~ 1,
    v174 == 6 ~ 2,
    v173 == 6 ~ 3,
    TRUE ~ 0,
    v175 == NA_real_ & v174 == NA_real_ & v175 == NA_real_ ~ NA_real_
    
  ))

table(EB41.1data$v173, EB41.1data$immig_sal, useNA  = 'ifany')
table(EB41.1data$v174, EB41.1data$immig_sal, useNA  = 'ifany')
table(EB41.1data$v175, EB41.1data$immig_sal, useNA  = 'ifany')

# USING CONTINUOUS MEASURE

# Create line plot with jittered dots

p <- ggplot(EB41.1data, aes(x = immig_sal, y = immig_restr, weight = v14)) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "grey76", size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, color = "navy", size = 1) +
  scale_x_continuous(breaks = 0:3) +  
  labs(
    title = "Immigration Salience and Position",
    subtitle = "EB 41.1",
    x = "Immigration Salience",
    y = "Immigration Restriction"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = rel(1.8), face = "bold"),
    plot.subtitle = element_text(size = rel(1.6), face = "bold"),
    axis.title = element_text(size = rel(1.5)),
    axis.text = element_text(size = rel(1))
  )

p

# Save as PDF
ggsave("tables&figures/immigration_salience_plot.pdf", plot = p, width = 10, height = 10)


# regression
mod<-lm(immig_restr ~ immig_sal, weights = v14, data = EB41.1data)

# # mean diff hypothesis test
# mod<-lm(immig_restr ~ immig_sal_dummy, weights = v14, data = EB41.1data)

stargazer(mod,
          type = "latex",
          title = "Linear Regression: Immigration Restriction by Salience (Dummy)",
          label = "tab:immig_salience_lm",
          style = "default",
          out = "tables&figures/immig_salience_lm.tex")

stargazer(mod,
          type = "html",
          title = "Linear Regression: Immigration Restriction by Salience (Dummy)",
          out = "tables&figures/immig_salience_lm.html")


modFE<-lm(immig_restr ~ immig_sal + isocntry, weights = v14, data = EB41.1data)
summary(modFE)

# by country

p <- ggplot(EB41.1data, aes(x = immig_sal, y = immig_restr, weight = v8)) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "grey76", size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, color = "navy", size = 1) +
  scale_x_continuous(breaks = c(0, 1, 2,3)) +
  labs(
    title = "Immigration Salience and Position by Country",
    subtitle = "EB 41.1",
    x = "Immigration Salience",
    y = "Immigration Restriction"
  ) +
  facet_wrap(~ isocntry) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = rel(1.8), face = "bold"),
    plot.subtitle = element_text(size = rel(1.6), face = "bold"),
    axis.title = element_text(size = rel(1.5)),
    axis.text = element_text(size = rel(1))
  )

p
# Save as PDF
ggsave("tables&figures/immigration_salience_plot_bycntry.pdf", plot = p, width = 10, height = 10)

# using ESS data for lagged anti-immig PO
LegScoreVotePtyData2 <- left_join(x = LegScoreVotePtyData,y = PO_ess, by = c('YEAR','country'))

collapsed_data <- LegScoreVotePtyData2 %>%
  group_by(country, year) %>%
  summarise(
    l_IM_saliencePCT = mean(l_IM_saliencePCT, na.rm = TRUE),
    Wgt_mean_anti_immig_pos = mean(Wgt_mean_anti_immig_pos, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Pivot to long format for plotting
plot_data <- collapsed_data %>%
  pivot_longer(cols = c(l_IM_saliencePCT, Wgt_mean_anti_immig_pos),
               names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable,
                           "l_IM_saliencePCT" = "Salience",
                           "Wgt_mean_anti_immig_pos" = "Position"))

plot_data_norm <- plot_data %>%
  group_by(country, variable) %>%
  mutate(
    min_val = min(value, na.rm = TRUE),
    max_val = max(value, na.rm = TRUE),
    range = max_val - min_val,
    value_scaled = if_else(
      is.na(range) | range == 0,   # if range is NA or zero
      0,                           # assign 0 scaled value
      (value - min_val) / range
    )
  ) %>%
  dplyr::select(-min_val, -max_val, -range) %>%
  ungroup() %>%
  filter(!country =="")

# Plot
p<-ggplot(plot_data_norm, aes(x = year, y = value_scaled, color = variable, linetype = variable)) +
  geom_line(aes(group = variable), size = 1) +
  scale_linetype_manual(values = c(
    "Salience" = "dotted",
    "Position" = "solid"
  )) +
  scale_color_manual(values = c(
    "Salience" = "gray36",
    "Position" = "black"
  )) +
  facet_wrap(~country, scales = "free_y", ncol = 4) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = rel(0.65)),
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 0.8, size=rel(0.7)),
    axis.text.y = element_text(size=rel(0.7))
  )
p
# Save as PDF
ggsave("tables&figures/immigration_salience_ESSplot_bycntry.pdf", plot = p, width = 10, height = 10)


## Formal Robustness tests -------------------------------------------------------



ksmess<- glm(p_yes ~ WS_INT*l_PO_EurosceptY + WS*Wgt_mean_anti_immig_pos + WS_INT*PTY_EUROSC + WS*PTY_IMMIG_RESTR + country + as.factor(YEAR), data = LegScoreVotePtyData2, family = quasibinomial(link = "logit"))
summary(ksmess)
#with robust SE:
ksmessr<-coeftest(ksmess, vcov = vcovHC(ksmess, type = "HC0"))
ksmessr

n_ksmessr <- nobs(ksmess)


# for aic and bic:
ksmess<- glm(p_yes ~ WS_INT*l_PO_EurosceptY + WS*Wgt_mean_anti_immig_pos + WS_INT*PTY_EUROSC + WS*PTY_IMMIG_RESTR + country + as.factor(YEAR), data = LegScoreVotePtyData2, family = binomial(link = "logit"))
aic_ksmessr<- AIC(ksmess)
bic_ksmessr<- BIC(ksmess)


#### TRIPLE INTERACTION
#using lagged PO
ksmtess<- glm(p_yes ~ WS_INT*l_PO_EurosceptY*PTY_EUROSC + WS*Wgt_mean_anti_immig_pos*PTY_IMMIG_RESTR + country + as.factor(YEAR), data = LegScoreVotePtyData2, family = quasibinomial(link = "logit"))
summary(ksmtess)
#with robust SE:
ksmtessr<-coeftest(ksmtess, vcov = vcovHC(ksmtess, type = "HC0"))
ksmtessr

n_ksmtessr <- nobs(ksmtess)


# for aic and bic:
ksmtess<- glm(p_yes ~ WS_INT*l_PO_EurosceptY*PTY_EUROSC + WS*Wgt_mean_anti_immig_pos*PTY_IMMIG_RESTR + country + as.factor(YEAR), data = LegScoreVotePtyData2, family = binomial(link = "logit"))
aic_ksmtessr<- AIC(ksmtess)
bic_ksmtessr<- BIC(ksmtess)


stargazer(ksmessr, ksmtessr,  apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/All_Pty_PO_mods_ESS.htm",
          add.lines = list(c("Country FE", "Y", "Y"),
                           c("Year FE", "Y", "Y"),
                           c("Observations", n_ksmessr, n_ksmtessr),
                           c("AIC", round(aic_ksmessr, 2), round(aic_ksmtessr, 2)),
                           c("BIC", round(bic_ksmessr, 2), round(bic_ksmtessr, 2))))

stargazer(ksmessr, ksmtessr,  apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/All_Pty_PO_mods_ESS.tex",
          add.lines = list(c("Country FE", "Y", "Y"),
                           c("Year FE", "Y", "Y"),
                           c("Observations", n_ksmessr, n_ksmtessr),
                           c("AIC", round(aic_ksmessr, 2), round(aic_ksmtessr, 2)),
                           c("BIC", round(bic_ksmessr, 2), round(bic_ksmtessr, 2))))


# No country FE
#Main
ksmnoc<- glm(p_yes ~ WS_INT*l_PO_EurosceptY + WS*l_IM_saliencePCT + WS_INT*PTY_EUROSC + WS*PTY_IMMIG_RESTR + as.factor(YEAR), data = LegScoreVotePtyData, family = quasibinomial(link = "logit"))
summary(ksmnoc)
#with robust SE:
ksmnocr<-coeftest(ksmnoc, vcov = vcovHC(ksmnoc, type = "HC0"))
ksmnocr
n_ksmnocr <- nobs(ksmnocr)


# for aic and bic:
ksmnoc<- glm(p_yes ~ WS_INT*l_PO_EurosceptY + WS*l_IM_saliencePCT + WS_INT*PTY_EUROSC + WS*PTY_IMMIG_RESTR + as.factor(YEAR), data = LegScoreVotePtyData, family = binomial(link = "logit"))

aic_ksmnocr<- AIC(ksmnoc)
bic_ksmnocr<- BIC(ksmnoc)


#Triple
ksmtnoc<- glm(p_yes ~ WS_INT*l_PO_EurosceptY*PTY_EUROSC + WS*l_IM_saliencePCT*PTY_IMMIG_RESTR  + as.factor(YEAR), data = LegScoreVotePtyData, family = quasibinomial(link = "logit"))
summary(ksmtnoc)
#with robust SE:
ksmtnocr<-coeftest(ksmtnoc, vcov = vcovHC(ksmtnoc, type = "HC0"))
ksmtnocr

n_ksmtnocr <- nobs(ksmtnoc)


# for aic and bic:
ksmtnoc<- glm(p_yes ~ WS_INT*l_PO_EurosceptY*PTY_EUROSC + WS*l_IM_saliencePCT*PTY_IMMIG_RESTR + as.factor(YEAR), data = LegScoreVotePtyData, family = binomial(link = "logit"))
aic_ksmtnocr<- AIC(ksmtnoc)
bic_ksmtnocr<- BIC(ksmtnoc)

stargazer(ksmnocr, ksmtnocr,  apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/All_Pty_PO_mods_nocfe.htm",
          add.lines = list(c("Country FE", "N", "N"),
                           c("Year FE", "Y", "Y"),
                           c("Observations", n_ksmnocr, n_ksmtnocr),
                           c("AIC", round(aic_ksmnocr, 2), round(aic_ksmtnocr, 2)),
                           c("BIC", round(bic_ksmnocr, 2), round(bic_ksmtnocr, 2))))
stargazer(ksmnocr, ksmtnocr,  apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/All_Pty_PO_mods_nocfe.tex",
          add.lines = list(c("Country FE", "N", "N"),
                           c("Year FE", "Y", "Y"),
                           c("Observations", n_ksmnocr, n_ksmtnocr),
                           c("AIC", round(aic_ksmnocr, 2), round(aic_ksmtnocr, 2)),
                           c("BIC", round(bic_ksmnocr, 2), round(bic_ksmtnocr, 2))))

# Contemporaneous PO
#Main
ksmcont<- glm(p_yes ~ WS_INT*PO_EurosceptY + WS*IM_saliencePCT + WS_INT*PTY_EUROSC + WS*PTY_IMMIG_RESTR + country + as.factor(YEAR), data = LegScoreVotePtyData, family = quasibinomial(link = "logit"))
summary(ksmcont)
#with robust SE:
ksmcontr<-coeftest(ksmcont, vcov = vcovHC(ksmcont, type = "HC0"))
ksmcontr
n_ksmcontr <- nobs(ksmcont)


# for aic and bic:
ksmcont <- glm(p_yes ~ WS_INT*PO_EurosceptY + WS*IM_saliencePCT + WS_INT*PTY_EUROSC + WS*PTY_IMMIG_RESTR + country + as.factor(YEAR), data = LegScoreVotePtyData, family = binomial(link = "logit"))

aic_ksmcontr<- AIC(ksmcont)
bic_ksmcontr<- BIC(ksmcont)


#Triple
ksmtcont<- glm(p_yes ~ WS_INT*PO_EurosceptY*PTY_EUROSC + WS*IM_saliencePCT*PTY_IMMIG_RESTR  + country + as.factor(YEAR), data = LegScoreVotePtyData, family = quasibinomial(link = "logit"))
summary(ksmtcont)
#with robust SE:
ksmtcontr<-coeftest(ksmtcont, vcov = vcovHC(ksmtcont, type = "HC0"))
ksmtcontr

n_ksmtcontr <- nobs(ksmtcont)


# for aic and bic:
ksmtcont<- glm(p_yes ~ WS_INT*PO_EurosceptY*PTY_EUROSC + WS*IM_saliencePCT*PTY_IMMIG_RESTR  + country + as.factor(YEAR), data = LegScoreVotePtyData, family = binomial(link = "logit"))
aic_ksmtcontr<- AIC(ksmtcont)
bic_ksmtcontr<- BIC(ksmtcont)

stargazer(ksmcontr, ksmtcontr,  apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/All_Pty_PO_mods_contempPO.htm",
          add.lines = list(c("Country FE", "Y", "Y"),
                           c("Year FE", "Y", "Y"),
                           c("Observations", n_ksmcontr, n_ksmtcontr),
                           c("AIC", round(aic_ksmcontr, 2), round(aic_ksmtcontr, 2)),
                           c("BIC", round(bic_ksmcontr, 2), round(bic_ksmtcontr, 2))))
stargazer(ksmcontr, ksmtcontr,  apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/All_Pty_PO_mods_contempPO.tex",
          add.lines = list(c("Country FE", "Y", "Y"),
                           c("Year FE", "Y", "Y"),
                           c("Observations", n_ksmcontr, n_ksmtcontr),
                           c("AIC", round(aic_ksmcontr, 2), round(aic_ksmtcontr, 2)),
                           c("BIC", round(bic_ksmcontr, 2), round(bic_ksmtcontr, 2))))


# Remove uncompetitive votes
df<- subset(LegScoreVotePtyData, extremely_uncompetitive_vote == 0)

#Main
ksmcomp<- glm(p_yes ~ WS_INT*l_PO_EurosceptY + WS*l_IM_saliencePCT + WS_INT*PTY_EUROSC + WS*PTY_IMMIG_RESTR + country + as.factor(YEAR), data = df, family = quasibinomial(link = "logit"))
summary(ksmcomp)
#with robust SE:
ksmcompr<-coeftest(ksmcomp, vcov = vcovHC(ksmcomp, type = "HC0"))
ksmcompr
n_ksmcompr <- nobs(ksmcompr)


# for aic and bic:
ksmcomp<- glm(p_yes ~ WS_INT*l_PO_EurosceptY + WS*l_IM_saliencePCT + WS_INT*PTY_EUROSC + WS*PTY_IMMIG_RESTR + country + as.factor(YEAR), data = df, family = binomial(link = "logit"))

aic_ksmcompr<- AIC(ksmcomp)
bic_ksmcompr<- BIC(ksmcomp)


#Triple
ksmtcomp<- glm(p_yes ~ WS_INT*l_PO_EurosceptY*PTY_EUROSC + WS*l_IM_saliencePCT*PTY_IMMIG_RESTR  + country + as.factor(YEAR), data = df, family = quasibinomial(link = "logit"))
summary(ksmtcomp)
#with robust SE:
ksmtcompr<-coeftest(ksmtcomp, vcov = vcovHC(ksmtcomp, type = "HC0"))
ksmtcompr

n_ksmtcompr <- nobs(ksmtcomp)


# for aic and bic:
ksmtcomp<- glm(p_yes ~ WS_INT*l_PO_EurosceptY*PTY_EUROSC + WS*l_IM_saliencePCT*PTY_IMMIG_RESTR + country + as.factor(YEAR), data = df, family = binomial(link = "logit"))
aic_ksmtcompr<- AIC(ksmtcomp)
bic_ksmtcompr<- BIC(ksmtcomp)

stargazer(ksmcompr, ksmtcompr,  apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/All_Pty_PO_mods_onlycompetitive.htm",
          add.lines = list(c("Country FE", "Y", "Y"),
                           c("Year FE", "Y", "Y"),
                           c("Observations", n_ksmcompr, n_ksmtcompr),
                           c("AIC", round(aic_ksmcompr, 2), round(aic_ksmtcompr, 2)),
                           c("BIC", round(bic_ksmcompr, 2), round(bic_ksmtcompr, 2))))


#CREATE FUNCTION FOR ROBUSTNESS TESTS
run_models_and_export <- function(data, output_file) {
  
  # --- MAIN MODEL ---
  mod1 <- glm(p_yes ~ WS_INT * l_PO_EurosceptY +
                WS * l_IM_saliencePCT +
                WS_INT * PTY_EUROSC +
                WS * PTY_IMMIG_RESTR +
                country + as.factor(YEAR),
              data = df, family = quasibinomial(link = "logit"))
  
  mod1r <- coeftest(mod1, vcov = vcovHC(mod1, type = "HC0"))
  n_mod1 <- nobs(mod1)
  
  mod1_bic_aic <- glm(p_yes ~ WS_INT * l_PO_EurosceptY +
                        WS * l_IM_saliencePCT +
                        WS_INT * PTY_EUROSC +
                        WS * PTY_IMMIG_RESTR +
                        country + as.factor(YEAR),
                      data = df, family = binomial(link = "logit"))
  
  aic_mod1 <- AIC(mod1_bic_aic)
  bic_mod1 <- BIC(mod1_bic_aic)
  
  # --- TRIPLE INTERACTION MODEL ---
  mod2 <- glm(p_yes ~ WS_INT * l_PO_EurosceptY * PTY_EUROSC +
                WS * l_IM_saliencePCT * PTY_IMMIG_RESTR +
                country + as.factor(YEAR),
              data = df, family = quasibinomial(link = "logit"))
  
  mod2r <- coeftest(mod2, vcov = vcovHC(mod2, type = "HC0"))
  n_mod2 <- nobs(mod2)
  
  mod2_bic_aic <- glm(p_yes ~ WS_INT * l_PO_EurosceptY * PTY_EUROSC +
                        WS * l_IM_saliencePCT * PTY_IMMIG_RESTR +
                        country + as.factor(YEAR),
                      data = df, family = binomial(link = "logit"))
  
  aic_mod2 <- AIC(mod2_bic_aic)
  bic_mod2 <- BIC(mod2_bic_aic)
  
  # --- EXPORT TABLE ---
  
  stargazer(mod1r, mod2r, 
            apply.coef = exp, 
            t.auto = FALSE, 
            p.auto = FALSE, 
            report = "vc*s",
            out = output_file,
            add.lines = list(
              c("Country FE", "Y", "Y"),
              c("Year FE", "Y", "Y"),
              c("Observations", n_mod1, n_mod2),
              c("AIC", round(aic_mod1, 2), round(aic_mod2, 2)),
              c("BIC", round(bic_mod1, 2), round(bic_mod2, 2))
            )
  )
  
  message("Model run complete. Output saved to: ", output_file)
}


#COMPETITIVE VOTES
df<- subset(LegScoreVotePtyData, extremely_uncompetitive_vote == 0)
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_onlycompetitive.htm")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_onlycompetitive.tex")

#ONLY CO-DECISION
df<- subset(LegScoreVotePtyData, ProcedureType.x == "Co-Decision")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_onlycodec.htm")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_onlycodec.tex")

#BY TOPIC
df<- subset(LegScoreVotePtyData, topic == "Labour Migr.")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_onlylabmigr.htm")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_onlylabmigr.tex")

df<- subset(LegScoreVotePtyData, topic == "Human Rights")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_onlyhr.htm")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_onlyhr.tex")

df<- subset(LegScoreVotePtyData, topic == "Visas")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_onlyvisa.htm")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_onlyvisa.tex")

df<- subset(LegScoreVotePtyData, topic == "Illegal Migr.")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_illegalmigr.htm")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_illegalmigr.tex")

df<- subset(LegScoreVotePtyData, topic == "Borders")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_onlyborders.htm")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_onlyborders.tex")

df<- subset(LegScoreVotePtyData, topic == "Labour Migr." | topic == "Human Rights")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_onlyhrandlabour.htm")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_onlyhrandlabour.tex")

df<- subset(LegScoreVotePtyData, topic == "Labour Migr." | topic == "Human Rights")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_onlyhrandlabour.htm")
run_models_and_export(df, "tables&figures/All_Pty_PO_mods_onlyhrandlabour.tex")


#The 'Quadrants'
LegScoreVotePtyData <- LegScoreVotePtyData %>% 
  mutate(Quad = factor(case_when(
    WS < 1 & WS_INT < 1 ~ "Pro-EU Liberal",
    WS >= 1 & WS_INT >= 1 ~ "Anti-EU Restrictive",
    WS >= 1 & WS_INT < 1 ~ "Pro-EU Restrictive",
    WS < 1 & WS_INT >= 1 ~ "Anti-EU Liberal"
  ), levels = c("Pro-EU Liberal", "Anti-EU Restrictive", "Pro-EU Restrictive", "Anti-EU Liberal")))


table(LegScoreVotePtyData$WS,LegScoreVotePtyData$Quad)
table(LegScoreVotePtyData$WS_INT,LegScoreVotePtyData$Quad)
table(LegScoreVotePtyData$Quad)


#datasets for each quadrant
ELdata <- subset(LegScoreVotePtyData, Quad == "Pro-EU Liberal")

ARdata <- subset(LegScoreVotePtyData, Quad == "Anti-EU Restrictive") 

ERdata <- subset(LegScoreVotePtyData, Quad == "Pro-EU Restrictive") #cross-pressure

ALdata <- subset(LegScoreVotePtyData, Quad == "Anti-EU Liberal") #cross-pressure


#Party Categories
ELdata <- ELdata %>% 
  mutate(
    PTY_TYPE = as.factor(case_when(
      PTY_EUROSC >=5 & PTY_IMMIG_RESTR >= 5 ~ "Anti-Immig. & Eurosceptic",
      PTY_EUROSC < 5 & PTY_IMMIG_RESTR >= 5 ~ "Anti-Immig. & Europhile",
      PTY_EUROSC >=5 & PTY_IMMIG_RESTR < 5 ~ "Pro-Immig. & Eurosceptic",
      PTY_EUROSC < 5 & PTY_IMMIG_RESTR < 5 ~ "Pro-Immig. & Europhile",
    )
    )
  )

table(ELdata$PTY_TYPE)

ARdata <- ARdata %>% 
  mutate(
    PTY_TYPE = as.factor(case_when(
      PTY_EUROSC >=5 & PTY_IMMIG_RESTR >= 5 ~ "Anti-Immig. & Eurosceptic",
      PTY_EUROSC < 5 & PTY_IMMIG_RESTR >= 5 ~ "Anti-Immig. & Europhile",
      PTY_EUROSC >= 5 & PTY_IMMIG_RESTR < 5 ~ "Pro-Immig. & Eurosceptic",
      PTY_EUROSC < 5 & PTY_IMMIG_RESTR < 5 ~ "Pro-Immig. & Europhile",
    )
    )
  )
table(ARdata$PTY_TYPE)


ERdata <- ERdata %>% 
  mutate(
    PTY_TYPE = as.factor(case_when(
      PTY_EUROSC >=5 & PTY_IMMIG_RESTR >= 5 ~ "Anti-Immig. & Eurosceptic",
      PTY_EUROSC < 5 & PTY_IMMIG_RESTR >= 5 ~ "Anti-Immig. & Europhile",
      PTY_EUROSC >= 5 & PTY_IMMIG_RESTR < 5 ~ "Pro-Immig. & Eurosceptic",
      PTY_EUROSC < 5 & PTY_IMMIG_RESTR < 5 ~ "Pro-Immig. & Europhile",
    )
    )
  )

table(ERdata$PTY_TYPE)



ALdata <- ALdata %>% 
  mutate(
    PTY_TYPE = as.factor(case_when(
      PTY_EUROSC >=5 & PTY_IMMIG_RESTR >= 5 ~ "Anti-Immig. & Eurosceptic",
      PTY_EUROSC < 5 & PTY_IMMIG_RESTR >= 5 ~ "Anti-Immig. & Europhile",
      PTY_EUROSC >= 5 & PTY_IMMIG_RESTR < 5 ~ "Pro-Immig. & Eurosceptic",
      PTY_EUROSC < 5 & PTY_IMMIG_RESTR < 5 ~ "Pro-Immig. & Europhile",
    )
    )
  )

table(ALdata$PTY_TYPE)


#cross-pressure datasets: identify legislation and parties at odds
#then check whether pty Eurosc or pty immig matters most



LegScoreVotePtyData <- LegScoreVotePtyData %>% 
  mutate(CrossP = factor(case_when(
    WS < 1 & WS_INT < 1 & PTY_EUROSC >=5 & PTY_IMMIG_RESTR < 5  ~ "Cross-Pressured (on EU)",
    WS < 1 & WS_INT < 1 & PTY_EUROSC <5 & PTY_IMMIG_RESTR >= 5  ~ "Cross-Pressured (on IM)",
    WS >= 1 & WS_INT >= 1 & PTY_EUROSC <5 & PTY_IMMIG_RESTR >= 5  ~ "Cross-Pressured (on EU)",
    WS >= 1 & WS_INT >= 1 & PTY_EUROSC >=5 & PTY_IMMIG_RESTR < 5  ~ "Cross-Pressured (on IM)",
    WS >= 1 & WS_INT < 1 & PTY_EUROSC >=5 & PTY_IMMIG_RESTR >= 5 ~ "Cross-Pressured (on EU)",
    WS >= 1 & WS_INT < 1 & PTY_EUROSC <5 & PTY_IMMIG_RESTR < 5 ~ "Cross-Pressured (on IM)",
    WS < 1 & WS_INT >= 1 & PTY_EUROSC <5 & PTY_IMMIG_RESTR < 5 ~ "Cross-Pressured (on EU)",
    WS < 1 & WS_INT >= 1 & PTY_EUROSC >=5 & PTY_IMMIG_RESTR >= 5 ~ "Cross-Pressured (on IM)",
    TRUE ~ "Either Fully Aligned or Misaligned"
  ), levels = c("Cross-Pressured (on EU)", "Cross-Pressured (on IM)", "Either Fully Aligned or Misaligned")))

table(LegScoreVotePtyData$CrossP, useNA = 'ifany')


#CrossPressure Data

CPdata <- subset(LegScoreVotePtyData, CrossP != "Either Fully Aligned or Misaligned")
CPdataIM <- subset(LegScoreVotePtyData, CrossP == "Cross-Pressured (on IM)")
CPdataEU <- subset(LegScoreVotePtyData, CrossP == "Cross-Pressured (on EU)")

##### Supranational Integration position effects
##### Pro-EU Liberal

cpmEL <- glm(p_yes ~ PTY_EUROSC + country + as.factor(YEAR), data = ELdata, family = quasibinomial(link = "logit"))
summary(cpmEL)

# Extract the number of observations
num_obs_cpmEL <- nobs(cpmEL)

#with robust SE:
cpmELr<-coeftest(cpmEL, vcov = vcovHC(cpmEL, type = "HC0"))
cpmELr


##### Anti-EU Liberal


cpmAL <- glm(p_yes ~  PTY_EUROSC + country + as.factor(YEAR), data = ALdata, family = quasibinomial(link = "logit"))
summary(cpmAL)

# Extract the number of observations
num_obs_cpmAL <- nobs(cpmAL)

#with robust SE:
cpmALr<-coeftest(cpmAL, vcov = vcovHC(cpmAL, type = "HC0"))
cpmALr


##### Anti-EU Restrictive


cpmAR <- glm(p_yes ~ PTY_EUROSC + country + as.factor(YEAR), data = ARdata, family = quasibinomial(link = "logit"))
summary(cpmAR)

# Extract the number of observations
num_obs_cpmAR <- nobs(cpmAR)

#with robust SE:
cpmARr<-coeftest(cpmAR, vcov = vcovHC(cpmAR, type = "HC0"))
cpmARr


##### Pro-EU Restrictive


cpmER <- glm(p_yes ~ PTY_EUROSC + country + as.factor(YEAR), data = ERdata, family = quasibinomial(link = "logit"))
summary(cpmER)

# Extract the number of observations
num_obs_cpmER <- nobs(cpmER)

#with robust SE:
cpmERr<-coeftest(cpmER, vcov = vcovHC(cpmER, type = "HC0"))
cpmERr



stargazer(cpmELr, cpmALr, cpmARr, cpmERr, 
          apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/DossTypeModsEU.htm",
          add.lines = list("N (cpmELr)" = num_obs_cpmEL, 
                           "N (cpmALr)" = num_obs_cpmAL,
                           "N (cpmARr)" = num_obs_cpmAR,
                           "N (cpmERr)" = num_obs_cpmER
          ))
stargazer(cpmELr, cpmALr, cpmARr, cpmERr, 
          apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/DossTypeModsEU.tex",
          add.lines = list("N (cpmELr)" = num_obs_cpmEL, 
                           "N (cpmALr)" = num_obs_cpmAL,
                           "N (cpmARr)" = num_obs_cpmAR,
                           "N (cpmERr)" = num_obs_cpmER
          ))

##### Immigration position effects
##### Pro-EU Liberal

cpmEL <- glm(p_yes ~ PTY_IMMIG_RESTR + country +  as.factor(YEAR), data = ELdata, family = quasibinomial(link = "logit"))
summary(cpmEL)

# Extract the number of observations
num_obs_cpmEL <- nobs(cpmEL)

#with robust SE:
cpmELr<-coeftest(cpmEL, vcov = vcovHC(cpmEL, type = "HC0"))
cpmELr


##### Anti-EU Liberal


cpmAL <- glm(p_yes ~ PTY_IMMIG_RESTR + country + as.factor(YEAR), data = ALdata, family = quasibinomial(link = "logit"))
summary(cpmAL)

# Extract the number of observations
num_obs_cpmAL <- nobs(cpmAL)

#with robust SE:
cpmALr<-coeftest(cpmAL, vcov = vcovHC(cpmAL, type = "HC0"))
cpmALr


##### Anti-EU Restrictive


cpmAR <- glm(p_yes ~ PTY_IMMIG_RESTR + country + as.factor(YEAR), data = ARdata, family = quasibinomial(link = "logit"))
summary(cpmAR)

# Extract the number of observations
num_obs_cpmAR <- nobs(cpmAR)

#with robust SE:
cpmARr<-coeftest(cpmAR, vcov = vcovHC(cpmAR, type = "HC0"))
cpmARr


##### Pro-EU Restrictive


cpmER <- glm(p_yes ~ PTY_IMMIG_RESTR + country + as.factor(YEAR), data = ERdata, family = quasibinomial(link = "logit"))
summary(cpmER)

# Extract the number of observations
num_obs_cpmER <- nobs(cpmER)

#with robust SE:
cpmERr<-coeftest(cpmER, vcov = vcovHC(cpmER, type = "HC0"))
cpmERr


stargazer(cpmELr, cpmALr, cpmARr, cpmERr, 
          apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/DossTypeModsIM.htm",
          add.lines = list("N (cpmELr)" = num_obs_cpmEL, 
                           "N (cpmALr)" = num_obs_cpmAL,
                           "N (cpmARr)" = num_obs_cpmAR,
                           "N (cpmERr)" = num_obs_cpmER
          ))
stargazer(cpmELr, cpmALr, cpmARr, cpmERr, 
          apply.coef = exp, t.auto=FALSE, p.auto=FALSE, report = "vc*s", out="tables&figures/DossTypeModsIM.tex",
          add.lines = list("N (cpmELr)" = num_obs_cpmEL, 
                           "N (cpmALr)" = num_obs_cpmAL,
                           "N (cpmARr)" = num_obs_cpmAR,
                           "N (cpmERr)" = num_obs_cpmER
          ))
