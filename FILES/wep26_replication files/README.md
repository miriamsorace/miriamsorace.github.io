# Repository Overview

This repository contains the data and code required to reproduce the analyses in **Sorace and Zaun (2026)**.

## How to Run the Code

1. Place `sorace_zaun_Rcode.R` and all data files listed below in the same folder.

2. Open R (or RStudio) and set the working directory to that folder:

```r
setwd("path/to/your/folder")
```

3. Run the script

## Data Files
*leg_data_wordscores.csv*
Contains Wordscores estimates on the immigration and EU integration dimensions for all 111 EU legislative items included in the 1990–2018 EU immigration and asylum acquis.

*LegScoreVotePtyData.R* 
Contains roll‑call votes (RCVs) data for the 67 EU legislative items from the acquis that could be matched to Hix & Høyland (2022) RCV data from *leg_data_wordscores.csv*, as well as party‑level information.

*ZA2491_v1-1-0.sav* and *ESS_preference.csv*
Contain Eurobarometer and ESS public opinion data files used in robustness tests assessing whether immigration salience is a valid proxy for immigration position.
