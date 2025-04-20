library(haven)
library(dplyr)

setwd("~/Desktop/Uni/year 3/econometrics and data science")

#loading datasets (group 1 is bcs and group 2 is next steps, the number next to it is the age at time of interview)

group1_26 <- read_dta("bcs_sweep5/stata/stata13/bcs96x.dta") %>%
  mutate(group = 1, sweep_age = 26)

group1_34 <- read_dta("bcs_sweep7/stata/stata13_se/bcs_2004_followup.dta") %>%
  mutate(group = 1, sweep_age = 34)

group2_26 <- read_dta("next_steps_all/stata/stata13/safeguarded_eul/ns8_2015_main_interview.dta") %>%
  mutate(group = 2, sweep_age = 26)

group2_33 <- read_dta("next_steps_all/stata/stata13/safeguarded_eul/ns9_2022_main_interview.dta") %>%
  mutate(group = 2, sweep_age = 33)

#derived
  
group2_26_derived <- read_dta("next_steps_all/stata/stata13/safeguarded_eul/ns8_2015_derived.dta")
  
group2_33_derived <- read_dta("next_steps_all/stata/stata13/safeguarded_eul/ns9_2022_derived_variables.dta")

#join

group2_26_merge <- full_join(group2_26, group2_26_derived, by = "NSID")

group2_33_merge <- full_join(group2_33, group2_33_derived, by = "NSID")

# Select only relevant columns (b960219 <- has degree level qualification, b960259 <- is in full time employment, b960312 <- net pay)
group1_26_selected <- group1_26 %>%
  select(bcsid, sex, group, sweep_age, b960219, b960259, b960312)

#(b7highal <- highest qual level (8 is undergrad degree), b7zotha0 <- current activity (1 is FT employment), b7cnetpy <- net pay (this might be monthly)) 
group1_34_selected <- group1_34 %>%
  select(bcsid, bd7sex, group, sweep_age, b7highal, b7zotha0, b7cnetpy)


#(W8QUALB0B <- degree is highest qual, W8ACTIVITYC <- 1 if in employment, W8WRKHRSA <- 1 if in FT employment, W8NETA <- take home pay)
group2_26_selected <- group2_26_merge %>%
  select(NSID, W8CMSEX, group, sweep_age, W8QUALB0B, W8ACTIVITYC, W8WRKHRSA, W8NETA)

#(W9DAQLVLH <- highest qualification level, W9CURPAIDWK <- 1 if in FT employment, W9NETA <- take home pay)
group2_33_selected <- group2_33_merge %>%
  select(NSID, W9DSEX, group, sweep_age, W9DAQLVLH, W9CURPAIDWK, W9NETA)
