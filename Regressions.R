suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(haven)
  library(mice)
  library(lmtest)
  library(sandwich)
  library(fs)
})




## ------------------------------------------------------------------
## 1.  Utility functions
## ------------------------------------------------------------------

clean_dataset_name <- function(path, root) {
  path %>%
    path_rel(start = root) %>%                  # 1) relative path
    path_ext_remove() %>%                       # 2) drop extension
    str_replace_all("[/\\s\\-]+", "_") %>%      # 3) tidy punctuation
    path_sanitize() %>%
    str_to_lower()
}

load_stata_folder <- function(root = here("Datasets_b2b")) {
  dir_ls(root, recurse = TRUE, type = "file", glob = "*.dta") |>
    set_names(~ clean_dataset_name(.x, root)) |>
    map(read_dta)
}

if (!exists("datasets")) {
  message("Loading datasets…")
  datasets <- load_stata_folder()
} else {
  message("`datasets` already in memory; skipping load.")
}

replace_missing <- function(data, codes = missing_codes) {
  data %>%
    mutate(across(
      any_of(names(codes)),
      ~ replace(.x, .x %in% codes[[cur_column()]], NA_real_)
    ))
}

## ------------------------------------------------------------------
## 2.  Key variables
## ------------------------------------------------------------------

# Education 
education_subset_29 <- datasets$sweep_6_age_29_stata_stata13_se_bcs6derived %>%
  select(BCSID, Highest_Academic_Level = HIACA00) %>%
  mutate(Highest_Academic_Level = na_if(Highest_Academic_Level, -9))

# Sex 
sex_subset_26 <- datasets$sweep_5_age_26_stata_stata13_bcs96x %>%
  transmute(
    BCSID = bcsid,
    Sex   = sex %>%
      haven::zap_labels() %>%
      na_if(-1) %>%
      factor(levels = c(1, 2), labels = c("Male", "Female"))
  )

hourly_wage_bcs6 <- function(df) {
  missing_codes <- list(
    cgropay   = c(9999998, 9999999),
    cgroprd   = c(6, 8, 9),
    cgropred  = 20:24,
    chours1   = c(98, 99),
    chours2   = c(998, 999),
    chours3   = c(998, 999),
    chours4   = c(998, 999)
  )
  
  period_multipliers <- c(
    `1`  = 52,   `2`  = 26,  `3`  = 13,  `4`  = 12,
    `5`  = 1,    `6`  = 52/3, `7` = 52/5, `8` = 52/6,
    `9`  = 52/7, `10` = 52/8, `11` = 6,    `12` = 8,
    `13` = 9,    `14` = 10,   `15` = 4,    `16` = 2
  )
  
  # 2) inline helper that zaps any code → NA
  replace_missing <- function(data) {
    data %>%
      mutate(across(
        all_of(names(missing_codes)),
        ~ replace(.x, .x %in% missing_codes[[cur_column()]], NA_real_)
      ))
  }
  
  # 3) the main pipeline
  df %>%
    select(
      bcsid, econact,
      cgropay, cgroprd, cgropred,
      chours1:chours4
    ) %>%
    mutate(across(everything(), haven::zap_labels)) %>%
    replace_missing() %>%                                         # zap bad codes
    mutate(
      ## pay period
      period_code = coalesce(cgropred, cgroprd),
      multiplier  = unname(period_multipliers[as.character(period_code)]),
      
      ## money & hours
      annual_pay   = cgropay * multiplier,
      Weekly_hours = coalesce(
        chours1 + chours3,
        chours2 + chours3,
        chours1,
        chours2
      ),
      Weekly_hours = if_else(between(Weekly_hours, 1, 99),
                             Weekly_hours, NA_real_),
      
      ## final wage
      Hourly_pay = annual_pay / (Weekly_hours * 52),
      Hourly_pay = if_else(between(Hourly_pay, 0, 500),
                           Hourly_pay, NA_real_),
      ln_wage    = log(Hourly_pay)
    ) %>%
    select(
      BCSID        = bcsid,
      Employment_Status     = econact,
      Hourly_pay,
      Weekly_hours,
      ln_wage
    )
}

salary_subset_29 <- hourly_wage_bcs6(
  datasets$sweep_6_age_29_stata_stata13_se_bcs2000
)

## ------------------------------------------------------------------
## 3. Control variables
## ------------------------------------------------------------------

missing_codes <- list(
  Region10        = c(-2, -1),
  Social_class10  = c(-2, -1),
  Family_income10 = c(-1,  8),
  ReadingScore10  = c(-1),
  MathsScore10    = c(-1),
  FinalSchoolType = c(-1),
  OrgType29       = c(8,  9),
  FirmSize29     = c(8,  9),
  UnionMem29      = c(8,  9),
  HouseOwnership10           = c(-1),
  SchoolDaysMissed10         = c(-1),
  SibCode10       = c(-1)
)



## 1. Background variables at age 10

background_subset_10 <- datasets$sweep_3_age_10_stata_stata13_bcs3derived %>%
  select(
    BCSID           = bcsid,
    Region10        = bd3regn,
    Social_class10  = bd3psoc,
    Family_income10 = bd3inc
  ) %>%
  replace_missing()


## 2. Cognitive scores at age 10

cognitive_subset_10 <- datasets$sweep_3_age_10_stata_stata13_bcs3derived %>%
  select(
    BCSID          = bcsid,
    ReadingScore10 = bd3read,
    MathsScore10   = bd3maths
  ) %>%
  replace_missing()


## 3. School type at age 10

school_subset_10 <- datasets$sweep_4_headteacher_stata_stata13_bcs70_htq %>%
  select(
    BCSID           = bcsid,
    FinalSchoolType = schtype
  ) %>%
  replace_missing() %>%
  mutate(
    PrivateSchool10 = if_else(FinalSchoolType == 2, 1, 0)
  )


## 4. Employment at age 29

employment_subset_29 <- datasets$sweep_6_age_29_stata_stata13_se_bcs2000 %>%
  select(
    BCSID        = bcsid,
    OrgType29    = cjorg,
    FirmSize29  = cjemps2,
    UnionMem29   = unionmem
  ) %>%
  replace_missing()


## 5. Additional background at age 10

background2_subset_10 <- datasets$sweep_3_age_10_stata_stata13_se_sn3723 %>%
  select(
    BCSID     = bcsid,
    HouseOwnership10     = d2,
    SchoolDaysMissed10   = j111,
    SibCode10 = a4a_42
  ) %>%
  replace_missing() %>%
  mutate(
    Siblings10 = SibCode10 - 1
  ) %>%
  select(BCSID, HouseOwnership10, SchoolDaysMissed10, Siblings10)





## ------------------------------------------------------------------
## 4.  Assemble the master file
## ------------------------------------------------------------------

base_variables <- reduce(
  list(salary_subset_29, sex_subset_26, education_subset_29),
  full_join, by = "BCSID"
)

master_data <- reduce(
  list(
    base_variables,
    background_subset_10,
    cognitive_subset_10,
    school_subset_10,
    employment_subset_29,
    background2_subset_10
  ),
  full_join,
  by = "BCSID"
)

names(master_data)



analysis <- master_data %>% 
  filter(
    Employment_Status %in% c(1,2),
    Weekly_hours > 0,
    Hourly_pay > 3, #Filtering to remove results below minimum wage
    Highest_Academic_Level > 3,
    !is.na(ln_wage),
    !is.na(Sex)          
  )


## Factorising Categorical Variables
num_cols <- c(
  "Hourly_pay", "Weekly_hours", "ln_wage",
  "ReadingScore10", "MathsScore10",
  "SchoolDaysMissed10", "Siblings10"
)
fac_cols <- setdiff(names(analysis), c("BCSID", num_cols))

analysis <- analysis %>%
  mutate(
    # convert all labelled or integer codes to factors
    across(all_of(fac_cols), ~ as_factor(.x)),
    # ensure truly continuous vars are naked numeric
    across(all_of(num_cols), as.numeric)
  )



control_vars <- c(
  "ln_wage",
  "Sex",
  "Highest_Academic_Level",
  "Region10",
  "ReadingScore10",
  "MathsScore10",
  "PrivateSchool10",
  "OrgType29",
  "FirmSize29",
  "UnionMem29",
  "HouseOwnership10",
  "SchoolDaysMissed10",
  "Siblings10"
)

imp <- mice(analysis[control_vars], m = 5, method = "pmm", seed = 123)

names(analysis)

## ------------------------------------------------------------------
## 5.  Regressions
## ------------------------------------------------------------------

## Specification 1

spec1_men <- lm(ln_wage ~ Highest_Academic_Level, data = subset(analysis, Sex == "Male"))
coeftest(spec1_men , vcov = vcovHC(spec1_men , type = "HC1"))

spec1_women <- lm(ln_wage ~ Highest_Academic_Level, data = subset(analysis, Sex == "Female"))
coeftest(spec1_women, vcov = vcovHC(spec1_women, type = "HC1"))

## Specification 2
fit_men_spec2 <- with(imp, lm(
  ln_wage ~ Highest_Academic_Level +
    Region10 +
    ReadingScore10 +
    MathsScore10 +
    PrivateSchool10,
  subset = Sex == "Male"
))
spec_2_men <- pool(fit_men_spec2)
summary(spec_2_men)

fit_women_spec2 <- with(imp, lm(
  ln_wage ~ Highest_Academic_Level +
    Region10 +
    ReadingScore10 +
    MathsScore10 +
    PrivateSchool10,
  subset = Sex == "Female"
))
spec_2_women <- pool(fit_women_spec2)
summary(spec_2_women)

## Specification 3
fit_men_spec3 <- with(imp, lm(
  ln_wage ~ 
    Highest_Academic_Level +
    Region10 +
    ReadingScore10 +
    MathsScore10 +
    PrivateSchool10 +
    OrgType29 +
    UnionMem29 +
    HouseOwnership10 +
    SchoolDaysMissed10 +
    Siblings10,
  subset = Sex == "Male"
))
spec_3_men <- pool(fit_men_spec3)
summary(spec_3_men)

fit_women_spec3 <- with(imp, lm(
  ln_wage ~ 
    Highest_Academic_Level +
    Region10 +
    ReadingScore10 +
    MathsScore10 +
    PrivateSchool10 +
    OrgType29 +
    UnionMem29 +
    HouseOwnership10 +
    SchoolDaysMissed10 +
    Siblings10,
  subset = Sex == "Female"
))
spec_3_women <- pool(fit_women_spec3)
summary(spec_3_women)

levels(analysis$Highest_Academic_Level)
