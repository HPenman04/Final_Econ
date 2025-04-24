library(dplyr)
library(purrr)
library(here)
library(haven)
library(sandwich)
library(lmtest)



if (!exists("datasets")) {
  library(haven)
  library(stringr)
  
  root_dir <- "/Users/harrypenman/Documents/R studio/Econ Data Sci Test/Datasets_b2b"
  dta_files <- list.files(path = root_dir, pattern = "\\.dta$", full.names = TRUE, recursive = TRUE)
  datasets <- list()
  
  for (file in dta_files) {
    clean_name <- file %>%
      str_replace(root_dir, "") %>%
      str_replace_all("/", "_") %>%
      str_replace_all(" ", "") %>%
      str_replace_all("-", "") %>%
      str_replace("_stata_stata13", "") %>%
      str_replace_all("\\.dta$", "") %>%
      str_to_lower() %>%
      str_replace("^_", "")   # NEW: remove leading underscore if present
    
    datasets[[clean_name]] <- read_dta(file)
    cat("Loaded:", clean_name, "\n")
  }
  
  list2env(datasets, envir = .GlobalEnv)
}

# 1. Prepare education subset
system2("open", shQuote(here::here("Datasets/Sweep 6 Age 29/mrdoc/ukda_data_dictionaries/bcs6derived_ukda_data_dictionary.rtf")), wait = FALSE)
education_subset_29 <- sweep6age29_se_bcs6derived %>%
  select(
    BCSID,
    Edu_level29 = HIACA00
  ) %>%
  mutate(
    Edu_level29 = na_if(Edu_level29, -9),
  )

attr(education_subset_29$Edu_level29, "label") <- "HIACA00"


sex_subset_26 <- sweep5age26_bcs96x %>%
  select(
    BCSID = bcsid,      # cohort member ID
    sex
  ) %>%
  mutate(
    sex = haven::zap_labels(sex),
    sex = na_if(sex, -1),
    sex = factor(sex,
                 levels = c(1, 2),
                 labels = c("Male", "Female")),
  )


# 2. Prepare Salary Subset
system2("open", shQuote(here::here("Datasets/Sweep 6 Age 29/mrdoc/ukda_data_dictionaries/bcs2000_ukda_data_dictionary.rtf")), wait = FALSE)
hourly_wage_bcs6 <- function(df) {
  df %>%
    select(
      BCSID      = bcsid,
      cgropay, cgroprd, cgropred,
      chours1, chours2, chours3, chours4, econact
    ) %>%
    mutate(across(everything(), zap_labels)) %>%
    
    # 1. clean missings
    mutate(
      cgropay  = if_else(cgropay  %in% c(9999998,9999999), NA_real_, cgropay),
      cgroprd  = if_else(cgroprd  %in% c(6, 8, 9),          NA_real_, cgroprd),
      cgropred = if_else(cgropred %in% 20:24,               NA_real_, cgropred),
      
      chours1  = if_else(chours1 %in% c(98,  99),   NA_real_, chours1),
      chours2  = if_else(chours2 %in% c(998, 999),  NA_real_, chours2),
      chours3  = if_else(chours3 %in% c(998, 999),  NA_real_, chours3),
      chours4  = if_else(chours3 %in% c(998, 999),  NA_real_, chours4)
    ) %>%
    
    # 2. annualise pay (prefer back‑coded cgropred)
    mutate(
      period_code = coalesce(cgropred, cgroprd),
      multiplier  = case_when(
        period_code == 1  ~ 52,
        period_code == 2  ~ 26,
        period_code == 3  ~ 13,
        period_code == 4  ~ 12,
        period_code == 5  ~  1,
        period_code == 6  ~ 52/3,
        period_code == 7  ~ 52/5,
        period_code == 8  ~ 52/6,
        period_code == 9  ~ 52/7,
        period_code == 10 ~ 52/8,
        period_code == 11 ~  6,
        period_code == 12 ~  8,
        period_code == 13 ~  9,
        period_code == 14 ~ 10,
        period_code == 15 ~  4,
        period_code == 16 ~  2,
        TRUE              ~ NA_real_
      ),
      annual_pay = cgropay * multiplier
    ) %>%
    
    # 3. best-available weekly hours: usual + paid OT
    mutate(
      weekly_hours = coalesce(
        # 1) usual hours + paid OT + unpaid OT
        chours1 + chours3,
        # 2) piece‑rate hours + paid OT + unpaid OT
        chours2 + chours3,
        # 3) usual hours + unpaid OT
        chours1,
        # 4) piece‑rate hours + unpaid OT
        chours2,
        # 5) usual hours + paid OT  (original 1st choice)
        chours1 + chours3,
        # 6) piece‑rate hours + paid OT (original 2nd choice)
        chours2 + chours3,
        # 7) usual hours only
        chours1,
        # 8) piece‑rate hours only
        chours2
      ),
      weekly_hours = if_else(weekly_hours < 1 | weekly_hours > 99,
                             NA_real_, weekly_hours)
    ) %>%
    
    # 4. compute hourly rate and log
    mutate(
      hourly_pay = annual_pay / (weekly_hours * 52),
      hourly_pay = if_else(hourly_pay <= 0 | hourly_pay > 500,
                           NA_real_, hourly_pay),
      ln_wage    = log(hourly_pay)
    ) 
}

salary_subset_29 <- hourly_wage_bcs6(sweep6age29_se_bcs2000)

salary_subset_29 <- salary_subset_29 %>%
  select(BCSID, econact, hourly_pay, weekly_hours, ln_wage)




# 5. Merge all prepared subsets
master_data <- education_subset_29 %>%
  full_join(salary_subset_29, by = "BCSID") %>%
  full_join(sex_subset_26, by = "BCSID")




master_data <- master_data %>% 
  mutate(
    edu_cat = case_when(
      Edu_level29 %in% 4                 ~ "A_level 1",
      Edu_level29 %in% 5                 ~ "A_level 2",
      Edu_level29 %in% 6      ~ "Non_degree_HE",
      Edu_level29 %in% 7               ~ "UG_degree",
      Edu_level29 %in% 8               ~ "PG_degree",
      TRUE                                 ~ NA_character_
    ),
    edu_cat = factor(edu_cat, 
                     levels = c("A_level 1", "A_level 2", "Non_degree_HE","UG_degree","PG_degree"))
  )


master_data <- master_data %>%
  filter(hourly_pay > 3, hourly_pay < 500)



  


analysis <- master_data %>% 
  filter(
    econact %in% c(1,2),
    weekly_hours > 0,
    !is.na(edu_cat),
    !is.na(ln_wage),
    !is.na(sex)          # drop the few with missing sex
  )

# first, compute the 1st and 99th percentile bounds
lower_bound <- quantile(analysis$hourly_pay, 0.01, na.rm = TRUE)
upper_bound <- quantile(analysis$hourly_pay, 0.99, na.rm = TRUE)

# now filter the analysis set to only those observations within those bounds
analysis_trimmed <- analysis %>%
  filter(
    hourly_pay >= lower_bound,
    hourly_pay <= upper_bound
  )



### Spec 1 ###
spec1_men <- lm(ln_wage ~ edu_cat, data = subset(analysis_trimmed, sex == "Male"))
spec1_women <- lm(ln_wage ~ edu_cat, data = subset(analysis_trimmed, sex == "Female"))

coeftest(spec1_men , vcov = vcovHC(spec1_men , type = "HC1"))
coeftest(spec1_women, vcov = vcovHC(spec1_women, type = "HC1"))


### Spec 2 ###

system2("open", shQuote(here::here("Datasets/Sweep 3 Age 10/mrdoc/ukda_data_dictionaries/bcs3derived_ukda_data_dictionary.rtf")), wait = FALSE)
background_subset_10 <- sweep3age10_bcs3derived %>%
  select(
    BCSID = bcsid,
    Region10 = bd3regn,
    Social_class10 = bd3psoc,
    Family_income10 = bd3inc
  ) %>%
  mutate(
    Region10 = na_if(Region10, -2),
    Region10 = na_if(Region10, -1),
    Social_class10 = na_if(Social_class10, -2),
    Social_class10 = na_if(Social_class10, -1),
    Family_income10 = na_if(Family_income10, -1),
    Family_income10 = na_if(Family_income10, 8)
  )

attr(background_subset_10$Region10, "label") <- "bd3regn"
attr(background_subset_10$Social_class10, "label") <- "bd3psoc"
attr(background_subset_10$Family_income10, "label") <- "bd3inc"


system2("open", shQuote(here::here("Datasets/Sweep 3 Age 10/mrdoc/ukda_data_dictionaries/bcs3derived_ukda_data_dictionary.rtf")), wait = FALSE)
cognitive_subset_10 <- sweep3age10_bcs3derived %>%
  select(
    BCSID         = bcsid,
    ReadingScore10 = bd3read,
    MathsScore10   = bd3maths
  ) %>%
  mutate(
    ReadingScore10 = na_if(ReadingScore10, -1),
    MathsScore10   = na_if(MathsScore10,   -1)
  )
attr(cognitive_subset_10$ReadingScore10, "label") <- "bd3read"
attr(cognitive_subset_10$MathsScore10,   "label") <- "bd3maths"

system2("open", shQuote(here::here("'Datasets/Sweep 4 Headteacher/mrdoc/bcs70_htq_ukda_data_dictionary.rtf'")), wait = FALSE)
school_subset_10 <- sweep4headteacher_bcs70_htq %>%
  select(
    BCSID      = bcsid,
    FinalSchoolType = schtype
  ) %>%
  mutate(
    FinalSchoolType = na_if(FinalSchoolType, -1),
    PrivateSchool10 = ifelse(FinalSchoolType == 2, 1, 0) # 2 = Independent school
  )

attr(school_subset_10$PrivateSchool10, "label") <- "Private school"
attr(school_subset_10$FinalSchoolType, "label") <- "schtype"

spec_2 <- analysis_trimmed %>%
  full_join(background_subset_10, by = "BCSID") %>%
  full_join(cognitive_subset_10, by = "BCSID") %>%
  full_join(school_subset_10, by = "BCSID")




spec_2 <- spec_2 %>%
  mutate(
    Region10 = as_factor(Region10),
    Social_class10 = as_factor(Social_class10),
    Family_income10 = as_factor(Family_income10),
    FinalSchoolType = as_factor(FinalSchoolType),
    sex = as_factor(sex),
    employment_status = as_factor(econact)
  )

names(spec_2)

analysis2 <- spec_2 %>% 
  filter(
    econact %in% c(1,2),
    weekly_hours > 0,
    !is.na(edu_cat),
    !is.na(ln_wage),
    !is.na(sex)          # drop the few with missing sex
  )


library(mice)

analysis2_clean <- analysis2 |>
  mutate(across(where(is.labelled), haven::zap_labels))

# Convert only desired vars to factor (exclude score vars)
labelled_vars <- names(analysis2_clean)[sapply(analysis2_clean, is.labelled)]
factor_vars <- setdiff(labelled_vars, c("ReadingScore10", "MathsScore10"))

analysis2_clean <- analysis2_clean |>
  mutate(across(all_of(factor_vars), as_factor))


# Select variables for imputation (include sex for subsetting)
vars <- c("ln_wage", "edu_cat", "Region10", "ReadingScore10",
          "MathsScore10", "PrivateSchool10", "sex")

# Run multiple imputation
imp <- mice(analysis2_clean[vars], m = 5, method = "pmm", seed = 123)

# Fit model across imputed datasets (subset to males)
fit_men <- with(imp, lm(
  ln_wage ~ edu_cat + Region10 + ReadingScore10 + MathsScore10 + PrivateSchool10,
  subset = sex == "Male"
))

# Pool results and summarise
spec_2_men <- pool(fit_men)
summary(spec_2_men)



fit_women <- with(imp, lm(
  ln_wage ~ edu_cat + Region10 + ReadingScore10 + MathsScore10 + PrivateSchool10,
  subset = sex == "Female"
))

spec_2_women <- pool(fit_women)
summary(spec_2_women)



#### Spec 3 ####

system2("open", shQuote(here::here("Datasets/Sweep 6 Age 29/mrdoc/ukda_data_dictionaries/bcs2000_ukda_data_dictionary.rtf")), wait = FALSE)
employment_subset_29 <- sweep6age29_se_bcs2000 %>%
  select(BCSID = bcsid, cjorg, cjemps2, unionmem) %>%
  mutate(
    cjorg = if_else(cjorg %in% c(8, 9), NA_real_, cjorg),
    cjemps2 = if_else(cjemps2  %in% c(8, 9),          NA_real_, cjemps2),
    unionmem = if_else(unionmem  %in% c(8, 9),          NA_real_, unionmem))

background2_subset_10 <- sweep3age10_se_sn3723 %>%
  select(BCSID = bcsid, d2, j111, a4a_42) %>%
  mutate(siblings = a4a_42 - 1)

spec_3 <- analysis2_clean %>%
  full_join(background2_subset_10, by = "BCSID") %>%
  full_join(employment_subset_29, by = "BCSID")

spec_3 <- spec_3 %>%
  mutate(
    cjorg = as_factor(cjorg),
    cjemps2 = as_factor(cjemps2),
    unionmem = as_factor(unionmem),
  )
  
analysis3 <- spec_3 %>% 
  filter(
    econact %in% c(1,2),
    weekly_hours > 0,
    !is.na(edu_cat),
    !is.na(ln_wage),
    !is.na(sex) 
  )



# Define variables
vars2 <- c("ln_wage", "edu_cat", "Region10", "ReadingScore10",
           "MathsScore10", "PrivateSchool10", "sex", "cjorg", "cjemps2",
           "unionmem", "d2", "j111", "siblings")

# 1a) strip any haven_labelled cruft
analysis3_clean <- analysis3 %>%
  mutate(across(all_of(vars2), zap_labels))

# 1b) coerce continuous vars back to numeric (ln_wage came out factor, etc)
analysis3_clean <- analysis3_clean %>%
  mutate(
    ln_wage  = as.numeric(as.character(ln_wage)),
    d2       = as.numeric(d2),
    j111     = as.numeric(j111),
    siblings = as.numeric(siblings)
  )

# 1c) everything else → a plain factor
cat_vars <- setdiff(vars2, c("ln_wage","d2","j111","siblings", "MathsScore10", "ReadingScore10"))
analysis3_clean <- analysis3_clean %>%
  mutate(across(all_of(cat_vars), as_factor))


# Run multiple imputation
imp <- mice(analysis3_clean[vars2], m = 5, method = "pmm", seed = 123)

# Fit model across imputed datasets (subset to males)
fit_men2 <- with(imp, lm(
  ln_wage ~ edu_cat + Region10 + ReadingScore10 + MathsScore10 + PrivateSchool10 + cjorg + unionmem + d2 + j111 + siblings,
  subset = sex == "Male"
))

# Pool results and summarise
spec_3_men <- pool(fit_men2)
summary(spec_3_men)


fit_women2 <- with(imp, lm(
  ln_wage ~ edu_cat + Region10 + ReadingScore10 + MathsScore10 + 
    PrivateSchool10 + cjorg + unionmem + d2 + j111 + siblings,
  subset = sex == "Female"
))

# Pool results and summarise
spec_3_women <- pool(fit_women2)
summary(spec_3_women)
