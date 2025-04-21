library(dplyr)
library(purrr)
library(here)


if (!exists("datasets")) {
  library(haven)
  library(stringr)
  
  root_dir <- "/Users/harrypenman/Documents/R studio/Econ Data Sci Test/Datasets"
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
    Degree = if_else(Edu_level29 >= 7, 1, 0)
  )

attr(education_subset_29$Edu_level29, "label") <- "HIACA00"
attr(education_subset_29$Degree, "label") <- "Derived"


# 2. Prepare Salary Subset
system2("open", shQuote(here::here("Datasets/Sweep 6 Age 29/mrdoc/ukda_data_dictionaries/bcs2000_ukda_data_dictionary.rtf")), wait = FALSE)
salary_subset_29 <- sweep6age29_se_bcs2000 %>%
  select(bcsid, cgropay, cgroprd, econact, chours1, chours5) %>%
  
  mutate(
    cgropay = haven::zap_labels(cgropay),
    cgroprd = haven::zap_labels(cgroprd)
  ) %>%
  mutate(
    cgropay = if_else(cgropay %in% c(9999998, 9999999), NA_real_, cgropay),
    cgroprd = if_else(cgroprd %in% c(6, 8, 9),          NA_real_, cgroprd)
  ) %>%
  
  # Convert pay to an annual figure
  mutate(
    gross_pay_annual = case_when(
      cgroprd == 1 ~ cgropay * 52,   # weekly
      cgroprd == 2 ~ cgropay * 26,   # fortnight
      cgroprd == 3 ~ cgropay * 13,   # four weeks
      cgroprd == 4 ~ cgropay * 12,   # calendar month
      cgroprd == 5 ~ cgropay,        # already annual
      TRUE         ~ NA_real_
    )
  ) %>%
  select(
    BCSID = bcsid, 
    gross_pay_annual_29 = gross_pay_annual, 
    employment_status = econact
  )

attr(salary_subset_29$gross_pay_annual_29, "label") <- "cgropay * cgroprd"
attr(salary_subset_29$employment_status, "label") <- "econact"




# 4. Prepare cognitive subset at age 10
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




# Load school type variables
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



# Select and clean parental education variables from Sweep 3 (age 10)

# Attach proper labels
attr(parent_education_subset_10$Father_Education10, "label") <- "c1.8"
attr(parent_education_subset_10$Mother_Education10, "label") <- "c1.9"


parent_education_subset_10 <- sweep3age10_se_sn3723 %>%
  transmute(
    BCSID = bcsid,
    Father_Education10 = case_when(
      c1_6  == 1 ~ 6, c1_5  == 1 ~ 5, c1_3  == 1 ~ 3, c1_2  == 1 ~ 2,
      c1_1  == 1 ~ 1, c1_4  == 1 ~ 4, c1_9  == 1 ~ 9,
      c1_10 == 1 ~ NA_real_, c1_11 == 1 ~ NA_real_, TRUE ~ NA_real_
    ),
    Mother_Education10 = case_when(
      c1_17 == 1 ~ 6, c1_16 == 1 ~ 5, c1_14 == 1 ~ 3, c1_13 == 1 ~ 2,
      c1_12 == 1 ~ 1, c1_15 == 1 ~ 4, c1_20 == 1 ~ 9,
      c1_21 == 1 ~ NA_real_, c1_22 == 1 ~ NA_real_, TRUE ~ NA_real_
    )
  )
attr(parent_education_subset_10$Father_Education10, "label") <- "Derived"
attr(parent_education_subset_10$Mother_Education10, "label") <- "Derived"



degree_subset_29 <- sweep6age29_se_bcs2000 %>%
  select(
    BCSID = bcsid,
    Subject_29 = edqsub66,
    Degree_grade29 = eddeg,
    Degree_location29 = edqloc66
  ) %>%
  mutate(
    Degree_grade29 = na_if(Degree_grade29, 8),
    Degree_grade29 = na_if(Degree_grade29, 9)
  )
attr(degree_subset_29$Degree_grade29, "label") <- "eddeg"
attr(degree_subset_29$Subject_29, "label") <- "edqsub66"
attr(degree_subset_29$Degree_location29, "label") <- "edqloc66"




# 5. Merge all prepared subsets
master_data <- education_subset_29 %>%
  full_join(salary_subset_29, by = "BCSID") %>%
  full_join(degree_subset_29, by = "BCSID") %>%
  full_join(background_subset_10, by = "BCSID") %>%
  full_join(school_subset_10, by = "BCSID") %>%
  full_join(parent_education_subset_10, by = "BCSID") %>%
  full_join(cognitive_subset_10, by = "BCSID")

# Check for duplicated BCSIDs
any(duplicated(master_data$BCSID))





master_data_complete <- master_data[complete.cases(master_data), ]

head(master_data_complete)



# Restrict master_data based on your sample selection criteria
restricted_data <- master_data %>%
  filter(
    Edu_level29 >= 3,                     # At least one A-level
    employment_status %in% c(1, 3),        # Employment status: employee full-time (1) or part-time (3)
    !is.na(gross_pay_annual_29)            # Non-missing gross annual pay
  ) %>%
  mutate(
    log_gross_pay = log(gross_pay_annual_29)
  )
  
restricted_data <- restricted_data %>%
  mutate(
    Edu_level29 = as_factor(Edu_level29),
    Degree_grade29 = as_factor(Degree_grade29),
    Degree_location29 = as_factor(Degree_location29),
    Region10 = as_factor(Region10),
    Social_class10 = as_factor(Social_class10),
    Family_income10 = as_factor(Family_income10),
    FinalSchoolType = as_factor(FinalSchoolType),
    Father_Education10 = as_factor(Father_Education10),
    Mother_Education10 = as_factor(Mother_Education10),
    employment_status = as_factor(employment_status)
  )



names(restricted_data)




model1 <- lm(log_gross_pay ~ Degree, data = restricted_data)
summary(model1)


model2 <- lm(log_gross_pay ~ Degree + ReadingScore10 + MathsScore10 + Social_class10 + Family_income10 + Region10 + FinalSchoolType + Father_Education10 + Mother_Education10, data = restricted_data)

summary(model2)




# List of control variables you want missing dummies for
controls_to_handle <- c(
  "Region10",
  "Social_class10",
  "Family_income10",
  "FinalSchoolType",
  "PrivateSchool10",
  "Father_Education10",
  "Mother_Education10",
  "ReadingScore10",
  "MathsScore10"
)


handle_missing_with_dummies_clean <- function(data, vars) {
  for (v in vars) {
    # Check if variable is numeric or categorical
    if (is.numeric(data[[v]])) {
      # For numeric variables: create missing dummy + fill NAs
      data[[paste0("Miss_", v)]] <- ifelse(is.na(data[[v]]), 1, 0)
      data[[v]][is.na(data[[v]])] <- 0
    } else {
      # For factor/categorical variables:
      # 1. Add "Missing" as a level
      # 2. Fill NAs with "Missing"
      data[[v]] <- forcats::fct_expand(data[[v]], "Missing")
      data[[v]][is.na(data[[v]])] <- "Missing"
      data[[v]] <- as.factor(data[[v]])
      
      # 3. DO NOT create missing dummies for factors anymore
    }
  }
  return(data)
}



# Apply it to your restricted_data
restricted_data <- handle_missing_with_dummies(restricted_data, controls_to_handle)


model3 <- lm(
  log_gross_pay ~ Degree 
  + ReadingScore10 + Miss_ReadingScore10
  + MathsScore10 + Miss_MathsScore10
  + Social_class10
  + Family_income10
  + Region10
  + FinalSchoolType
  + Father_Education10
  + Mother_Education10,
  data = restricted_data
)


summary(model3)
