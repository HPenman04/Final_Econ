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
  select(bcsid, cgropay, cgroprd, econact) %>%
  
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
  full_join(cognitive_subset_10, by = "BCSID")

# Check for duplicated BCSIDs
any(duplicated(master_data$BCSID))


master_data_complete <- master_data[complete.cases(master_data), ]

names(master_data_complete)



head(master_data)



