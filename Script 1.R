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
education_subset_29 <- sweep6age29_se_bcs6derived[, c("BCSID", "HIACA00")]
names(education_subset_29)[names(education_subset_29) == "HIACA00"] <- "Edu_level29"
attr(education_subset_29$Edu_level29, "label") <- "HIACA00"
education_subset_29$Edu_level29[education_subset_29$Edu_level29 == -9] <- NA


salary_subset_29 <- sweep6age29_se_bcs2000 %>%
  select(bcsid, cgropay, cgroprd, chours1) %>%
  
  # Recode “Don’t know”/“Not answered” to NA
  mutate(
    cgropay = if_else(cgropay %in% c(9999998, 9999999), NA_real_, cgropay),
    cgroprd  = if_else(cgroprd  %in% c(6, 8, 9),       NA_real_, cgroprd)
  ) %>%
  
  # Compute annual pay from pay amount + period
  mutate(
    gross_pay_annual = case_when(
      cgroprd == 1 ~ cgropay * 52,    # weekly → 52 weeks
      cgroprd == 2 ~ cgropay * 26,    # fortnight → 26
      cgroprd == 3 ~ cgropay * 13,    # four weeks → 13
      cgroprd == 4 ~ cgropay * 12,    # calendar month → 12
      cgroprd == 5 ~ cgropay,         # already annual
      TRUE         ~ NA_real_         # “Other” or missing
    )
  )



names(salary_subset_29) <- c("BCSID", "Grosspay29")
attr(salary_subset_29$Grosspay29, "label") <- "cgropay"
salary_subset_29$Grosspay29[salary_subset_29$Grosspay29 %in% c(9999998, 9999999)] <- NA


# 3. Prepare salary subset at age 46
system2("open", shQuote(here::here("Datasets/Sweep 10 Age 46/mrdoc/ukda_data_dictionaries/bcs_age46_main_ukda_data_dictionary.rtf")), wait = FALSE)
salary_subset_46 <- sweep10age46_bcs_age46_main[, c("BCSID", "B10GROA")]
names(salary_subset_46)[names(salary_subset_46) == "B10GROA"] <- "Grosspay46"
attr(salary_subset_46$Grosspay46, "label") <- "B10GROA"
salary_subset_46$Grosspay46[salary_subset_46$Grosspay46 %in% c(-9, -8, -1)] <- NA

# 4. Prepare cognitive subset at age 10
system2("open", shQuote(here::here("Datasets/Sweep 3 Age 10/mrdoc/ukda_data_dictionaries/bcs3derived_ukda_data_dictionary.rtf")), wait = FALSE)
cognitive_subset_10 <- sweep3age10_bcs3derived[, c("bcsid", "bd3read", "bd3maths")]
names(cognitive_subset_10)[names(cognitive_subset_10) == "bcsid"]   <- "BCSID"
names(cognitive_subset_10)[names(cognitive_subset_10) == "bd3read"]  <- "ReadingScore10"
names(cognitive_subset_10)[names(cognitive_subset_10) == "bd3maths"] <- "MathsScore10"
attr(cognitive_subset_10$ReadingScore10, "label") <- "bd3read"
attr(cognitive_subset_10$MathsScore10,   "label") <- "bd3maths"
cognitive_subset_10$ReadingScore10[cognitive_subset_10$ReadingScore10 == -1] <- NA
cognitive_subset_10$MathsScore10[cognitive_subset_10$MathsScore10 == -1] <- NA


# 5. Merge all prepared subsets
master_data <- list(
  education_subset_29,
  salary_subset_29,
  salary_subset_46,
  cognitive_subset_10
) %>%
  reduce(full_join, by = "BCSID")

# Check for duplicated BCSIDs
any(duplicated(master_data$BCSID))


master_data_complete <- master_data[complete.cases(master_data), ]

# Correlation between MathsScore10 and ReadingScore10
correlation_math_reading <- cor(master_data_complete$Grosspay29, master_data_complete$Grosspay46)

# Print the result
print(correlation_math_reading)


