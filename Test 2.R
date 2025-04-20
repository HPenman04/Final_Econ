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



names(sweep6age29_se_bcs6derived)



# Total number of rows
total_rows <- nrow(sweep6age29_se_bcs6derived)

# Functions treating -9 and 0 as missing
count_non_missing <- function(x) {
  sum(!is.na(x) & !(x %in% c(-9, 0)))
}

count_missing <- function(x) {
  sum(is.na(x) | x %in% c(-9, 0))
}

# Create the missing data summary
missing_summary <- data.frame(
  Variable = names(sweep6age29_se_bcs6derived),
  NonMissingCount = sapply(sweep6age29_se_bcs6derived, count_non_missing),
  NonMissingPercentage = sapply(sweep6age29_se_bcs6derived, count_non_missing) / total_rows * 100,
  MissingCount = sapply(sweep6age29_se_bcs6derived, count_missing),
  MissingPercentage = sapply(sweep6age29_se_bcs6derived, count_missing) / total_rows * 100
)

# View
print(missing_summary)


sweep6_subset <- sweep6age29_se_bcs6derived[, c("BCSID", "HIACA00")]

# Create a frequency table
hiaca00_table <- table(sweep6_subset$HIACA00)

# View it
print(hiaca00_table)


# Create the subset
salary_subset <- sweep6age29_se_bcs2000[, c("bcsid", "cgropay", "cgroprd", "cnetpay", "cnetprd", "seearn", "ojnetpw", "ojhours")]

# View the new dataset
View(salary_subset)



# Total number of rows
total_rows <- nrow(salary_subset)

# New functions: treat values > 99999 or NA as missing
count_non_missing <- function(x) {
  sum(!is.na(x) & x <= 99999)
}

count_missing <- function(x) {
  sum(is.na(x) | x > 99999)
}

# Create the missing data summary
missing_summary_salary <- data.frame(
  Variable = names(salary_subset),
  NonMissingCount = sapply(salary_subset, count_non_missing),
  NonMissingPercentage = sapply(salary_subset, count_non_missing) / total_rows * 100,
  MissingCount = sapply(salary_subset, count_missing),
  MissingPercentage = sapply(salary_subset, count_missing) / total_rows * 100
)

# View the table
print(missing_summary_salary)


# First, rename 'bcsid' to 'BCSID' in salary_subset
names(salary_subset)[names(salary_subset) == "bcsid"] <- "BCSID"

# Now merge the two datasets on BCSID
sweep6_subset <- merge(sweep6_subset, salary_subset, by = "BCSID", all = TRUE)




# Step 1: Select only the needed columns
filtered_data <- sweep6_subset[, c("BCSID", "HIACA00", "cgropay")]

# Step 2: Filter rows
filtered_data <- filtered_data[
  !filtered_data$HIACA00 %in% c(0, -9) & 
    filtered_data$cgropay < 999999, 
]
rownames(filtered_data) <- NULL

# Step 3: View the result
View(filtered_data)
filtered_data <- na.omit(filtered_data)


# Spearman correlation (for ordinal + continuous)
correlation_spearman <- cor(filtered_data$HIACA00, filtered_data$cgropay, method = "spearman", use = "complete.obs")

# Print it
print(correlation_spearman)


# Spearman correlation test
cor_test_result <- cor.test(filtered_data$HIACA00, filtered_data$cgropay, method = "spearman", exact = FALSE)

# Print full test output
print(cor_test_result)

print(sweep10age46_bcs_age46_main)


salary_subset_age46 <- sweep10age46_bcs_age46_main[, c("BCSID", "B10GROA")]

# Set -9, -8, and -1 to NA in B10GROA
salary_subset_age46$B10GROA[salary_subset_age46$B10GROA %in% c(-9, -8, -1)] <- NA

names(salary_subset)[names(salary_subset) == "bcsid"] <- "BCSID"

merged_data <- merge(filtered_data, salary_subset_age46, by = "BCSID", all = TRUE)


# First, filter to complete cases
correlation_data <- merged_data[!is.na(merged_data$cgropay) & !is.na(merged_data$B10GROA), ]
# Remove extreme outliers
correlation_data <- correlation_data[correlation_data$cgropay < 50000 & correlation_data$B10GROA < 100000, ]


# Now calculate correlation (Pearson)
correlation_result <- cor(correlation_data$cgropay, correlation_data$B10GROA, method = "pearson")

# Print
print(correlation_result)


# First filter complete cases again if needed
correlation_data <- merged_data[!is.na(merged_data$cgropay) & !is.na(merged_data$B10GROA), ]

# Basic scatter plot
plot(correlation_data$cgropay, correlation_data$B10GROA,
     xlab = "Gross Pay at Age 29 (cgropay)",
     ylab = "Gross Pay at Age 46 (B10GROA)",
     main = "Scatter Plot of Salary at 29 vs Salary at 46",
     pch = 16,  # Solid circle point style
     cex = 0.6) # Slightly smaller points



# Log-transform salaries (+1 to avoid log(0) issues)
correlation_data$log_cgropay <- log(correlation_data$cgropay + 1)
correlation_data$log_B10GROA <- log(correlation_data$B10GROA + 1)

# Scatterplot of log salaries
plot(correlation_data$log_cgropay, correlation_data$log_B10GROA,
     xlab = "Log Gross Pay at 29",
     ylab = "Log Gross Pay at 46",
     main = "Scatter Plot (Log Scale)",
     pch = 16, cex = 0.6)

# Add a line of best fit
abline(lm(log_B10GROA ~ log_cgropay, data = correlation_data), col = "blue", lwd = 2)

# Correlation on log salaries
correlation_log <- cor(correlation_data$log_cgropay, correlation_data$log_B10GROA, method = "pearson", use = "complete.obs")

# Print it
print(correlation_log)

