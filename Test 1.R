# Install 'haven' if needed
# install.packages("haven")

library(haven)

# Load the dataset into an R data frame
bcs_data <- read_dta("/Users/harrypenman/Downloads/UKDA-3723-stata/stata/stata13_se/sn3723.dta")

# Peek at the first few rows
head(bcs_data)

# Check structure
str(bcs_data)


sapply(bcs_data, function(x) attr(x, "label"))


# Get variable labels
var_labels <- sapply(bcs_data, function(x) attr(x, "label"))

# Combine into a data frame so it’s easy to write out
labels_df <- data.frame(
  variable = names(var_labels), 
  label    = unname(var_labels), 
  stringsAsFactors = FALSE
)

# Write to a text file (tab-separated)
write.table(labels_df,
            file = "bcs_variable_labels.txt",
            sep = "\t",
            row.names = FALSE,
            quote = FALSE)


bcs_data2 <- read_dta("'/Users/harrypenman/Documents/R studio/Econ Data Sci Test/UKDA-5641-stata/stata/stata13/bcs70_response_1970-2021.dta'")
