library(ggcorrplot)

#---------------------
# Data Preprocessing
#--------------------

setwd("/Users/avacorry-roberts1/Documents/Edinburgh/VA/assessment2/Assessment Data/SIMD_2020")

# Read in the CSV file for the SIMD data
simd_data <- read.csv(file="SIMD_Edinburgh_2020csv.csv", header=TRUE, na.strings = c("*", "n/a", "N/A"))

# Clean data
## Remove the non SIMD indicator data
simd_data <- simd_data %>%
  select(-c(SIMD2020v2_Rank, SIMD_2020v2_Percentile, SIMD2020v2_Vigintile, 
            SIMD2020v2_Decile, SIMD2020v2_Quintile, SIMD2020v2_Income_Domain_Rank, 
            SIMD2020_Employment_Domain_Rank, SIMD2020_Health_Domain_Rank, 
            SIMD2020_Education_Domain_Rank, SIMD2020_Access_Domain_Rank, 
            SIMD2020_Crime_Domain_Rank, SIMD2020_Housing_Domain_Rank, income_rate,
            employment_rate,crime_rate, overcrowded_rate,nocentralheating_rate))
## Exclude columns with any NA values
simd_data <- simd_data[, colSums(is.na(simd_data)) == 0]
## Define area label columns to exclude from numeric conversion
exclude_cols <- c("Data_Zone", "Intermediate_Zone", "Council_area")
## Identify character columns containing '%'
problematic_cols <- sapply(simd_data, function(col) {
  is.character(col) && any(grepl("[%]", col))
})
## Remove % and convert those columns to numeric
simd_data[problematic_cols] <- lapply(simd_data[problematic_cols], function(col) {
  as.numeric(gsub("[%]", "", col))  
})
## Convert all other non-excluded columns to numeric
numeric_cols <- setdiff(names(simd_data), exclude_cols)
simd_data[numeric_cols] <- lapply(simd_data[numeric_cols], as.numeric)

# Select numeric columns
numeric_data <- simd_data[sapply(simd_data, is.numeric)]

# Standardize the data creating Z-scores
data_standard <- as.matrix(scale(numeric_data))

# Add row names to the matrix (use row numbers if no other identifiers)
rownames(data_standard) <- 1:nrow(data_standard)

#---------------------
# Correlation Analysis
#---------------------

# Pearson correlation matrix
cor_matrix <- cor(data_standard, use = "complete.obs", method = "pearson")

# Plot the correlation matrix without hierarchical clustering
ggcorrplot(cor_matrix, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 1.25) +  # Add label size adjustment
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0, 
    name = "Correlation Strength"  # Change the legend title here
  ) +
  theme(
    axis.text.x = element_text(size = 4, angle = 90, hjust = 1),  # Rotate x-axis labels and adjust size
    axis.text.y = element_text(size = 4),                        # Adjust y-axis label size
    legend.text = element_text(size = 4),                         
    legend.title = element_text(size= 4)
  )

# Create the correlation matrix
cor_matrix <- cor(data_standard, use = "complete.obs", method = "pearson")

# Create a data frame with the correlation values and their corresponding variable pairs
cor_values <- as.data.frame(as.table(cor_matrix))
colnames(cor_values) <- c("Var1", "Var2", "Correlation")

# Remove self-correlations (diagonal values), where Var1 == Var2
cor_values <- cor_values[cor_values$Var1 != cor_values$Var2, ]

# Sort correlations by absolute value (weakest correlations first)
cor_values_sorted <- cor_values[order(-abs(cor_values$Correlation)), ]

#-------------------------------------
# Strongest correlations (groups of 4)
#-------------------------------------

# Find the groups of 4 variables with the strongest correlations
strong_cor_groups <- list()

for (i in 1:(nrow(cor_values_sorted) - 3)) {
  current_group <- unique(c(cor_values_sorted[i, "Var1"], cor_values_sorted[i, "Var2"], 
                            cor_values_sorted[i + 1, "Var1"], cor_values_sorted[i + 1, "Var2"],
                            cor_values_sorted[i + 2, "Var1"], cor_values_sorted[i + 2, "Var2"],
                            cor_values_sorted[i + 3, "Var1"], cor_values_sorted[i + 3, "Var2"]))
  ## Check if the 4 variables are distinct (no duplicates)
  if (length(unique(current_group)) == 4) {
    strong_cor_groups <- c(strong_cor_groups, list(current_group))
  }
}

# Display the highest correlation groups
print(strong_cor_groups)

# Save to CSV
write.csv(strong_cor_groups, "4strong_correlations.csv", row.names = FALSE)

#-------------------------------------
# Weakest correlations (groups of 4)
#-------------------------------------

# Sort correlations by absolute values (weakest correlation first)
cor_values_sorted_weakest <- cor_values[order(abs(cor_values$Correlation)), ]

# List of groups of 4 variables with the weakest correlations
weakest_cor_groups <- list()

for (i in 1:(nrow(cor_values_sorted_weakest) - 3)) {
  current_group <- unique(c(cor_values_sorted_weakest[i, "Var1"], cor_values_sorted_weakest[i, "Var2"], 
                            cor_values_sorted_weakest[i + 1, "Var1"], cor_values_sorted_weakest[i + 1, "Var2"],
                            cor_values_sorted_weakest[i + 2, "Var1"], cor_values_sorted_weakest[i + 2, "Var2"],
                            cor_values_sorted_weakest[i + 3, "Var1"], cor_values_sorted_weakest[i + 3, "Var2"]))
  
  # Check if the 4 variables are distinct (no duplicates)
  if (length(current_group) == 4) {
    weakest_cor_groups <- c(weakest_cor_groups, list(current_group))
  }
}

# Display the lowest correlation groups
print(weakest_cor_groups)

# Save to CSV
write.csv(weakest_cor_groups, "4weak_correlations.csv", row.names = FALSE)
