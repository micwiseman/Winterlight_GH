library("dplyr")
library("here")
df1 <- read.csv(here("Final_Consolidated_Psychiatry_Data.csv"))
df2 <- read.csv(here("ClinicalAndSociodemo-TmsMw_DATA_2024-04-02_1350.csv"))

# Identify common columns
common_columns <- intersect(names(df1), names(df2))

# Ensure columns in common_columns are of the same type
for (col in common_columns) {
  class_df1 <- class(df1[[col]])
  class_df2 <- class(df2[[col]])
  
  # If classes differ, decide on a common type
  # Here we choose to convert to character if there's a mismatch
  if (class_df1 != class_df2) {
    df1[[col]] <- as.character(df1[[col]])
    df2[[col]] <- as.character(df2[[col]])
  }
}

# Now you can combine the data frames
combined_df <- bind_rows(df1[, common_columns], df2[, common_columns])

# Save the combined data frame
write.csv(combined_df, here("Final_Consolidated_Psychiatry_Data.csv"), row.names = FALSE)
