### Run Winterlight_Baseline_DataClean.r first 



# Check which participants are in WL dataset but missing from demographics spreadsheet --------
## Using most recent winterlight data 

# Find participant_external_ids in WL_combined that are not in demoALL
missing_ids <- WL_combined[!(WL_combined$participant_external_id %in% demoALL$participant_external_id), "participant_external_id", drop = FALSE]

# Check if there are any missing IDs and display the result
if (nrow(missing_ids) > 0) {
  cat("participant_external_ids in WL_combined but not in demoALL:\n")
  print(missing_ids)
} else {
  cat("All participant_external_ids in WL_combined are present in demoALL\n")
}


# Check which participants are in WL dataset but missing from psych spreadsheet --------


# Find participant_external_ids in WL_combined that are not in psych, excluding controls
missing_ids <- WL_combined[!(WL_combined$participant_external_id %in% psych$participant_external_id) & 
                             !grepl("^CTC|^C_CTC", WL_combined$participant_external_id), "participant_external_id", drop = FALSE]

# Check if there are any missing IDs and display the result
if (nrow(missing_ids) > 0) {
  cat("participant_external_ids in WL_combined but not in psych:\n")
  print(missing_ids)
} else {
  cat("All participant_external_ids in WL_combined are present in psych\n")
}




# Find participants with missing data and missing columns --------
# can be performed with different dataframe names 

# Function to identify columns with missing data for each participant
find_missing_data <- function(df) {
  # Initialize an empty list to store results
  results <- list()
  # Loop through each row in the dataframe
  for (i in 1:nrow(df)) {
    participant_id <- df$participant_external_id[i]
    missing_columns <- names(df)[which(is.na(df[i,]))]
    results[[participant_id]] <- missing_columns
  }
  
  return(results)
}

# Get the missing data summary for the demographic data 
missing_data_demo <- find_missing_data(demoALL)
# Convert the list to a data frame for easier saving to CSV
missing_demo_df <- do.call(rbind, lapply(names(missing_data_demo), function(id) {
  data.frame(participant_external_id = id, missing_columns = paste(missing_data_demo[[id]], collapse = ", "), stringsAsFactors = FALSE)
}))
# Save the summary to a CSV file
write.csv(missing_demo_df, "missing_demo.csv", row.names = FALSE)


# Get the missing data summary for the clinical data 
missing_data_clin <- find_missing_data(psych_filtered)
# Convert the list to a data frame for easier saving to CSV
missing_clin_df <- do.call(rbind, lapply(names(missing_data_clin), function(id) {
  data.frame(participant_external_id = id, missing_columns = paste(missing_data_clin[[id]], collapse = ", "), stringsAsFactors = FALSE)
}))
# Save the summary to a CSV file
write.csv(missing_clin_df, "missing_clin.csv", row.names = FALSE)

