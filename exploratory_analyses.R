##### EXPLORATORY ANALYSES ########

# Taking out out unwanted columns 
to_exclude <-c("X", "sample_id", "task_name",
               "session_label", "sample-datetime_completed_utc", "stimulus_filename",
               "terminal_state", "terminal_message", "race", "prior_tms_response", 
               "prior_tms", "prior_ect_response", "prior_ect", "prior_t3", "prior_lithium", 
               "prior_ketamine", "prior_anticonv_sufficient", "prior_maoi_sufficient", 
               "prior_3aa_sufficient", "prior_2aa_sufficient", "prior_vortioxetine_sufficient", 
               "prior_trazodone_sufficient", "prior_tca_sufficient", "prior_mirt_sufficient", 
               "prior_bupropion_sufficient", "prior_snri_sufficient", "prior_ssri_sufficient", 
               "yrs_speak_english", "primary_language", 
               "handedness","tms_protocol","gender","dob_mo", "dob_yr","sample_datetime_completed_utc")
WL_feeling_BLex<-WL_feeling_BL[,!(names(WL_feeling_BL) %in% to_exclude)]
# Check for columns with all NA values
na_check <- sapply(WL_feeling_BLex, function(col) all(is.na(col)))
# Exclude columns with all NA values
WL_feeling_BLex <- WL_feeling_BLex[, !na_check]
# Identify columns with integer values (excluding characters)
integer_cols <- sapply(WL_feeling_BLex, function(col) all(is.na(col) | (is.integer(col) & !is.character(col))))
# Convert identified integer columns to numeric
WL_feeling_BLex[, integer_cols] <- lapply(WL_feeling_BLex[, integer_cols], as.numeric)
# Check for columns with all zero values
zero_check <- sapply(WL_feeling_BLex, function(col) all(col == 0 | is.na(col)))
# Exclude columns with all zero values
WL_feeling_BLex <- WL_feeling_BLex[, !zero_check]
for (col in names(WL_feeling_BLex)) {
  WL_feeling_BLex[[col]][is.infinite(WL_feeling_BLex[[col]])] <- NA
}



# Taking out out unwanted columns 
to_exclude <-c("X", "sample_id", "task_name",
               "session_label", "sample-datetime_completed_utc", "stimulus_filename",
               "terminal_state", "terminal_message", "race", "prior_tms_response", 
               "prior_tms", "prior_ect_response", "prior_ect", "prior_t3", "prior_lithium", 
               "prior_ketamine", "prior_anticonv_sufficient", "prior_maoi_sufficient", 
               "prior_3aa_sufficient", "prior_2aa_sufficient", "prior_vortioxetine_sufficient", 
               "prior_trazodone_sufficient", "prior_tca_sufficient", "prior_mirt_sufficient", 
               "prior_bupropion_sufficient", "prior_snri_sufficient", "prior_ssri_sufficient", 
               "yrs_speak_english", "primary_language", 
               "handedness","tms_protocol","gender","dob_mo", "dob_yr","sample_datetime_completed_utc")
MDD_feeling_BLex<-MDD_feeling_BL[,!(names(MDD_feeling_BL) %in% to_exclude)]
# Check for columns with all NA values
na_check <- sapply(MDD_feeling_BLex, function(col) all(is.na(col)))
# Exclude columns with all NA values
WL_feeling_BLex <- MDD_feeling_BLex[, !na_check]
# Identify columns with integer values (excluding characters)
integer_cols <- sapply(MDD_feeling_BLex, function(col) all(is.na(col) | (is.integer(col) & !is.character(col))))
# Convert identified integer columns to numeric
MDD_feeling_BLex[, integer_cols] <- lapply(MDD_feeling_BLex[, integer_cols], as.numeric)
# Check for columns with all zero values
zero_check <- sapply(MDD_feeling_BLex, function(col) all(col == 0 | is.na(col)))
# Exclude columns with all zero values
MDD_feeling_BLex <- MDD_feeling_BLex[, !zero_check]
for (col in names(MDD_feeling_BLex)) {
  MDD_feeling_BLex[[col]][is.infinite(MDD_feeling_BLex[[col]])] <- NA
}



# Initialize an empty data frame to store the results
results_df <- data.frame(
  ColumnName = character(),
  Control_mean = numeric(),
  Control_SD = numeric(),
  MDD_mean = numeric(),
  MDD_SD = numeric(),
  FValue = numeric(),
  PValue = numeric(),
  EffectSize = numeric(),
  stringsAsFactors = FALSE
)

# Initialize a vector to store column names with zero residual sum of squares
error_columns <- character()

# Loop through each column in the data frame (excluding the response variable)
for (col in colnames(WL_feeling_BLex)[-1]) {
  cols_to_exclude <- c("participant_group", "age_screening", "sex")
  if (!(col %in% cols_to_exclude)) {
    filtered_data <- na.omit(WL_feeling_BLex[c("participant_group", "age_screening", "sex", col)])
    
    # Check for the number of levels in factor variables
    if (any(sapply(filtered_data[, c("participant_group", "sex")], function(x) length(unique(x))) < 2)) {
      cat("Skipping column", col, "due to insufficient factor levels after filtering.\n")
      next
    }
    
    sum_stats <- filtered_data %>%
      group_by(participant_group) %>%
      get_summary_stats(all_of(col), type = "common")
    
    model1 <- lm(paste(col, "~ age_screening + sex +factor(participant_group)"), data = filtered_data)
    
    # Use tryCatch to handle the error and skip problematic columns
    tryCatch({
      ancova <- Anova(model1, type = "III")
      p_value <- ancova$`Pr(>F)`[5]
      if (p_value < 0.05) {
        # Calculate effect size (partial eta squared)
        eta_squared <- ancova[["Sum Sq"]][6] / sum(ancova[["Sum Sq"]])
        # Add the results to the results data frame and round values to 3 decimal places
        results_df <- rbind(results_df, data.frame(
          ColumnName = col,
          Control_mean = round(sum_stats[1, 8], 4),
          Control_SD = round(sum_stats[1, 9], 4),
          MDD_mean = round(sum_stats[2, 8], 4),
          MDD_SD = round(sum_stats[2, 9], 4),
          FValue = round(ancova$`F value`[5], 4),
          PValue = round(p_value, 4),
          EffectSize = round(eta_squared, 4)
        ))
      }
    }, error = function(e) {
      error_columns <- c(error_columns, col)  # Store the name of the problematic column
      cat("Warning: Skipping column", col, "due to error:", conditionMessage(e), "\n")
    })
  }
}

# Sort the results dataframe by PValue in ascending order
results_df <- results_df[order(results_df$PValue), ]

# Print the names of columns causing the error
if (length(error_columns) > 0) {
  cat("Columns with errors:", paste(error_columns, collapse = ", "), "\n")
}

# Print the results data frame
print(results_df)

##LM exploratory
# Initialize an empty data frame to store the results
## LM exploratory
# Initialize an empty data frame to store the results
results_df2 <- data.frame(
  ClinicalVariable = character(),
  SpeechVariable = character(),
  Beta = numeric(),
  SE = numeric(),
  TValue = numeric(),
  PValue = numeric(),
  stringsAsFactors = FALSE
)

# Initialize a vector to store column names with zero residual sum of squares
error_columns <- character()

# List of clinical variables
clinical_vars <- c("clinical_var1", "clinical_var2", "clinical_var3") # Replace with actual clinical variable names

# Loop through each clinical variable
for (clinical_var in clinical_vars) {
  # Loop through each speech variable
  for (col in colnames(MDD_feeling_BLex)[-1]) {
    cols_to_exclude <- c("participant_group", "testing_location", "age_screening", "sex", "years_education", "first_language_english", "age_learned_english", clinical_vars)
    if (!(col %in% cols_to_exclude)) {
      # Omit rows with missing values in the current column and the clinical variable
      filtered_data <- na.omit(MDD_feeling_BLex[c("participant_group", "age_screening", "sex", "testing_location", clinical_var, col)])
      
      # Check if the response variable is numeric
      if (is.numeric(filtered_data[[col]])) {
        lm_formula <- as.formula(paste(col, "~", clinical_var, "+ sex + age_screening + testing_location"))
        lm_model <- tryCatch({
          lm(lm_formula, data = filtered_data)
        }, error = function(e) {
          NULL
        })
        
        if (!is.null(lm_model)) {
          summary_lm <- summary(lm_model)
          coef_table <- summary_lm$coefficients
          
          # Extract relevant statistics for the clinical variable
          beta <- coef_table[which(rownames(coef_table) == clinical_var), 1]
          se <- coef_table[which(rownames(coef_table) == clinical_var), 2]
          t_value <- coef_table[which(rownames(coef_table) == clinical_var), 3]
          p_value <- coef_table[which(rownames(coef_table) == clinical_var), 4]
          
          # Add the results to the results data frame
          results_df2 <- rbind(results_df2, data.frame(
            ClinicalVariable = clinical_var,
            SpeechVariable = col,
            Beta = beta,
            SE = se,
            TValue = t_value,
            PValue = p_value
          ))
        } else {
          error_columns <- c(error_columns, col)  # Store the name of the problematic outcome variable
          cat("Warning: Skipping outcome variable", col, "due to model fitting error.\n")
        }
      } else {
        cat("Warning: Skipping outcome variable", col, "because it is not numeric.\n")
      }
    } else {
      cat("Warning: Skipping outcome variable", col, "due to missing or infinite values.\n")
    }
  }
}

# Round and reorder the results
results_df2$Beta <- round(results_df2$Beta, 4)
results_df2$SE <- round(results_df2$SE, 4)
results_df2$TValue <- round(results_df2$TValue, 4)
results_df2$PValue <- round(results_df2$PValue, 4)
results_df2 <- results_df2[order(-results_df2$Beta), ]

print(results_df2)
#overlap

# Filter results_df2 based on conditions
filtered_results <- results_df2[results_df2$PValue < 0.1 & results_df2$Predictor %in% results_df$ColumnName, ]

# Print the filtered results
print(filtered_results)

# Filter results_df2 based on the absolute value of Beta >= 1
filtered_results <- results_df2[abs(results_df2$Beta) >= 1 & results_df2$Predictor %in% results_df$ColumnName, ]

# Print the filtered results
print(filtered_results)

