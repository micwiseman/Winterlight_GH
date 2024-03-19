library(tidyverse)
library(readxl)

# Function to load and preprocess Winterlight data
load_preprocess_WL <- function(file_path) {
  WL <- read.csv(file_path)
  WL <- WL[grep("^(TMS|MDD|CTC)", WL$participant_external_id), ]
  WL
}

# Load and preprocess the data
WL <- load_preprocess_WL("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook_rTMS_2023_11_03.csv")
WL2 <- load_preprocess_WL("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook_rTMSremote_2023_11_03.csv")

# Further preprocessing on WL2
WL2 <- WL2 %>% 
  filter(!(participant_external_id == "TMS039" & session_label %in% c("V2", "V3", "V4")),
         !(participant_external_id == "TMS039b" & session_label == "V1")) %>%
  mutate(participant_external_id = ifelse(session_label %in% c("V2", "V3") & participant_external_id == "TMS039b", "TMS039", participant_external_id))

# Merging and creating participant_group column
WL <- rbind(WL, WL2) %>%
  mutate(participant_group = factor(ifelse(grepl("(^CTC|C_CTC|CTB)", participant_external_id), "Control", 
                                           ifelse(grepl("^(TMS|MDD)", participant_external_id), "MDD", NA))),
         testing_location = as.factor(ifelse(participant_external_id %in% remote_participants, "remote", "in-person")),
         session_label = case_when(
           session_label == "Baseline" ~ "V1",
           session_label == "Week 2"   ~ "V2",
           session_label == "Week 6"   ~ "V3",
           TRUE                        ~ session_label  # Default case
         ))

# Filter journaling task and baseline session
WL_Jou <- WL %>% filter(task_name == "journaling", session_label == "V1")

# Calculate average for numeric columns and mode for categorical columns using custom getmode function
WL_Jou_BL_avg <- WL_Jou %>%
  group_by(participant_external_id) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)), 
            across(where(is.factor), getmode))


# Merge demographics and clinical data

#Read in and merge demographics data 
demoMDD <- read_excel("~/Lab/Winterlight/TMS_CTRL_Demographics.xlsx", sheet=1 )
names(demoMDD)[1] <- "participant_external_id"
demoCTRL <- read_excel("~/Lab/Winterlight/TMS_CTRL_Demographics.xlsx", sheet=2)
names(demoCTRL)[1] <- "participant_external_id"
# Change the values in the sex column of demoCTRL
demoCTRL$sex[demoCTRL$sex == 0] <- "F"
demoCTRL$sex[demoCTRL$sex == 1] <- "M"
column_mapping <- c(
  "qids_total_pre" = "bl_qids_tot",
  "qids_1_pre" = "bl_qids_sr_1",
  "qids_2_pre" = "bl_qids_sr_2",
  "qids_3_pre" = "bl_qids_sr_3",
  "qids_4_pre" = "bl_qids_sr_4",
  "qids_5_pre" = "bl_qids_sr_5",
  "qids_6_pre" = "bl_qids_sr_6",
  "qids_7_pre" = "bl_qids_sr_7",
  "qids_8_pre" = "bl_qids_sr_8",
  "qids_9_pre" = "bl_qids_sr_9",
  "qids_10_pre" = "bl_qids_sr_10",
  "qids_11_pre" = "bl_qids_sr_11",
  "qids_12_pre" = "bl_qids_sr_12",
  "qids_13_pre" = "bl_qids_sr_13",
  "qids_14_pre" = "bl_qids_sr_14",
  "qids_15_pre" = "bl_qids_sr_15",
  "qids_16_pre" = "bl_qids_sr_16",
  "qids_total_wk2" = "2wk_qids_tot",
  "qids_1_wk2" = "2wk_qids_sr_1",
  "qids_2_wk2" = "2wk_qids_sr_2",
  "qids_3_wk2" = "2wk_qids_sr_3",
  "qids_4_wk2" = "2wk_qids_sr_4",
  "qids_5_wk2" = "2wk_qids_sr_5",
  "qids_6_wk2" = "2wk_qids_sr_6",
  "qids_7_wk2" = "2wk_qids_sr_7",
  "qids_8_wk2" = "2wk_qids_sr_8",
  "qids_9_wk2" = "2wk_qids_sr_9",
  "qids_10_wk2" = "2wk_qids_sr_10",
  "qids_11_wk2" = "2wk_qids_sr_11",
  "qids_12_wk2" = "2wk_qids_sr_12",
  "qids_13_wk2" = "2wk_qids_sr_13",
  "qids_14_wk2" = "2wk_qids_sr_14",
  "qids_15_wk2" = "2wk_qids_sr_15",
  "qids_16_wk2" = "2wk_qids_sr_16",
  "qids_total_wk4" = "4wk_qids_tot",
  "qids_1_wk4" = "4wk_qids_sr_1",
  "qids_2_wk4" = "4wk_qids_sr_2",
  "qids_3_wk4" = "4wk_qids_sr_3",
  "qids_4_wk4" = "4wk_qids_sr_4",
  "qids_5_wk4" = "4wk_qids_sr_5",
  "qids_6_wk4" = "4wk_qids_sr_6",
  "qids_7_wk4" = "4wk_qids_sr_7",
  "qids_8_wk4" = "4wk_qids_sr_8",
  "qids_9_wk4" = "4wk_qids_sr_9",
  "qids_10_wk4" = "4wk_qids_sr_10",
  "qids_11_wk4" = "4wk_qids_sr_11",
  "qids_12_wk4" = "4wk_qids_sr_12",
  "qids_13_wk4" = "4wk_qids_sr_13",
  "qids_14_wk4" = "4wk_qids_sr_14",
  "qids_15_wk4" = "4wk_qids_sr_15",
  "qids_16_wk4" = "4wk_qids_sr_16",
  "gad7_total_pre" = "bl_gad7_tot",
  "gad7_1_pre" = "bl_gad7_1",
  "gad7_2_pre" = "bl_gad7_2",
  "gad7_3_pre" = "bl_gad7_3",
  "gad7_4_pre" = "bl_gad7_4",
  "gad7_5_pre" = "bl_gad7_5",
  "gad7_6_pre" = "bl_gad7_6",
  "gad7_7_pre" = "bl_gad7_7",
  "gad7_8_pre" = "bl_gad7_8",
  "gad7_total_wk2" = "2wk_gad7_tot",
  "gad7_1_wk2" = "2wk_gad7_1",
  "gad7_2_wk2" = "2wk_gad7_2",
  "gad7_3_wk2" = "2wk_gad7_3",
  "gad7_4_wk2" = "2wk_gad7_4",
  "gad7_5_wk2" = "2wk_gad7_5",
  "gad7_6_wk2" = "2wk_gad7_6",
  "gad7_7_wk2" = "2wk_gad7_7",
  "gad7_8_wk2" = "2wk_gad7_8",
  "gad7_total_wk4" = "4wk_gad7_tot",
  "gad7_1_wk4" = "4wk_gad7_1",
  "gad7_2_wk4" = "4wk_gad7_2",
  "gad7_3_wk4" = "4wk_gad7_3",
  "gad7_4_wk4" = "4wk_gad7_4",
  "gad7_5_wk4" = "4wk_gad7_5",
  "gad7_6_wk4" = "4wk_gad7_6",
  "gad7_7_wk4" = "4wk_gad7_7",
  "gad7_8_wk4" = "4wk_gad7_8")

demoCTRL <- demoCTRL %>%
  rename(!!!column_mapping)

demoALL <- bind_rows(demoMDD, demoCTRL)

# Merge with WL_Jou_BL_avg
Jou_demo <- merge(WL_Jou_BL_avg, demoALL, by = "participant_external_id", all = TRUE)

# Load psychiatry data and filter
psych <- read_csv("~/Lab/Winterlight/PsychiatryData_for_RabinLab.csv") %>%
  filter(participant_external_id %in% Jou_demo$participant_external_id)

# Merge datasets and coalesce NA values
merged_data <- full_join(Jou_demo, psych, by = "participant_external_id", suffix = c("", ".psych")) %>%
  mutate(across(common_columns, ~ coalesce(.x, get(paste0(cur_column(), ".psych")))))

# Remove the psych columns
Jou_demo_psych <- select(merged_data, -ends_with(".psych"))

# Plotting Histograms
plot_histogram <- function(data, column, title) {
  ggplot(data, aes(x = {{ column }})) + 
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    theme_minimal() +
    labs(title = title)
}

plot_histogram(Jou_demo_psych, hamd17_total_pre, "Distribution of HAMD Scores")
plot_histogram(Jou_demo_psych, qids_total_pre, "Distribution of QIDS Scores")
plot_histogram(Jou_demo_psych %>% filter(participant_group == 'MDD'), qids_total_pre, "QIDS Scores for MDD Group")
plot_histogram(Jou_demo_psych %>% filter(participant_group == 'Control'), qids_total_pre, "QIDS Scores for Control Group")


#STEP 2 PREPROCESS FOR PCA

# Select speech variables for PCA (assuming they are in specific columns)
speech_data <- Jou_demo_psych %>% select(c("participant_external_id", 10:768)) 
# Loop over each column in the dataframe
for (col in names(speech_data)) {
  # Check if the column is numeric
  if (is.numeric(speech_data[[col]])) {
    # Replace NaN with NA in numeric columns
    speech_data[[col]][is.nan(speech_data[[col]])] <- NA
  }
}

# Remove rows with all NA values in columns other than the specified ones
speech_data_clean <- speech_data[!apply(speech_data[, colnames(speech_data) != c("participant_external_id","participant_group")], 1, function(row) all(is.na(row))), ]
excluded_database <- speech_data[apply(speech_data[, colnames(speech_data) != c("participant_external_id","participant_group")], 1, function(row) all(is.na(row))), ]
excluded_database <- excluded_database[, c("participant_external_id")]

length(speech_data_clean$participant_group[speech_data_clean$participant_group == "MDD"])
length(speech_data_clean$participant_group[speech_data_clean$participant_group == "Control"])

# Print the columns where most values are NA
colnames(speech_data_clean)[colMeans(is.na(speech_data_clean)) >= 0.7]
# Take out features where the most values are NA 
speech_data_clean <- speech_data_clean[, colMeans(is.na(speech_data_clean)) < 0.7]
# Find and print the column names with all values equal to 0 or NA
zero_or_na_cols <- colnames(speech_data_clean)[apply(speech_data_clean, 2, function(col) all(is.na(col) | col == 0))]
cat("Columns with all values equal to 0 or NA:", zero_or_na_cols, "\n")
speech_data_clean <-  speech_data_clean[!apply(speech_data_clean, 2, function(col) all(is.na(col) | col == 0))]
# Select numeric columns
numeric_cols <- names(speech_data_clean)[sapply(speech_data_clean, is.numeric)]
# Calculate the variance for numeric columns
variance <- apply(speech_data_clean[, numeric_cols, drop = FALSE], 2, function(x) var(x, na.rm = TRUE))
# Filter columns with variance less than or equal to 0.001
selected_columns <- names(variance[variance == 0])
speech_data_clean <- speech_data_clean[,!names(speech_data_clean) %in% selected_columns]


speech_data_pca <- speech_data_clean %>% select(2:560) 
non_numeric_cols <- names(speech_data_pca)[sapply(speech_data_pca, function(x) !is.numeric(x))]
non_numeric_cols
speech_data_scaled <- as.data.frame(scale(speech_data_pca, center = TRUE, scale = TRUE))

### Extra check to get rid of columns with mostly NAs 
# Check for missing values in the dataset
missing_values <- colSums(is.na(speech_data_scaled))
# Calculate the percentage of missing values for each column
missing_percentage <- (missing_values / nrow(speech_data_scaled)) * 100
# Sort the percentages in descending order
missing_percentage_sorted <- sort(missing_percentage, decreasing = TRUE)
# Display the top 10 columns with the highest percentage of missing values
head(missing_percentage_sorted, 10)
# Define a threshold for the maximum allowed percentage of missing values
threshold <- 50.0
# Identify columns with missing value percentage greater than the threshold
columns_to_drop <- names(missing_percentage[missing_percentage > threshold])
# Drop columns with missing value percentage greater than the threshold
speech_data_scaled <- speech_data_scaled[, !(names(speech_data_scaled) %in% columns_to_drop)]

# Impute the remaining missing values with the mean of each column
# Note: This only works for numerical columns
numerical_columns <- names(speech_data_scaled)[sapply(speech_data_scaled, is.numeric)]
# Apply function only to numeric columns
speech_data_scaled[numerical_columns] <- lapply(speech_data_scaled[numerical_columns], function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})

# Convert the list back to a dataframe if necessary
speech_data_scaled <- as.data.frame(speech_data_scaled)
# Verify that there are no more missing values
missing_values_after_imputation <- sum(is.na(speech_data_scaled))


# Checking for infinite values in the dataset
# Replace infinite values with NA
speech_data_scaled[speech_data_scaled == Inf | speech_data_scaled == -Inf] <- NA
infinite_values <- colSums(is.na(speech_data_scaled))
infinite_columns <- names(infinite_values[infinite_values > 0])
# Re-impute NA values with the mean of each column, excluding the NAs in the calculation
speech_data_scaled <- as.data.frame(lapply(speech_data_scaled, function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  return(x)
}))
# Check again for infinite values after re-imputation
infinite_values_after_reimputation <- colSums(as.matrix(sapply(speech_data_scaled, is.infinite)))



#STEP 3 PERFORM PCA 

library(FactoMineR)
pca_result <- PCA(speech_data_scaled, scale.unit = TRUE, ncp = 10, graph = FALSE)

#STEP 4 MERGE PCA SCORES WITH CLINICAL DATA 

# Extract PCA scores
pca_scores <- as.data.frame(pca_result$ind$coord)
# examine loadings 
loadings <- pca_result$var$coord
# Apply absolute value to the loadings
# Apply absolute value to the loadings
abs_loadings <- abs(loadings)

# Initialize a list to store sorted loadings along with variable names for each dimension
sorted_loadings_with_names <- list()

# Loop through each dimension, pair loadings with variable names, and then sort
for(i in 1:ncol(abs_loadings)) {
  # Pairing loadings with variable names
  named_loadings <- setNames(abs_loadings[,i], rownames(abs_loadings))
  
  # Sorting
  sorted_loadings_with_names[[paste0("Dimension", i)]] <- sort(named_loadings, decreasing = TRUE)
}

## inspect sorted loadings 


fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))
# Merge PCA scores with clinical data (assuming clinical measures are still in 'merged_data')
Jou_demo_psych_filtered <- Jou_demo_psych[Jou_demo_psych$participant_external_id %in% speech_data_clean$participant_external_id, ]
# Bind the pca_scores with the filtered merged_data
final_data <- cbind(pca_scores, Jou_demo_psych_filtered)

#STEP 5 LINEAR REGRESSION ANALYSIS 

clinical_vars <- c("qids_total_pre", "qids_1_pre", "qids_2_pre","qids_3_pre", "qids_4_pre","qids_5_pre",
                   "qids_6_pre","qids_7_pre", "qids_8_pre","qids_9_pre","qids_10_pre", "qids_11_pre","qids_12_pre",
                   "qids_13_pre", "qids_14_pre", "hamd17_total_pre","hamd17_1_pre","hamd17_2_pre",
                   "hamd17_3_pre", "hamd17_4_pre","hamd17_5_pre","hamd17_6_pre",
                   "hamd17_7_pre","hamd17_8_pre","hamd17_9_pre","hamd17_10_pre","hamd17_11_pre","hamd17_12_pre",
                   "hamd17_13_pre","hamd17_14_pre","hamd17_15_pre","hamd17_16_pre",
                   "hamd17_anxiety_pre","hamd17_depression_pre","hamd17_insomnia_pre")  

qids_vars <- c("qids_total_pre", "qids_1_pre", "qids_2_pre","qids_3_pre", "qids_4_pre","qids_5_pre",
               "qids_6_pre","qids_7_pre", "qids_8_pre","qids_9_pre","qids_10_pre", "qids_11_pre","qids_12_pre",
               "qids_13_pre", "qids_14_pre")

mdd_participants <- final_data[final_data$participant_group == "MDD",]

# library(Hmisc)  # For imputation
# 
# # Impute missing values in clinical variables
# for (var in clinical_vars) {
#   final_data[[var]] <- with(final_data, impute(final_data[[var]], mean))
# }

# Rest of your code remains the same

# Number of PCA dimensions
num_pca_dimensions <- ncol(pca_scores)
# Initialize a list to store regression results
regression_results <- list()
# Loop through each PCA dimension
for (i in 1:num_pca_dimensions) {
  # Loop through each clinical variable
  for (clinical_var in qids_vars) {
    # Construct the regression formula
    formula <- as.formula(paste(paste0("Dim.", i), "~",clinical_var, "+ age_screening + sex"))
    # Perform linear regression
    model <- lm(formula, data = final_data)
    # Store or print the summary of the model
    regression_results[[paste("PC", i, "_", clinical_var)]] <- summary(model)
  }
}


for (i in 1:num_pca_dimensions) {
  # Loop through each clinical variable
  for (clinical_var in clinical_vars) {
    # Construct the regression formula
    formula <- as.formula(paste(paste0("Dim.", i), "~", clinical_var, "+ age_screening + sex"))
    # Perform linear regression
    model <- lm(formula, data = mdd_participants)
    # Store or print the summary of the model
    regression_results[[paste("PC", i, "_", clinical_var)]] <- summary(model)
  }
}

print(regression_results)


# Function to check if the p-value of the speech variable coefficient is significant
coefficient_index <- 2  # Adjust this based on your specific coefficient's position
# Loop through the list with an index to access names
significant_results <-list()
for (i in seq_along(regression_results)) {
  model_summary <- regression_results[[i]]
  coefficients_table <- model_summary$coefficients
  
  # Check if the p-value for the specific coefficient is less than 0.05
  if (coefficients_table[coefficient_index, "Pr(>|t|)"] < 0.05) {
    # Print the name of the model
    cat("Model Name:", names(regression_results)[i], "\n")
    
    # Print the model summary
    print(model_summary)
    significant_results<-c(significant_results, model_summary)
  }
}


# Function to check if the p-value of the speech variable coefficient is significant
coefficient_index <- 2  # Adjust this based on your specific coefficient's position
# Loop through the list with an index to access names
for (i in seq_along(regression_results)) {
  model_summary <- regression_results[[i]]
  coefficients_table <- model_summary$coefficients
  
  # Check if the p-value for the specific coefficient is less than 0.05
  if (abs(coefficients_table[coefficient_index, "Estimate"]) > 1) {
    # Print the name of the model
    cat("Model Name:", names(regression_results)[i], "\n")
    
    # Print the model summary
    print(model_summary)
  }
}



library(ggplot2)

# Assuming significant_results contains your significant linear models
for (model_name in names(significant_results)) {
  model <- significant_results[[model_name]]
  # Extracting the name of the clinical variable and the PC number from the model_name
  split_names <- strsplit(model_name, "_")[[1]]
  var_name <- paste(head(split_names, -2), collapse = "_")  
  pc_number <- strsplit(model_name, "_")[[1]][length(strsplit(model_name, "_")[[1]])]
  # Ensure the PC column names are correct
  pc_column_name <- paste0("Dim.", pc_number)  # Adjust according to actual PC column names
  # Creating the plot
  plot <- ggplot(final_data, aes(x = !!sym(pc_column_name), y = !!sym(var_name))) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = paste("Relationship between", var_name, "and PC", pc_number),
         x = paste("Principal Component", pc_number),
         y = var_name)
  
  print(plot)
}



## for each dimension - which variable falls under different categories

abs_loadings <- abs(pca_result$var$coord)  # Using absolute values of loadings
#determine sig loadings 
threshold <- 0.3
significant_loadings <- abs_loadings > threshold

# Define patterns in a named list
patterns <- list(
  acoustic = "intensity|jitter|shimmer|mfcc|zcr|fundamental_frequency|hnr",
  discourse_mapping = "cos|graph",
  lexical_richness = "MATTR|frequency|familiarity|imageability|age_of_acquisition|pos|utt_len|tag|category|pos",
  local_coherence = "local_coherence|cos",
  sentiment = "sentiment",
  syntactic_complexity = "ADJP|FRAG|Lu|NP|PP|PRT|ROOT|SBAR|S_|VP|WHADVP|WHNP|constituency|morph",
  word_finding = "pause|rate"
)

# Initialize categories
columns_by_category <- lapply(patterns, function(x) character())

# Categorize variables
for (col_name in rownames(significant_loadings)) {
  for (category in names(patterns)) {
    if (grepl(patterns[[category]], col_name)) {
      columns_by_category[[category]] <- c(columns_by_category[[category]], col_name)
    }
  }
}

## clean up categorization (remove f0 from lexical richness and the tag categorical variables
#add other variables)
columns_by_category$lexical_richness <- setdiff(columns_by_category$lexical_richness, "fundamental_frequency")
additional_lexical_vars <- c("avg_word_duration", "brunet", "honore", "prp_ratio", "sub_coord_ratio", "avg_word_length")
columns_by_category$lexical_richness <- c(columns_by_category$lexical_richness, additional_lexical_vars)
columns_by_category$local_coherence <- c(columns_by_category$local_coherence, "temporal_cohesion_avg_switches_in_tense")
additional_syntactic_vars <- c("noun_ratio", "nv_ratio", "propositional_density_ratio_propositions",
                               "temporal_cohesion_avg_switches_in_tense", "TTR")
columns_by_category$syntactic_complexity <- c(columns_by_category$syntactic_complexity,additional_syntactic_vars)
additional_wordfinding_vars <- c("total_duration_audio", "total_duration_speech",
                                 "total_words", "uh", "um", "MLU", "NID", "INTJ_.._UH", "hesitation")
columns_by_category$word_finding <- c(columns_by_category$word_finding, additional_wordfinding_vars)



num_categories <- length(columns_by_category)
num_pcs <- ncol(loadings)
# Initialize an empty matrix
proportion_table <- matrix(NA, nrow = num_categories, ncol = num_pcs)
rownames(proportion_table) <- names(columns_by_category)
colnames(proportion_table) <- paste("PC", 1:num_pcs, sep = "")


for (cat_name in names(columns_by_category)) {
  for (pc in 1:num_pcs) {
    # Extract variables in the category and their loadings in this PC
    cat_vars <- intersect(columns_by_category[[cat_name]], rownames(significant_loadings))    
    cat_loadings <- significant_loadings[cat_vars, pc]

    # Calculate the proportion of significant variables
    num_significant <- sum(cat_loadings, na.rm = TRUE)
    total_vars_in_category <- length(cat_vars)
    proportion_table[cat_name, pc] <- (num_significant / total_vars_in_category) * 100
  }
}

proportion_table[is.na(proportion_table)] <- 0


# Optional: Create a heatmap of the contingency table
heatmap(as.matrix(proportion_table), 
        Rowv = NA, Colv = NA, 
        col = heat.colors(256), 
        scale = "column", 
        margins = c(5,10))

# Optional: Create a bar plot
bar_data <- as.data.frame(proportion_table)
bar_data$Category <- rownames(bar_data)

long_data <- reshape2::melt(bar_data, id.vars = "Category")

ggplot(long_data, aes(x = variable, y = value, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Principal Component", y = "Proportion of High Loading Variables") +
  theme_minimal()


ggplot(long_data, aes(x = Category, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Category", y = "Proportion of High Loading Variables") +
  theme_minimal()
