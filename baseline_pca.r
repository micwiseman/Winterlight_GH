library(tidyverse)
library(ggplot2)
library(ggpubr)
library(readxl)
library(dplyr)
library(broom)
library(tidyr)


WL <- read.csv("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook_rTMS_2023_11_03.csv")
WL <- WL[grep("^(TMS|MDD)", WL$participant_external_id), ]
WL2 <- read.csv("~/Lab/Winterlight/WINTERLIGHT_Sunnybrook_rTMSremote_2023_11_03.csv")
WL2 <- WL2[!(WL2$participant_external_id == "TMS039" & WL2$session_label %in% c("V2", "V3", "V4")), ]
WL2 <- WL2[!(WL2$participant_external_id == "TMS039b" & WL2$session_label == "V1"), ]
WL2$participant_external_id[WL2$session_label %in% c("V2", "V3") & WL2$participant_external_id == "TMS039b"] <- "TMS039"
WL <- rbind(WL,WL2)
# Create a new column participant_group and assign it a value based on participant_external_id
WL$participant_group <- factor(
  ifelse(
    grepl("(^CTC|C_CTC|CTB)", WL$participant_external_id), "Control",
    ifelse(grepl("^(TMS|MDD)", WL$participant_external_id), "MDD", NA)
  )
)
WL <- WL[!WL$participant_external_id == "CTC036",]

## Exclude controls with sig qids
#WL <- WL[!(WL$participant_external_id == c("CTC006", "CTC039", "CTC042", "CTC045")), ]


# Define a vector of participant_external_id values that correspond to remote testing
remote_participants <- c("CTC001", "CTC015", "CTC021", "CTC028", "CTC013", "CTC030", 
                         "CTC034", "CTC036", "CTC045", "TMS038", "TMS040", "TMS041", "TMS042", "TMS043", 
                         "TMS044", "TMS045", "TMS046", "TMS048", "TMS049", "TMS050", "TMS051")
# Add a new column testing_location and assign values based on participant_external_id
WL$testing_location <- as.factor(ifelse(WL$participant_external_id %in% remote_participants, "remote", "in-person"))


# Replace values in session_label column
WL$session_label <- gsub("Baseline", "V1", WL$session_label)
WL$session_label <- gsub("Week 2", "V2", WL$session_label)
WL$session_label <- gsub("Week 6", "V3", WL$session_label)

WL_Jou <- WL %>%
  filter(task_name == "journaling")
WL_Jou_BL <- WL_Jou %>%
  filter(session_label=="V1")

# Helper function to calculate mode (most common level of a factor)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate average for numeric columns and mode for categorical columns
WL_Jou_BL_avg <- WL_Jou_BL %>%
  group_by(participant_external_id) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)), 
            across(where(is.factor), getmode))

length(WL_Jou_BL_avg$participant_group[WL_Jou_BL_avg$participant_group == "MDD"])
length(WL_Jou_BL_avg$participant_group[WL_Jou_BL_avg$participant_group == "Control"])


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
Jou_demo <- merge(WL_Jou_BL_avg, demoALL, by = "participant_external_id", all.x=TRUE)

#merge HAMD results 
psych <- read_csv("~/Lab/Winterlight/PsychiatryData_for_RabinLab.csv")
#psych <- psych %>% rename(participant_external_id = record_id)
psych <- psych %>%
  filter(participant_external_id %in% Jou_demo$participant_external_id)
# Merge the datasets based on "participant_external_id" and retain all columns
merged_data <- full_join(Jou_demo, psych, by = "participant_external_id", suffix = c("", ".psych"))
# Loop through the columns and update NA values in wl_jou_bl with values from psych
common_columns <- setdiff(intersect(names(Jou_demo), names(psych)), "participant_external_id")
for (col in common_columns) {
  merged_data[[col]] <- ifelse(is.na(merged_data[[col]]), merged_data[[paste0(col, ".psych")]], merged_data[[col]])
}
# Remove the psych columns
Jou_demo_psych <- merged_data %>% select(-one_of(paste0(common_columns, ".psych")))


ggplot(Jou_demo_psych, aes(x = hamd17_total_pre)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of HAMD Scores")

ggplot(Jou_demo_psych, aes(x = qids_total_pre)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of QIDS Scores")

  # Filter for participant_group 'MDD' and then plot histogram of qids_total_pre
Jou_demo_psych %>%
  filter(participant_group == 'MDD') %>%
  ggplot(aes(x = qids_total_pre)) + 
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    theme_minimal() +
    labs(title = "QIDS Scores for MDD Group")

# Filter for participant_group 'MDD' and then plot histogram of qids_total_pre
Jou_demo_psych %>%
  filter(participant_group == 'Control') %>%
  ggplot(aes(x = qids_total_pre)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "QIDS Scores for Control Group")


# condense the data set so it only includes rows
# necessary for PCA 
Jou_FA <- Jou_demo_psych %>%
  select(participant_external_id, participant_group, testing_location,
         10:767, 
         qids_total_pre, hamd17_total_pre,
         806:821, 
         1045:1061, 
         1064:1070)

# Define the columns you want to exclude from the check for NA values
exclude_columns <- c("participant_external_id", "X","participant_group", "sample_id", "testing_location", "task_name", "session_label", "sample_datetime_completed_utc", "stimulus_filename","terminal_state")
# Remove rows with all NA values in columns other than the specified ones
Jou_FA_clean <- Jou_FA[!apply(Jou_FA[, !(colnames(Jou_FA) %in% exclude_columns)], 1, function(row) all(is.na(row))), ]
excluded_database <- Jou_FA[apply(Jou_FA[, !(colnames(Jou_FA) %in% exclude_columns)], 1, function(row) all(is.na(row))), ]
excluded_database <- excluded_database[, c("participant_external_id")]

length(Jou_FA_clean$participant_group[Jou_FA_clean$participant_group == "MDD"])
length(Jou_FA_clean$participant_group[Jou_FA_clean$participant_group == "Control"])

# Print the columns where most values are NA
colnames(Jou_FA_clean)[colMeans(is.na(Jou_FA_clean)) >= 0.7]
# Take out features where the most values are NA 
Jou_FA_clean <- Jou_FA_clean[, colMeans(is.na(Jou_FA_clean)) < 0.7]
# Find and print the column names with all values equal to 0 or NA
zero_or_na_cols <- colnames(Jou_FA_clean)[apply(Jou_FA_clean, 2, function(col) all(is.na(col) | col == 0))]
cat("Columns with all values equal to 0 or NA:", zero_or_na_cols, "\n")
Jou_FA_clean <-  Jou_FA_clean[!apply(Jou_FA_clean, 2, function(col) all(is.na(col) | col == 0))]
# Select numeric columns
numeric_cols <- names(Jou_FA_clean)[sapply(Jou_FA_clean, is.numeric)]
# Calculate the variance for numeric columns
variance <- apply(Jou_FA_clean[, numeric_cols, drop = FALSE], 2, function(x) var(x, na.rm = TRUE))
# Filter columns with variance less than or equal to 0.001
selected_columns <- names(variance[variance == 0])
Jou_FA_clean <- Jou_FA_clean[,!names(Jou_FA_clean) %in% selected_columns]



library(missMDA)
library(FactoMineR)

#Just MDD patients 

Jou_FA_MDD <- Jou_FA_clean %>%
  filter(participant_group == 'MDD')


#participant external id is unique to each participant so it is not useful for PCA and there is only one value for participant group
# so we drop it 

Jou_FA_MDD <- Jou_FA_MDD %>% select(-participant_external_id, -participant_group)

# Convert the columns you want to one-hot encode to factors if not already
Jou_FA_MDD$testing_location <- as.factor(Jou_FA_MDD$testing_location)

# Use dummyVars to create the one-hot encoded variables
dummies <- dummyVars(~ testing_location, data = Jou_FA_MDD)
encoded_data <- predict(dummies, newdata = Jou_FA_MDD)

# Bind the encoded data to the non-encoded columns
non_encoded_cols <- Jou_FA_MDD[, !(names(Jou_FA_MDD) %in% "testing_location")]
Jou_FA_MDD <- cbind(non_encoded_cols, encoded_data)


### Extra check to get rid of columns with mostly NAs 
# Check for missing values in the dataset
missing_values <- colSums(is.na(Jou_FA_MDD))
# Calculate the percentage of missing values for each column
missing_percentage <- (missing_values / nrow(Jou_FA_MDD)) * 100
# Sort the percentages in descending order
missing_percentage_sorted <- sort(missing_percentage, decreasing = TRUE)
# Display the top 10 columns with the highest percentage of missing values
head(missing_percentage_sorted, 10)
# Define a threshold for the maximum allowed percentage of missing values
threshold <- 50.0
# Identify columns with missing value percentage greater than the threshold
columns_to_drop <- names(missing_percentage[missing_percentage > threshold])
# Drop columns with missing value percentage greater than the threshold
Jou_FA_MDD <- Jou_FA_MDD[, !(names(Jou_FA_MDD) %in% columns_to_drop)]

# Impute the remaining missing values with the mean of each column
# Note: This only works for numerical columns
numerical_columns <- sapply(Jou_FA_MDD, is.numeric)
Jou_FA_MDD[numerical_columns] <- lapply(Jou_FA_MDD[numerical_columns], function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})

# Verify that there are no more missing values
missing_values_after_imputation <- sum(is.na(Jou_FA_MDD))


# Checking for infinite values in the dataset
# Replace infinite values with NA
Jou_FA_MDD[Jou_FA_MDD == Inf | Jou_FA_MDD == -Inf] <- NA
infinite_values <- colSums(is.na(Jou_FA_MDD))
infinite_columns <- names(infinite_values[infinite_values > 0])
## Honore and sub_coord_ratio have infinite values so we reimpute them 
# Re-impute NA values with the mean of each column, excluding the NAs in the calculation
Jou_FA_MDD <- as.data.frame(lapply(Jou_FA_MDD, function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  return(x)
}))
# Check again for infinite values after re-imputation
infinite_values_after_reimputation <- colSums(sapply(Jou_FA_MDD, is.infinite))


# Scale the data
preProcValues <- preProcess(Jou_FA_MDD, method = c("center", "scale"))
## warning that These variables have zero variances: morph_num_type_mult, morph_verb_form_ger
# so we remove 
Jou_FA_MDD <- Jou_FA_MDD[, !c(Jou_FA_MDD$morph_num_type_mult,Jou_FA_MDD$morph_verb_form_ger)]
data_scaled <- predict(preProcValues, Jou_FA_MDD)

# Perform PCA
pca_result <- PCA(data_scaled, ncp = 10, graph = FALSE)
library(factoextra)

# Print summary of PCA
print(summary(pca_result))
# Scree Plot Look for the point where the plot starts to flatten out (the 'elbow'), 
# which suggests that additional components contribute less to explaining the variance. Components before this point are generally considered significant.
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))

## 6 dimensions look good 

# Biplot: Points or vectors close to each other are similar. 
#Vectors pointing in the same direction indicate positive correlation, while those in opposite directions indicate negative correlation. 
# The length of a vector indicates the strength of the variable in the component.
plot(fviz_pca_biplot(pca_result, repel = TRUE))  # repel=TRUE helps to avoid text overlapping
plot(fviz_pca_var(pca_result, col.var = "contrib"))
# This can be used for clustering analysis.
fviz_pca_ind(pca_result)
# Plot of variables: Helps in understanding which variables have the most influence on each component.
fviz_pca_var(pca_result)


# Assuming pca_result is your PCA object from FactoMineR
loadings <- pca_result$var$coord  # Extract loadings
high_loadings <- apply(loadings, 2, function(x) names(x)[abs(x) > 0.3])


## which dimensions do the HAMD and qids items have the highest loadings 
# List of specific variables
clinical_vars <- c("qids_total_pre", "qids_1_pre", "qids_2_pre","qids_3_pre", "qids_4_pre","qids_5_pre",
                   "qids_6_pre","qids_7_pre", "qids_8_pre","qids_9_pre","qids_10_pre", "qids_11_pre","qids_12_pre",
                   "qids_13_pre", "qids_14_pre", "hamd17_total_pre","hamd17_1_pre","hamd17_2_pre",
                   "hamd17_3_pre", "hamd17_4_pre","hamd17_5_pre","hamd17_6_pre",
                   "hamd17_7_pre","hamd17_8_pre","hamd17_9_pre","hamd17_10_pre","hamd17_11_pre","hamd17_12_pre",
                   "hamd17_13_pre","hamd17_14_pre","hamd17_15_pre","hamd17_16_pre",
                   "hamd17_anxiety_pre","hamd17_depression_pre","hamd17_insomnia_pre")  

# Loop through each variable and find the PC it loads highest on
for (var in clinical_vars) {
  if (var %in% rownames(loadings)) {
    # Perform the operation only if the variable is in loadings
    variable_loadings <- loadings[var, ]
    max_loading_index <- which.max(abs(variable_loadings))
    max_loading_value <- variable_loadings[max_loading_index]
    
    # Print the results
    cat("The variable", var, "has the highest loading on PC", max_loading_index, 
        "with a value of", max_loading_value, "\n")
  } else {
    # Inform if the variable is not in loadings
    cat("The variable", var, "does not have a match in loadings\n")
  }
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


# Identify clinical variables
clin <- setdiff(rownames(high_loadings), unlist(columns_by_category))
columns_by_category$clinical <- clin



num_categories <- length(columns_by_category)
num_pcs <- ncol(loadings)
# Initialize an empty matrix
proportion_table <- matrix(NA, nrow = num_categories, ncol = num_pcs)
rownames(proportion_table) <- names(columns_by_category)
colnames(proportion_table) <- paste("PC", 1:num_pcs, sep = "")


for (cat_name in names(columns_by_category)) {
  for (pc in 1:num_pcs) {
    # Extract variables in the category and their loadings in this PC
    cat_vars <- columns_by_category[[cat_name]]
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
  labs(x = "Principal Component", y = "Percent of High Loading Variables") +
  theme_minimal()


ggplot(long_data, aes(x = Category, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Category", y = "Percent of High Loading Variables") +
  theme_minimal()



