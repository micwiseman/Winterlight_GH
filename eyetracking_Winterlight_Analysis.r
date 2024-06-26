# Install the here package (if haven't already)
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)
source(here("WL_helperfuncs.r"), encoding = "UTF-8")

# Load the data

# Read and preprocess speech datasets
library(readxl)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# Read datasets
# Read and preprocess speech data
WL <- speech_read_and_preprocess(here("WINTERLIGHT_Sunnybrook_rTMS_2023_11_03.csv"))
WL_2 <- speech_read_and_preprocess(here("WINTERLIGHT_Sunnybrook_rTMSremote_2024_03_28.csv"))

# Apply filters
WL <- WL[grep("^(TMS|MDD)", WL$participant_external_id), ]
WL_2 <- WL_2[!(WL_2$participant_external_id == "TMS039" & WL_2$session_label %in% c("V2", "V3", "V4")), ]
WL_2 <- WL_2[!(WL_2$participant_external_id == "TMS039b" & WL_2$session_label == "V1"), ]
WL_2$participant_external_id[WL_2$session_label %in% c("V2", "V3") & WL_2$participant_external_id == "TMS039b"] <- "TMS039"


# Combine datasets
common_columns <- intersect(names(WL), names(WL_2))
WL_common <- WL[, common_columns]
WL_2_common <- WL_2[, common_columns]
WL_combined <- rbind(WL_common, WL_2_common)

# Processing participant_group
WL_combined$participant_group <- factor(
  ifelse(
    grepl("(^CTC|C_CTC)", WL_combined$participant_external_id), "Control",
    ifelse(grepl("^(TMS|MDD|MFB)", WL_combined$participant_external_id), "MDD", NA)
  )
)

## Remove participants with high QIDS
WL_combined <- WL_combined[WL_combined$participant_external_id != "CTC036" &
                          WL_combined$participant_external_id != "CTC004" &
                          WL_combined$participant_external_id != "CTC006" &
                          WL_combined$participant_external_id != "CTC017" &
                          WL_combined$participant_external_id != "CTC030" &
                          WL_combined$participant_external_id != "CTC039" &
                          WL_combined$participant_external_id != "CTC043" &
                          WL_combined$participant_external_id != "CTC045" &
                          WL_combined$participant_external_id != "CTC053" &
                          WL_combined$participant_external_id != "CTC052" &
                          WL_combined$participant_external_id != "CTC058" &
                          WL_combined$participant_external_id != "CTC058_new" &
                          WL_combined$participant_external_id != "CTC023" &
                          WL_combined$participant_external_id != "CTB001" &
                          WL_combined$participant_external_id != "CTC063" &
                          WL_combined$participant_external_id != "CTC076" , ]

# Define remote participants and assign testing location
remote_participants <- c(
    "CTC001", "CTC015", "CTC021", "CTC028", "CTC013", "CTC030",
    "CTC034", "CTC036", "CTC045", "TMS038")

# Function to identify CTC046 and higher
is_ctc046_or_higher <- function(id) {
  if (grepl("^CTC", id)) {
    # Extract the numeric part of the ID and check if it is 46 or higher
    num_part <- as.numeric(sub("^CTC", "", id))
    return(num_part >= 46)
  }
  FALSE
}

# Apply the function to all participants and get those that are CTC046 or higher
ctc046_or_higher <- sapply(WL_combined$participant_external_id, is_ctc046_or_higher)
additional_ctc <- WL_combined$participant_external_id[ctc046_or_higher]

# Function to identify TMS040 and higher
is_tms040_or_higher <- function(id) {
    if (grepl("^TMS", id)) {
        # Extract the numeric part of the ID and check if it is 46 or higher
        num_part <- as.numeric(sub("^TMS", "", id))
        return(num_part >= 40)
    }
    FALSE
}

# Apply the function to all participants and get those that are CTC046 or higher
tms040_or_higher <- sapply(WL_combined$participant_external_id, is_tms040_or_higher)
additional_tms <- WL_combined$participant_external_id[tms040_or_higher]

remote_participants <- c(remote_participants, additional_ctc, additional_tms)

WL_combined$testing_location <- ifelse(WL_combined$participant_external_id %in% remote_participants, 
                                       "remote", "in-person")
WL_combined$testing_location <- ifelse(WL_combined$participant_external_id %in% c("TMS052", "TMS053"), 
                                       "remote", WL_combined$testing_location)


# Replace values in session_label column
WL_combined$session_label <- gsub("Baseline", "V1", WL_combined$session_label)

# Read and process demographic data
demoMDD <- demo_read_and_process(1)
demoCTRL <- demo_read_and_process(2)

# Generate new column names
column_mapping <- setNames(
    # Old names
  c(paste0(rep(c("bl_", "2wk_", "4wk_"), each = 1), "qids_tot"),
    paste0(rep(c("bl_", "2wk_", "4wk_"), each = 16), "qids_sr_", 1:16),
    paste0(rep(c("bl_", "2wk_", "4wk_"), each = 8), "gad7_", 1:8),
    paste0(rep(c("bl_", "2wk_", "4wk_"), each = 8), "gad7_tot")),
  # New names
  c(paste0("qids_total_", c("pre", "wk2", "wk4")),
    paste0("qids_", 1:16, "_", rep(c("pre", "wk2", "wk4"), each = 16)),
    paste0("gad7_", 1:8, "_", rep(c("pre", "wk2", "wk4"), each = 8)),
    paste0("gad7_total_", rep(c("pre", "wk2", "wk4"), each = 8)))
)

# Remove leading and trailing whitespaces from column names in demoCTRL
demoCTRL <- demoCTRL %>%
  rename_all(~trimws(.))

# Rename columns in demoCTRL
demoCTRL <- demoCTRL %>% rename(!!!column_mapping)

# Combine demographic datasets
demoMDD$dob_yr <- as.numeric(demoMDD$dob_yr)
demoCTRL$dob_yr <- as.numeric(demoCTRL$dob_yr)
demoMDD$handedness <- as.numeric(as.character(demoMDD$handedness))
demoCTRL$handedness <- as.numeric(as.character(demoCTRL$handedness))

demoALL <- bind_rows(demoMDD, demoCTRL)

# Merge relevant columns from demoALL into WL

WL_demo <- WL_combined %>% left_join(demoALL %>% select(participant_external_id, sex,
                                          age_screening,
                                          years_education,
                                          age_learned_english,
                                          first_language_english,
                                          starts_with("qids"),
                                          starts_with("gad7")))

# Read in psychiatry data
# Correctly specify the file path using the here function
file_path <- here("Final_Consolidated_Psychiatry_Data.csv")
# Read in the psychiatry data with empty cells coded as NAs
psych <- read_csv(file_path)

# Filter to match participant IDs in WL_demo
psych_filtered <- psych %>%
  filter(participant_external_id %in% WL_demo$participant_external_id)



# Merge the datasets based on "participant_external_id" and retain all columns
# Use full_join to include all participants from both datasets
WL_demo_psych <- full_join(psych_filtered %>% select(participant_external_id,
                                              starts_with("hamd17"),
                                              starts_with("qids"),
                                              starts_with("gad7"),
                                              starts_with("whodas")),
                           WL_demo,
                           by = "participant_external_id",
                           suffix = c("", ".psych"))



# Identify common columns (excluding "participant_external_id")
common_columns <- setdiff(intersect(names(WL_demo), names(psych_filtered)), "participant_external_id")

# Update NA values in WL_demo columns with values from psych where applicable
WL_demo_psych <- WL_demo_psych %>%
  mutate(across(all_of(common_columns), 
  ~ifelse(is.na(.), get(paste0(cur_column(), ".psych")), .)))

# Optionally, remove the extra columns if they are no longer needed
WL_demo_psych <- select(WL_demo_psych, -ends_with(".psych"))

# Subset to look at journaling and baseline scores
WL_jou <-subset_by_task(WL_demo_psych, "journaling")
WL_jou_bl<-subset_by_visit(WL_jou, "V1")
WL_feeling_bl <- WL_jou_bl %>%filter(stimulus_filename == "en_instruction_journal_feeling.mp3")

# Remove participants with missing journaling data
WL_feeling_bl <- WL_feeling_bl[WL_feeling_bl$participant_external_id != "TMS055" &
                                 WL_feeling_bl$participant_external_id != "TMS053" &
                                 WL_feeling_bl$participant_external_id != "MFB14", ]

eyetracking <- read_csv(here("rtms_ipast_clean_18feb2024.csv"))
eyetracking <- eyetracking %>%
  rename(participant_external_id = ID)


# Filter to match participant IDs in WL_demo_psych
eyetracking_filtered <- eyetracking %>%
  filter(participant_external_id %in% WL_feeling_bl$participant_external_id)

# Merge the datasets based on "participant_external_id" and retain all columns
# Use full_join to include all participants from both datasets
WL_eyetracking_jou_bl <- merge(eyetracking_filtered, WL_jou_bl, by = "participant_external_id")

# define speech variables
speech_variables <- c('fundamental_frequency_mean','fundamental_frequency_variance', 'intensity_mean_db','medium_pause_duration',
                    'speech_rate', 'sentiment_dominance', 'sentiment_valence',
                    'sentiment_arousal')

# Linear regression for each speech variable against RT for prosaccade


results <- list()
plots <- list()
stats_df <- data.frame()  # Dataframe to store statistics
p_values_lms <- c()  # Vector to collect p-values


for (s in speech_variables) {

    
    # Linear regression without interaction term
    formula <- as.formula(paste("mean_srt_via_cor_pros ~", s, "+ sex + age_screening + age_learned_english"))
    model <- lm(formula, data = WL_eyetracking_jou_bl)
    
    # Model summary
    model_summary <- summary(model)
    
    # Store the summary result
    results[[paste(s, "mean_srt_via_cor_pros", sep = "_")]] <- model_summary
    
    # Extract coefficients and p-values
    coefs <- summary(model)$coefficients
    p_values_lms<- c(p_values_lms, coefs[2, "Pr(>|t|)"])  # Collect p-values
    
    # Create a summary stats row
    stats_row <- data.frame(variable = s, 
                            estimate = coefs[2, "Estimate"], 
                            std_error = coefs[2, "Std. Error"], 
                            statistic = coefs[2, "t value"], 
                            p_value = coefs[2, "Pr(>|t|)"])
    stats_df <- rbind(stats_df, stats_row)  # Append to the stats dataframe

    # Create and save plots
    plot <- ggplot(WL_eyetracking_jou_bl, aes_string(x = s, y = "mean_srt_via_cor_pros")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE) + 
        labs(title = paste(s, "vs", "mean_srt_via_cor_pros"),
             x = s, y = "mean_srt_via_cor_pros") +
        theme_minimal() +
        theme(plot.title = element_text(size = 17))
plots <- c(plots, list(plot))
}

# Print and save results
print(plots)
print(results)

# Linear regression for each speech variable against RT for prosaccade MDD only 

results <- list()
plots <- list()
stats_df <- data.frame()  # Dataframe to store statistics
p_values_lms <- c()  # Vector to collect p-values

WL_eyetracking_jou_bl_mdd <- WL_eyetracking_jou_bl %>% filter(participant_group == "MDD")

for (s in speech_variables) {

    
    # Linear regression without interaction term
    formula <- as.formula(paste("mean_srt_via_cor_pros ~", s, "+ sex + age_screening + age_learned_english + testing_location"))
    model <- lm(formula, data = WL_eyetracking_jou_bl_mdd)
    
    # Model summary
    model_summary <- summary(model)
    
    # Store the summary result
    results[[paste(s, "mean_srt_via_cor_pros", sep = "_")]] <- model_summary
    
    # Extract coefficients and p-values
    coefs <- summary(model)$coefficients
    p_values_lms<- c(p_values_lms, coefs[2, "Pr(>|t|)"])  # Collect p-values
    
    # Create a summary stats row
    stats_row <- data.frame(variable = s, 
                            estimate = coefs[2, "Estimate"], 
                            std_error = coefs[2, "Std. Error"], 
                            statistic = coefs[2, "t value"], 
                            p_value = coefs[2, "Pr(>|t|)"])
    stats_df <- rbind(stats_df, stats_row)  # Append to the stats dataframe

    # Create and save plots
    plot <- ggplot(WL_eyetracking_jou_bl_mdd, aes_string(x = s, y = "mean_srt_via_cor_pros")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE) + 
        labs(title = paste(s, "vs", "mean_srt_via_cor_pros"),
             x = s, y = "mean_srt_via_cor_pros") +
        theme_minimal() +
        theme(plot.title = element_text(size = 17))
plots <- c(plots, list(plot))

}

# Print and save results
print(results)
print(plots)


# Linear regression for each speech variable against RT for prosaccade with interaction 

results <- list()
plots <- list()
stats_df <- data.frame()  # Dataframe to store statistics
p_values_lms <- c()  # Vector to collect p-values


for (s in speech_variables) {

    
    # Linear regression 
    formula <- as.formula(paste("mean_srt_via_cor_pros ~", s, "* participant_group + sex + age_screening + age_learned_english + testing_location"))
    model <- lm(formula, data = WL_eyetracking_jou_bl)
    
    # Model summary
    model_summary <- summary(model)
    
    # Store the summary result
    results[[paste(s, "mean_srt_via_cor_pros", sep = "_")]] <- model_summary
    
    # Extract coefficients and p-values
    coefs <- summary(model)$coefficients
    p_values_lms<- c(p_values_lms, coefs[2, "Pr(>|t|)"])  # Collect p-values
    
    # Create a summary stats row
    stats_row <- data.frame(variable = s, 
                            estimate = coefs[2, "Estimate"], 
                            std_error = coefs[2, "Std. Error"], 
                            statistic = coefs[2, "t value"], 
                            p_value = coefs[2, "Pr(>|t|)"])
    stats_df <- rbind(stats_df, stats_row)  # Append to the stats dataframe

    # Create and save plots
plot <- ggplot(WL_eyetracking_jou_bl, aes_string(x = s, y = "mean_srt_via_cor_pros", color = "participant_group")) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", aes_string(group = "participant_group"), se = FALSE) + 
    labs(title = paste(s, "vs", "mean_srt_via_cor_pros with interaction term"),
         x = s, y = "mean_srt_via_cor_pros") +
    theme_minimal() +
    theme(plot.title = element_text(size = 17))
plots <- c(plots, list(plot))
}
# Print and save results
print(plots)
print(results)



# Linear regression for each speech variable against RT for antisacca 

results <- list()
plots <- list()
stats_df <- data.frame()  # Dataframe to store statistics
p_values_lms <- c()  # Vector to collect p-values


for (s in speech_variables) {

    
    # Linear regression 
    formula <- as.formula(paste("mean_srt_via_cor_anti ~", s, "+ sex + age_screening + age_learned_english + testing_location"))
    model <- lm(formula, data = WL_eyetracking_jou_bl_mdd)
    
    # Model summary
    model_summary <- summary(model)
    
    # Store the summary result
    results[[paste(s, "mean_srt_via_cor_anti", sep = "_")]] <- model_summary
    
    # Extract coefficients and p-values
    coefs <- summary(model)$coefficients
    p_values_lms<- c(p_values_lms, coefs[2, "Pr(>|t|)"])  # Collect p-values
    
    # Create a summary stats row
    stats_row <- data.frame(variable = s, 
                            estimate = coefs[2, "Estimate"], 
                            std_error = coefs[2, "Std. Error"], 
                            statistic = coefs[2, "t value"], 
                            p_value = coefs[2, "Pr(>|t|)"])
    stats_df <- rbind(stats_df, stats_row)  # Append to the stats dataframe

    # Create and save plots
    plot <- ggplot(WL_eyetracking_jou_bl_mdd, aes_string(x = s, y = "mean_srt_via_cor_anti")) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) + 
      labs(title = paste(s, "vs", "mean_srt_via_cor_anti"),
           x = s, y = "mean_srt_via_cor_anti") +
      theme_minimal() +
      theme(plot.title = element_text(size = 17))
plots <- c(plots, list(plot))
}
# Print and save results
print(plots)
print(results)

# Linear regression for each speech variable against RT for prosaccadde 

results <- list()
plots <- list()
stats_df <- data.frame()  # Dataframe to store statistics
p_values_lms <- c()  # Vector to collect p-values


for (s in speech_variables) {

    
    # Linear regression without interaction term
    formula <- as.formula(paste("mean_srt_via_cor_pros ~", s, "* participant_group + sex + age_screening+age_learned_english+years_education"))
    model <- lm(formula, data = WL_eyetracking_jou_bl)
    
    # Model summary
    model_summary <- summary(model)
    
    # Store the summary result
    results[[paste(s, "mean_srt_via_cor_pros", sep = "_")]] <- model_summary
    
    # Extract coefficients and p-values
    coefs <- summary(model)$coefficients
    p_values_lms<- c(p_values_lms, coefs[2, "Pr(>|t|)"])  # Collect p-values
    
    # Create a summary stats row
    stats_row <- data.frame(variable = s, 
                            estimate = coefs[2, "Estimate"], 
                            std_error = coefs[2, "Std. Error"], 
                            statistic = coefs[2, "t value"], 
                            p_value = coefs[2, "Pr(>|t|)"])
    stats_df <- rbind(stats_df, stats_row)  # Append to the stats dataframe

    # Create and save plots
    plot <- ggplot(WL_eyetracking_jou_bl, aes_string(x = s, y = "mean_srt_via_cor_pros")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE) + 
        labs(title = paste(s, "vs", "mean_srt_via_cor_pros"),
             x = s, y = "mean_srt_via_cor_pros") +
        theme_minimal() +
        theme(plot.title = element_text(size = 17))
plots <- c(plots, list(plot))
}

# Print and save results
print(plots)
print(results)


# Linear regression for each speech variable against ratio errors for pro and anti

results <- list()
plots <- list()
stats_df <- data.frame()  # Dataframe to store statistics
p_values_lms <- c()  # Vector to collect p-values


for (s in speech_variables) {

    
    # Linear regression without interaction term
    formula <- as.formula(paste("ratio_via_err_anti ~", s, "+ sex + age_screening + age_learned_english + testing_location"))
    model <- lm(formula, data = WL_eyetracking_jou_bl_mdd)
    
    # Model summary
    model_summary <- summary(model)
    
    # Store the summary result
    results[[paste(s, "ratio_via_err_anti", sep = "_")]] <- model_summary
    
    # Extract coefficients and p-values
    coefs <- summary(model)$coefficients
    p_values_lms<- c(p_values_lms, coefs[2, "Pr(>|t|)"])  # Collect p-values
    
    # Create a summary stats row
    stats_row <- data.frame(variable = s, 
                            estimate = coefs[2, "Estimate"], 
                            std_error = coefs[2, "Std. Error"], 
                            statistic = coefs[2, "t value"], 
                            p_value = coefs[2, "Pr(>|t|)"])
    stats_df <- rbind(stats_df, stats_row)  # Append to the stats dataframe

    # Create and save plots
    plot <- ggplot(WL_eyetracking_jou_bl_mdd, aes_string(x = s, y = "ratio_via_err_anti")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE) + 
        labs(title = paste(s, "vs", "ratio_via_err_anti"),
             x = s, y = "ratio_via_err_anti") +
        theme_minimal() +
        theme(plot.title = element_text(size = 17))
plots <- c(plots, list(plot))
}

# Print and save results
print(plots)
print(results)

# Linear regression for each speech variable against RT for prosaccade

results <- list()
plots <- list()
stats_df <- data.frame()  # Dataframe to store statistics
p_values_lms <- c()  # Vector to collect p-values


for (s in speech_variables) {

    
    # Linear regression without interaction term
    formula <- as.formula(paste("mean_srt_via_cor_pros ~", s, "+ sex + age_screening + age_learned_english"))
    model <- lm(formula, data = WL_eyetracking_jou_bl)
    
    # Model summary
    model_summary <- summary(model)
    
    # Store the summary result
    results[[paste(s, "mean_srt_via_cor_pros", sep = "_")]] <- model_summary
    
    # Extract coefficients and p-values
    coefs <- summary(model)$coefficients
    p_values_lms<- c(p_values_lms, coefs[2, "Pr(>|t|)"])  # Collect p-values
    
    # Create a summary stats row
    stats_row <- data.frame(variable = s, 
                            estimate = coefs[2, "Estimate"], 
                            std_error = coefs[2, "Std. Error"], 
                            statistic = coefs[2, "t value"], 
                            p_value = coefs[2, "Pr(>|t|)"])
    stats_df <- rbind(stats_df, stats_row)  # Append to the stats dataframe

    # Create and save plots
    plot <- ggplot(WL_eyetracking_jou_bl, aes_string(x = s, y = "mean_srt_via_cor_pros")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE) + 
        labs(title = paste(s, "vs", "mean_srt_via_cor_pros"),
             x = s, y = "mean_srt_via_cor_pros") +
        theme_minimal() +
        theme(plot.title = element_text(size = 17))
plots <- c(plots, list(plot))
}

# Print and save results
print(plots)
print(results)


# Linear regression for each speech variable against error ratio with interaction

results <- list()
plots <- list()
stats_df <- data.frame()  # Dataframe to store statistics
p_values_lms <- c()  # Vector to collect p-values


for (s in speech_variables) {

    
    # Linear regression 
    formula <- as.formula(paste("ratio_via_err_anti ~", s, "* participant_group + sex + age_screening + age_learned_english + testing_location"))
    model <- lm(formula, data = WL_eyetracking_jou_bl)
    
    # Model summary
    model_summary <- summary(model)
    
    # Store the summary result
    results[[paste(s, "ratio_via_err_anti", sep = "_")]] <- model_summary
    
    # Extract coefficients and p-values
    coefs <- summary(model)$coefficients
    p_values_lms<- c(p_values_lms, coefs[2, "Pr(>|t|)"])  # Collect p-values
    
    # Create a summary stats row
    stats_row <- data.frame(variable = s, 
                            estimate = coefs[2, "Estimate"], 
                            std_error = coefs[2, "Std. Error"], 
                            statistic = coefs[2, "t value"], 
                            p_value = coefs[2, "Pr(>|t|)"])
    stats_df <- rbind(stats_df, stats_row)  # Append to the stats dataframe

    # Create and save plots
plot <- ggplot(WL_eyetracking_jou_bl, aes_string(x = s, y = "ratio_via_err_anti", color = "participant_group")) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", aes_string(group = "participant_group"), se = FALSE) + 
    labs(title = paste(s, "vs", "ratio_via_err_anti with interaction term"),
         x = s, y = "ratio_via_err_anti") +
    theme_minimal() +
    theme(plot.title = element_text(size = 17))
plots <- c(plots, list(plot))
}
# Print and save results
print(plots)
print(results)


# Calculate mean and SD of age for each participant group
age_stats <- WL_eyetracking_jou_bl %>%
  group_by(participant_group) %>%
  summarise(
    mean_age = mean(age_screening, na.rm = TRUE),
    sd_age = sd(age_screening, na.rm = TRUE)
  )

# Calculate percentage of M and F in each participant group
sex_distribution <- WL_eyetracking_jou_bl %>%
  group_by(participant_group, sex) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    percentage = count / sum(count) * 100
  )

# Adjusting previous code for unique IDs and NA participants
# Count unique IDs for each combination of sex and participant group
unique_counts <- WL_eyetracking_jou_bl %>%
  group_by(participant_group, sex) %>%
  summarise(unique_ids = n_distinct(participant_external_id), .groups = 'drop')

# Filter rows where both sex and participant_group are NA
na_participants <- WL_eyetracking_jou_bl %>%
  filter(is.na(sex) | is.na(participant_group)) %>%
  distinct(participant_external_id)


# Adjusted code to classify and count IDs by prefix
unique_counts <- WL_eyetracking_jou_bl %>%
  mutate(prefix = case_when(
    substr(participant_external_id, 1, 3) == "TMS" ~ "TMS",
    substr(participant_external_id, 1, 3) == "MDD" ~ "MDD",
    substr(participant_external_id, 1, 3) == "MFB" ~ "MFB",
    TRUE ~ "Other"  # Handles cases that do not match the above prefixes
  )) %>%
  group_by(participant_group, sex, prefix) %>%
  summarise(unique_ids = n_distinct(participant_external_id), .groups = 'drop')


# Print the results
print(age_stats)
print(sex_distribution)
print(unique_counts)
print(na_participants)

write.csv(WL_eyetracking_jou_bl, "WL_eyetracking_jou_bl.csv")