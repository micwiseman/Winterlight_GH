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
psych <- read_csv(here("Final_Consolidated_Psychiatry_Data.csv"))

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

## Score WHODAS
# Define the items for each domain
domains <- list(
  whodas_undcom = list(items = c("whodas_undcom_1_pre", "whodas_undcom_2_pre", "whodas_undcom_3_pre", 
                                 "whodas_undcom_4_pre", "whodas_undcom_5_pre", "whodas_undcom_6_pre"), divisor = 24),

  whodas_getaround = list(items =c("whodas_getaround_1_pre", "whodas_getaround_2_pre", "whodas_getaround_3_pre", 
                                   "whodas_getaround_4_pre", "whodas_getaround_5_pre"), divisor = 20),

  whodas_selfcare = list(items = c("whodas_selfcare_1_pre", "whodas_selfcare_2_pre", "whodas_selfcare_3_pre",
                                   "whodas_selfcare_4_pre"), divisor = 16),

  whodas_getalong = list(items = c("whodas_getalong_1_pre", "whodas_getalong_2_pre", "whodas_getalong_3_pre",
                                   'whodas_getalong_4_pre', 'whodas_getalong_5_pre'), divisor = 20),

  whodas_lifeact_house = list(items = c("whodas_house_1_pre", "whodas_house_2_pre", "whodas_house_3_pre",
                                        'whodas_house_4_pre'), divisor = 16),

  whodas_lifeact_schwork = list(items=c('whodas_schwork_1_pre', 'whodas_schwork_2_pre', 'whodas_schwork_3_pre', 'whodas_schwork_4_pre'), divisor = 16 ),

  whodas_soc = list(items = c("whodas_soc_1_pre", "whodas_soc_2_pre", "whodas_soc_3_pre", 'whodas_soc_4_pre',
                              'whodas_soc_5_pre', 'whodas_soc_7_pre', 'whodas_soc_8_pre'), divisor = 32)
)

# Function to calculate domain scores with missing data handling and imputation - according to WHO guidelines for handling missing data
calculate_domain_score_complex <- function(df, items, divisor) {
  apply(df[, items], 1, function(row) {
    missing_count <- sum(is.na(row))
    if (missing_count > 2) {
      return(NA)
    } else {
      if (missing_count > 0 && missing_count <= 2) {
        impute_mean <- mean(row, na.rm = TRUE)
        row[is.na(row)] <- impute_mean
      }
      return(sum(row, na.rm = TRUE) / divisor)
    }
  })
}

calculate_domain_score_simple <- function(df, items) {
  apply(df[, items], 1, function(row) {
    missing_count <- sum(is.na(row))
    if (missing_count > 2) {
      return(NA)
    } else {
      if (missing_count > 0 && missing_count <= 2) {
        impute_mean <- mean(row, na.rm = TRUE)
        row[is.na(row)] <- impute_mean
      }
      return(sum(row, na.rm = TRUE))
    }
  })
}

WL_demo_psych <- WL_demo_psych %>%
  mutate(
    whodas_undcom_tot_pre_complex = calculate_domain_score_complex(., domains$whodas_undcom$items, domains$whodas_undcom$divisor),
    whodas_undcom_tot_pre_simple = calculate_domain_score_simple(., domains$whodas_undcom$items),
    whodas_getaround_tot_pre_complex = calculate_domain_score_complex(., domains$whodas_getaround$items, domains$whodas_getaround$divisor),
    whodas_getaround_tot_pre_simple = calculate_domain_score_simple(., domains$whodas_getaround$items),
    whodas_selfcare_tot_pre_complex = calculate_domain_score_complex(., domains$whodas_selfcare$items, domains$whodas_selfcare$divisor),
    whodas_selfcare_tot_pre_simple = calculate_domain_score_simple(., domains$whodas_selfcare$items),
    whodas_getalong_tot_pre_complex = calculate_domain_score_complex(., domains$whodas_getalong$items, domains$whodas_getalong$divisor),
    whodas_getalong_tot_pre_simple = calculate_domain_score_simple(., domains$whodas_getalong$items),
    whodas_lifeact_tot_pre_complex = ifelse(whodas_schwork_screen_pre == 1,
                                    rowSums(select(., all_of(c(domains$whodas_lifeact_house$items, domains$whodas_lifeact_schwork$items))), na.rm = TRUE) / 32,
                                    rowSums(select(., all_of(domains$whodas_lifeact_house$items)), na.rm = TRUE) / 6),
    whodas_lifeact_tot_pre_simple = ifelse(whodas_schwork_screen_pre == 1,
                                    rowSums(select(., all_of(c(domains$whodas_lifeact_house$items, domains$whodas_lifeact_schwork$items))), na.rm = TRUE),
                                    rowSums(select(., all_of(domains$whodas_lifeact_house$items)), na.rm = TRUE)),
    whodas_soc_tot_pre_complex = calculate_domain_score_complex(., domains$whodas_soc$items, domains$whodas_soc$divisor),
    whodas_soc_tot_pre_simple = calculate_domain_score_simple(., domains$whodas_soc$items)
)

# calculate total WHODAS scores
WL_demo_psych <- WL_demo_psych %>%
  mutate(
    whodas_complex_total_pre = rowSums(select(., whodas_undcom_tot_pre_complex, whodas_getaround_tot_pre_complex, whodas_selfcare_tot_pre_simple, whodas_getalong_tot_pre_complex, whodas_lifeact_tot_pre_complex, whodas_soc_tot_pre_complex), na.rm = TRUE) / 6
  )
WL_demo_psych <- WL_demo_psych %>%
  mutate(
    whodas_simple_total_pre = rowSums(select(., whodas_undcom_tot_pre_simple, whodas_getaround_tot_pre_simple, whodas_selfcare_tot_pre_simple, whodas_getalong_tot_pre_simple, whodas_lifeact_tot_pre_simple, whodas_soc_tot_pre_simple), na.rm = TRUE)
  )

WL_demo_psych$whodas_complex_total_pre <- ifelse(WL_demo_psych$whodas_complex_total_pre == 0, NA, WL_demo_psych$whodas_complex_total_pre)
WL_demo_psych$whodas_simple_total_pre <- ifelse(WL_demo_psych$whodas_simple_total_pre == 0, NA, WL_demo_psych$whodas_simple_total_pre)


# Subset to look at journaling and baseline scores
WL_full_jou <-subset_by_task(WL_demo_psych, "journaling")
WL_full_jou_bl<-subset_by_visit(WL_full_jou, "V1")

# Remove participants with missing journaling data
WL_full_jou_bl <- WL_full_jou_bl[WL_full_jou_bl$participant_external_id != "TMS055" &
                                 WL_full_jou_bl$participant_external_id != "TMS053" &
                                 WL_full_jou_bl$participant_external_id != "MFB14", ]

# define speech variables
speech_variables <- c('fundamental_frequency_mean','fundamental_frequency_variance', 'intensity_mean_db','medium_pause_duration',
                    'speech_rate', 'sentiment_dominance', 'sentiment_valence',
                    'sentiment_arousal')


WL_full_jou_bl$sex <- as.factor(WL_full_jou_bl$sex)
WL_full_jou_bl$testing_location <- as.factor(WL_full_jou_bl$testing_location)
WL_jou_bl_MDD <- WL_full_jou_bl %>% filter(participant_group == "MDD")
WL_jou_bl_MDD_feeling <- WL_jou_bl_MDD %>% filter(stimulus_filename == "en_instruction_journal_feeling.mp3")

### visualize WHODAS simple 
 # Select WHODAS columns
  whodas_pre_columns_simple <- grep("^whodas_.*_tot_pre_simple$", names(WL_jou_bl_MDD_feeling), value = TRUE)
  
  # Gather data for plotting
  whodas_long_simple <- WL_jou_bl_MDD_feeling %>%
    select(participant_external_id, all_of(whodas_pre_columns)) %>%
    pivot_longer(cols = all_of(whodas_pre_columns), names_to = "whodas_domain", values_to = "score")
  
  # Count missing values
  missing_counts_simple <- whodas_long_simple %>%
    group_by(whodas_domain) %>%
    summarize(missing_values = sum(is.na(score)), .groups = 'drop')
  
  print("Missing values count per WHODAS complex domain:")
  print(missing_counts_simple)
  
  # Plot the WHODAS scores
  p3 <- ggplot(whodas_long_simple, aes(x = whodas_domain, y = score, fill = whodas_domain)) +
    geom_violin(trim = FALSE) +
    geom_jitter(width = 0.2, size = 0.5) +
    labs(title = "Distribution of WHODAS Complex Scores by Domain",
         x = "WHODAS Domain",
         y = "Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p3)

  # Plot the distribution of the total WHODAS pre scores - Histogram
  p4 <- ggplot(WL_jou_bl_MDD_feeling, aes(x = whodas_simple_total_pre)) +
    geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Distribution of WHODAS Simple Scores",
         x = "WHODAS Simple Score",
         y = "Frequency") +
    theme_minimal()
  
  print(p4)


### visualize WHODAS complex 
 # Select WHODAS columns
  whodas_pre_columns_complex <- grep("^whodas_.*_tot_pre_complex$", names(WL_jou_bl_MDD_feeling), value = TRUE)
  
  # Gather data for plotting
  whodas_long_complex <- WL_jou_bl_MDD_feeling %>%
    select(participant_external_id, all_of(whodas_pre_columns_complex)) %>%
    pivot_longer(cols = all_of(whodas_pre_columns_complex), names_to = "whodas_domain", values_to = "score")
  
  # Count missing values
  missing_counts_complex <- whodas_long_complex %>%
    group_by(whodas_domain) %>%
    summarize(missing_values = sum(is.na(score)), .groups = 'drop')
  
  print("Missing values count per WHODAS complex domain:")
  print(missing_counts_complex)
  
  # Plot the WHODAS scores
  p <- ggplot(whodas_long_complex, aes(x = whodas_domain, y = score, fill = whodas_domain)) +
    geom_violin(trim = FALSE) +
    geom_jitter(width = 0.2, size = 0.5) +
    labs(title = "Distribution of WHODAS Complex Scores by Domain",
         x = "WHODAS Domain",
         y = "Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)

  # Plot the distribution of the total WHODAS pre scores - Histogram
  p2 <- ggplot(WL_jou_bl_MDD_feeling, aes(x = whodas_complex_total_pre)) +
    geom_histogram(binwidth = 0.04, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Distribution of WHODAS Complex Scores",
         x = "WHODAS Complex Score",
         y = "Frequency") +
    theme_minimal()
  
  print(p2)

# Linear regression for each speech variable against WHODAS using complex scores 
library(ggplot2)
library(lmtest)

results <- list()
plots <- list()
stats_df <- data.frame()  # Dataframe to store statistics
p_values_lms <- c()  # Vector to collect p-values


for (s in speech_variables) {
    # Filter out observations where sex is NA
    filtered_data <- WL_jou_bl_MDD_feeling %>%
        filter(!is.na(sex), !is.na(!!as.symbol(s)), !is.na(hamd17_total_pre))
    
    # Linear regression without interaction term
    formula <- as.formula(paste("whodas_complex_total_pre ~", s, "+ sex + age_screening + age_learned_english"))
    model <- lm(formula, data = filtered_data)
    
    # Model summary
    model_summary <- summary(model)
    
    # Store the summary result
    results[[paste(s, "whodas_complex_total_pre", sep = "_")]] <- model_summary
    
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
    plot <- ggplot(filtered_data, aes_string(x = s, y = "whodas_complex_total_pre")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE) + 
        labs(title = paste(s, "vs", "whodas_complex_total_pre"),
             x = s, y = "whodas_complex_total_pre") +
        theme_minimal() +
        theme(plot.title = element_text(size = 17))
  print(plot)
}

# Print and save results
print(plots)
print(results)
write.csv(stats_df, "stats_df.csv", row.names = FALSE)


# Linear regression for each speech variable against WHODAS using complex scores 
library(ggplot2)
library(lmtest)

WL_full_jou_bl$sex <- as.factor(WL_full_jou_bl$sex)
WL_full_jou_bl$testing_location <- as.factor(WL_full_jou_bl$testing_location)
WL_jou_bl_MDD <- WL_full_jou_bl %>% filter(participant_group == "MDD")
WL_jou_bl_MDD_feeling <- WL_jou_bl_MDD %>% filter(stimulus_filename == "en_instruction_journal_feeling.mp3")

### visualize WHODAS
 # Select WHODAS columns
  whodas_pre_columns <- grep("^whodas_.*_tot_pre$", names(WL_jou_bl_MDD_feeling), value = TRUE)
  
  # Gather data for plotting
  whodas_long_complex <- WL_jou_bl_MDD_feeling %>%
    select(participant_external_id, all_of(whodas_pre_columns)) %>%
    pivot_longer(cols = all_of(whodas_pre_columns), names_to = "whodas_domain", values_to = "score")
  
  # Count missing values
  missing_counts_complex <- whodas_long_complex %>%
    group_by(whodas_domain) %>%
    summarize(missing_values = sum(is.na(score)), .groups = 'drop')
  
  print("Missing values count per WHODAS domain:")
  print(missing_counts_complex)
  
  # Plot the WHODAS scores
  p <- ggplot(whodas_long_complex, aes(x = whodas_domain, y = score, fill = whodas_domain)) +
    geom_violin(trim = FALSE) +
    geom_jitter(width = 0.2, size = 0.5) +
    labs(title = "Distribution of WHODAS Scores by Domain",
         x = "WHODAS Domain",
         y = "Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)

  # Plot the distribution of the total WHODAS pre scores - Histogram
  p2 <- ggplot(WL_jou_bl_MDD_feeling, aes(x = whodas_complex_total_pre)) +
    geom_histogram(binwidth = 0.04, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Distribution of WHODAS Complex Scores",
         x = "WHODAS Complex Score",
         y = "Frequency") +
    theme_minimal()
  
  print(p2)


results <- list()
plots <- list()
stats_df <- data.frame()  # Dataframe to store statistics
p_values_lms <- c()  # Vector to collect p-values


for (s in speech_variables) {
    # Filter out observations where sex is NA
    filtered_data <- WL_jou_bl_MDD_feeling %>%
        filter(!is.na(sex), !is.na(!!as.symbol(s)), !is.na(hamd17_total_pre))
    
    # Linear regression without interaction term
    formula <- as.formula(paste("whodas_complex_total_pre ~", s, "+ sex + age_screening + age_learned_english"))
    model <- lm(formula, data = filtered_data)
    
    # Model summary
    model_summary <- summary(model)
    
    # Store the summary result
    results[[paste(s, "whodas_complex_total_pre", sep = "_")]] <- model_summary
    
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
    plot <- ggplot(filtered_data, aes_string(x = s, y = "whodas_complex_total_pre")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE) + 
        labs(title = paste(s, "vs", "whodas_complex_total_pre"),
             x = s, y = "whodas_complex_total_pre") +
        theme_minimal() +
        theme(plot.title = element_text(size = 17))
  print(plot)
}

# Print and save results
print(plots)
print(results)
write.csv(stats_df, "stats_df.csv", row.names = FALSE)


# Linear regression for each speech variable against WHODAS using complex scores 
library(ggplot2)
library(lmtest)

results <- list()
plots <- list()
stats_df <- data.frame()  # Dataframe to store statistics
p_values_lms <- c()  # Vector to collect p-values


for (s in speech_variables) {
    # Filter out observations where sex is NA
    filtered_data <- WL_jou_bl_MDD_feeling %>%
        filter(!is.na(sex), !is.na(!!as.symbol(s)), !is.na(hamd17_total_pre))
    
    # Linear regression without interaction term
    formula <- as.formula(paste("whodas_complex_total_pre ~", s, "+ sex + age_screening"))
    model <- lm(formula, data = filtered_data)
    
    # Model summary
    model_summary <- summary(model)
    
    # Store the summary result
    results[[paste(s, "whodas_complex_total_pre", sep = "_")]] <- model_summary
    
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
    plot <- ggplot(filtered_data, aes_string(x = s, y = "whodas_complex_total_pre")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE) + 
        labs(title = paste(s, "vs", "whodas_complex_total_pre"),
             x = s, y = "whodas_complex_total_pre") +
        theme_minimal() +
        theme(plot.title = element_text(size = 17))
  print(plot)
}

# Print and save results
print(plots)
print(results)
write.csv(stats_df, "stats_df.csv", row.names = FALSE)


# Linear regression for each speech variable against WHODAS using simple scores 

results <- list()
plots <- list()
stats_df <- data.frame()  # Dataframe to store statistics
p_values_lms <- c()  # Vector to collect p-values


for (s in speech_variables) {
    # Filter out observations where sex is NA
    filtered_data <- WL_jou_bl_MDD_feeling %>%
        filter(!is.na(sex), !is.na(!!as.symbol(s)))
    
    # Linear regression without interaction term
    formula <- as.formula(paste("whodas_simple_total_pre ~", s, "+ sex + age_screening"))
    model <- lm(formula, data = filtered_data)
    
    # Model summary
    model_summary <- summary(model)
    
    # Store the summary result
    results[[paste(s, "whodas_simple_total_pre", sep = "_")]] <- model_summary
    
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
    plot <- ggplot(filtered_data, aes_string(x = s, y = "whodas_simple_total_pre")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE) + 
        labs(title = paste(s, "vs", "whodas_simple_total_pre"),
             x = s, y = "whodas_simple_total_pre") +
        theme_minimal() +
        theme(plot.title = element_text(size = 17))
  print(plot)
}

# Print and save results
print(plots)
print(results)
write.csv(stats_df, "stats_df.csv", row.names = FALSE)


